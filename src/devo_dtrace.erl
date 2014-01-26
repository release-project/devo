-module(devo_dtrace).

-export([start_trace/2, stop_trace/0, loop/3]).

start_trace(Opts, Receiver) ->
    DTFramework = erlang:system_info(dynamic_trace),
    Script  = gen_script(DTFramework, Opts),
    ScriptF = save_script(Script),
    P = spawn(?MODULE, loop, [DTFramework, ScriptF, Receiver]),
    register('devo_dtracer', P),
    P ! start.

loop(DTFramework, ScriptF, Receiver) ->
    receive
        start ->
            Cmd = fmt_command(DTFramework, ScriptF),
            P = open_port({spawn, Cmd}, [{line, 1024}]),
            put(port, P),
            loop(DTFramework, ScriptF, Receiver);
        {S, stop} ->
            P = get(port),
            true = port_close(P),
            %ok = file:delete(ScriptF),
            S ! stopped;
        {_, {data, {eol, Data}}} ->
            {ok, Tokens, _} = erl_scan:string(Data),
            {ok, M} = erl_parse:parse_term(Tokens),
            case M of
                { in_queue, Pid, Rq } ->
                    case erlang:get({run_queue, Pid}) of
                        undefined -> 
                            erlang:put({run_queue, Pid}, Rq);
                        Rq -> 
                            ok;
                        OldRq ->
                            erlang:put({run_queue, Pid}, Rq),
                            Receiver ! { trace_rq, OldRq, Rq }
                    end;
                { trace_inter_node, _, ToNode, _ } ->
                    case ToNode == get_node_name(Receiver) of
                        true -> ok;
                        false -> Receiver ! M
                    end;
                _ -> 
                    Receiver ! M
            end,
            loop(DTFramework, ScriptF, Receiver)
    end.

stop_trace() ->
    devo_dtracer ! {self(), stop},
    receive
        stopped -> ok
    end.

gen_script(DTFramework, Opts) ->
    fl([gen_script_chunk(DTFramework, Opt) || Opt <- Opts]).

gen_script_chunk(systemtap, run_queue_size) ->
    RqNo = erlang:system_info(schedulers),
    fl(["global run_queue_sizes[", integer_to_list(RqNo), "]\n",
        "global t0\n\n",
        "probe begin {\n",
        "\tt0 = gettimeofday_ms()\n",
        "}\n\n",
        "probe process(\"", get_vm_executable(), "\").",
        "mark(\"run_queue-process_enqueue\") {\n", 
        "\trun_queue_sizes[$arg1] = $arg3\n",
        "}\n\n",
        "probe process(\"", get_vm_executable(), "\").",
        "mark(\"run_queue-process_dequeue\") {\n", 
        "\trun_queue_sizes[$arg1] = $arg3\n",
        "}\n\n",
        "probe process(\"", get_vm_executable(), "\").",
        "mark(\"run_queue-port_enqueue\") {\n", 
        "\trun_queue_sizes[$arg1] = $arg3\n",
        "}\n\n",
        "probe process(\"", get_vm_executable(), "\").",
        "mark(\"run_queue-port_dequeue\") {\n",
        "\trun_queue_sizes[$arg1] = $arg3\n",
        "}\n\n",
        "probe timer.ms(200) {\n",
        "\tt1 = gettimeofday_ms()\n",
        "\tdiff = t1 - t0\n",
        "\tprintf(\"{ run_queues_info, %d.%d, {\", diff/1000, diff%1000)\n",
        "\tfor(i = 0; i < ", integer_to_list(RqNo), "; i ++) {\n",
        "\t\tif(!run_queue_sizes[i])\n",
        "\t\t\tprintf(\"0\")\n",
        "\t\telse\n",
        "\t\t\tprintf(\"%d\", run_queue_sizes[i])\n",
        "\t\tif(i < ", integer_to_list(RqNo), " - 1)\n",
        "\t\t\tprintf(\", \")\n"
        "\t}\n",
        "\tprintf(\"} }.\\n\")\n" 
        "}\n\n"]);
gen_script_chunk(systemtap, process_migration) ->
% Attempt #1: This probe was never fired.
%    fl(["probe process(\"", get_vm_executable(), "\").",
%        "mark(\"process-migrate\") {\n",
%        "\tprintf(\"{ trace_rq, %d, %d }.\\n\", $arg2, $arg4)\n",
%        "}\n\n"]).
% Attempt #2
    fl(["probe process(\"", get_vm_executable(), "\").",
        "mark(\"process-scheduled\") {\n",
        "\tprintf(\"{ in_queue, \\\"%s\\\", %d }.\\n\", ",
        "user_string($arg1), $arg3)\n",
        "}\n\n"]);
gen_script_chunk(systemtap, {message_queue_size, P}) ->
    Pid = case is_atom(P) of
              true -> whereis(P);
              false -> P
          end,
    case Pid of 
        undefined -> "";
        _ -> 
            S = lists:filter(fun(C) -> not lists:member(C, [10,32]) end,
                             fl(io_lib:format("~p", [term_to_binary(Pid)]))),
            fl(["global message_queue_size = 0\n",
                "global t0\n\n",
                "probe begin {\n",
                "\tt0 = gettimeofday_ms()\n",
                "}\n\n",
                "probe process(\"", get_vm_executable(), "\").",
                "mark(\"message-queued\") {\n",
                "\tif(user_string($arg1) == \"", S, "\")\n",
                "\tmessage_queue_size = $arg3\n",
                "}\n\n",
                "probe process(\"", get_vm_executable(), "\").",
                "mark(\"message-receive\") {\n",
                "\tif(user_string($arg1) == \"", S, "\")\n",
                "\tmessage_queue_size = $arg3\n",
                "}\n\n",
                "probe timer.ms(200) {\n",
                "\tt1 = gettimeofday_ms()\n",
                "\tdiff = t1 - t0\n",
                "\tprintf(\"{ message_queue_len_info, %d.%d, %d }.\\n\", ",
                ", diff/1000, diff%1000, message_queue_size)\n",
                "}\n\n"])
    end;
gen_script_chunk(systemtap, inter_node_communication) ->
    N = node(),
    fl(["probe process(\"", get_vm_executable(), "\").",
        "mark(\"message-send-remote\") {\n",
        "\tprintf(\"{ trace_inter_node, '", atom_to_list(N), "', %s, %d }.\\n\", ",
        "user_string($arg2), $arg4 * 4)\n",
        "}\n\n"]);
gen_script_chunk(systemtap, s_group) ->
    N = node(),
    fl(["probe process(\"", get_vm_executable(), "\").mark(\"user_trace-n0\") {\n",
        "\tprintf(\"{ s_group, '", atom_to_list(N), "', new_s_group, [{%s, []}] }.\\n\", ",
        "user_string($arg7))\n",
        "}\n\n",
        "probe process(\"", get_vm_executable(), "\").mark(\"user_trace-n1\") {\n",
        "\tprintf(\"{ s_group, '", atom_to_list(N), "', delete_s_group, [%s] }.\\n\", ",
        "user_string($arg7))\n",
        "}\n\n",
        "probe process(\"", get_vm_executable(), "\").mark(\"user_trace-n2\") {\n",
        "\tprintf(\"{ s_group, '", atom_to_list(N), "', add_nodes, [%s, [%s]]}.\\n\", ",
        "user_string($arg7), user_string($arg8))\n",
        "}\n\n",
        "probe process(\"", get_vm_executable(), "\").mark(\"user_trace-n2\") {\n",
        "\tprintf(\"{ s_group, '", atom_to_list(N), "', remove_nodes, [%s, [%s]]}.\\n\", ",
        "user_string($arg7), user_string($arg8))\n",
        "}\n\n"]).
                         
save_script(S) ->
    SFN = filename:join(["/tmp/",
                         test_server:temp_name("devo-trace-script-")]),
    {ok, SF} = file:open(SFN, [write, raw]),
    ok = file:write(SF, S),
    file:close(SF),
    SFN.

fmt_command(systemtap, SF) ->
    Exec = filename:dirname(os:find_executable(get_vm_executable())),
    fl(["env PATH=", Exec, "/:$PATH ", "stap -x ", os:getpid(), " ", SF]).

get_vm_executable() ->
    case erlang:system_info(multi_scheduling) of
        disabled -> "beam";
        _        -> "beam.smp"
    end.

fl(L) ->
    lists:flatten(L).

get_node_name({_RegName, Node}) ->
    Node;
get_node_name(Arg) ->
    try node(Arg)
    catch _E1:_E2 ->
            nonode
    end.

