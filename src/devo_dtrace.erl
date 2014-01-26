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
            Receiver ! M,
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

