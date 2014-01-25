-module(devo_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-compile(export_all).

-record(state, {
               count = 1             :: integer(),
               cmd   = undefined     :: any(),
               mechanism = undefined :: trace | dtrace,
               nodes = []            :: [node()],
               data  = []            :: any()
              }).                        

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    case whereis(devo) of 
        undefined ->
            register(devo, self());
        _ -> 
            unregister(devo),
            register(devo, self())
    end,
    {ok, Req, #state{}}.

websocket_handle({text, <<"stop">>}, Req, State) ->
    Mechanism = State#state.mechanism,
    Cmd = State#state.cmd, 
    Nodes = State#state.nodes,
    stop_profiling(Mechanism, Cmd, Nodes),
    {shutdown, Req, State};
websocket_handle({text, <<"start">>}, Req, State) ->
    {ok, Req, State};
websocket_handle({text, Msg}, Req, State) ->
    MsgStr = binary_to_list(Msg),
    case string:tokens(MsgStr, ":") of 
        ["start_profile", "message_queue_len", NodeStr, Mechanism] ->
            Nodes = string:tokens(NodeStr, ";"),
            Ns = [list_to_atom(N)||N<-Nodes],
            Cmd = message_queue_len,
            M = list_to_atom(Mechanism),
            %% Hardcoded process name, will be removed.
            start_profiling(M, Cmd, {sd_orbit, Ns}),
            NewState = State#state{mechanism=M, cmd=Cmd, nodes=Ns},
            {ok, Req, NewState};
        ["start_profile", Feature, NodeStr, Mechanism] ->
            Nodes= string:tokens(NodeStr, ";"),
            Ns = [list_to_atom(N)||N<-Nodes],
            Cmd = list_to_atom(Feature),
            M = list_to_atom(Mechanism),
            start_profiling(M, Cmd, Ns),
            NewState = State#state{mechanism=M, cmd=Cmd, nodes=Ns},
            {ok, Req, NewState};
        _ ->
            Msg = "Unexpected message from client:"++ binary_to_list(Msg),
            {reply, {text,list_to_binary(Msg)}, State}
    end;
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.


websocket_info({timeout, _Ref, stop_profile}, Req, State) ->
    {shutdown, Req, State};
websocket_info({timeout, _Ref, _Msg}, Req, State) ->
    Cnt = State#state.count,
    StateStr=case lists:member(State#state.cmd,
                               [migration, rq_migration])
             of 
                 true ->
                     Data = [{From, To, Times}||
                                {{From, To}, Times}<-State#state.data],
                     lists:flatten(
                       io_lib:format("{~.3f,~p}.", 
                                     [Cnt*200/1000,Data]));
                 false -> 
                     lists:flatten(
                       io_lib:format("{~.3f,~p}.", 
                                     [Cnt*200/1000,
                                      State#state.data]))
             end,
    erlang:start_timer(200, self(), <<"timeout">>),
    {reply, {text, list_to_binary(rm_whites(StateStr))}, Req, 
     State#state{count=Cnt+1, data=[]}};
websocket_info({trace_inter_node, From, To,MsgSize}, Req, State=#state{data=Data}) ->
    Key = {From, To},
    NewData=case lists:keyfind(Key, 1, Data) of
                false ->
                    [{{From, To},1, MsgSize}|Data];
                {_, Count, SumSize} ->
                    lists:keyreplace({From, To}, 1, 
                                     Data, {Key, Count+1, MsgSize+SumSize})
            end,
    {ok, Req, State#state{data=NewData}};
websocket_info({trace_rq, FromRq, ToRq}, Req, State=#state{data=Data}) ->
    Key = {FromRq, ToRq},
    NewData=case lists:keyfind(Key, 1, Data) of
                 false ->
                     [{{FromRq, ToRq},1}|Data];
                 {_, Count} ->
                     lists:keyreplace({FromRq, ToRq}, 1, 
                                      Data, {Key, Count+1})
             end,
    {ok, Req, State#state{data=NewData}};
websocket_info(_Info={run_queues_info, Ts, Rqs}, Req, State) ->
    Str=lists:flatten([" "++integer_to_list(Len)++" "
                       ||Len<-tuple_to_list(Rqs)]),
    InfoStr=lists:flatten(io_lib:format("~p ~s", [Ts, Str])),
    {reply, {text, list_to_binary(InfoStr)}, Req, State};
websocket_info(_Info={message_queue_len_info, Ts, Len}, Req, State) ->
    InfoStr=lists:flatten(io_lib:format("~p ~p", [Ts, Len])),
    {reply, {text, list_to_binary(InfoStr)}, Req, State};
websocket_info(Info={s_group, _Node, _Fun, _Args}, Req, State) ->
    InfoStr=lists:flatten(io_lib:format("~p.", [Info])),
    {reply, {text, list_to_binary(rm_whites(InfoStr))}, Req, State};

websocket_info(Info={cpu, _Cpu}, Req, State) ->
    InfoStr=lists:flatten(io_lib:format("~p.", [Info])),
    {reply, {text, list_to_binary(rm_whites(InfoStr))}, Req, State};
websocket_info(Info={s_group_init_config, _Config}, Req, State) ->
    InfoStr=lists:flatten(io_lib:format("~p.", [Info])),
    {reply, {text, list_to_binary(rm_whites(InfoStr))}, Req, State};
websocket_info(start_profile, Req, State) ->
    erlang:start_timer(1, self(), <<"Online profiling started...!">>),
    {ok, Req, State};
websocket_info(stop_profile, Req, State) ->
    self()!stop_profile,
    {ok, Req, State};
websocket_info(Info, Req, State) ->
    io:format("Devo_ws_handler:unexpected trace:~p\n", [Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.


start_profiling(trace, rq, [Node|_]) ->
    case rpc:call(Node, erlang, system_info, [cpu_topology]) of 
        {badrpc, Reason} ->
            io:format("Devo failed to start profiling for reason:\n~p\n",
                      [Reason]);
        Cpu ->
            self()!{cpu, Cpu},
            Res=rpc:call(Node, devo_sampling, start,
                         [[run_queues], infinity, none, {devo, node()}]),
            case Res of 
                {badrpc, Reason} ->
                    io:format("Devo failed to start profiling for reason:\n~p\n",
                              [Reason]);
                _ -> Res
            end
    end;
start_profiling(trace, migration, [Node|_]) ->
    case rpc:call(Node, erlang, system_info, [cpu_topology]) of 
        {badrpc, Reason} ->
            io:format("Devo failed to start profiling for reason:\n~p\n",
                      [Reason]);
        Cpu ->
            self()!{cpu, Cpu},
            erlang:start_timer(1, self(), <<"Online profiling started...!">>),
            devo_trace:start_trace(migration, Node, {devo, node()})
    end;
start_profiling(trace, rq_migration, [Node|_]) ->
    case rpc:call(Node, erlang, system_info, [cpu_topology]) of 
        {badrpc, Reason} ->
            io:format("Devo failed to start profiling for reason:\n~p\n",
                      [Reason]);
        Cpu ->
            self()!{cpu, Cpu},
            erlang:start_timer(1, self(), <<"Online profiling started...!">>),
            Res=rpc:call(Node, devo_sampling, start,
                         [[run_queues], infinity, none,{devo, node()}]),
            case Res of 
                {badrpc, Reason} ->
                    io:format("Devo failed to start profiling for reason:\n~p\n",
                              [Reason]);
                _ -> 
                    devo_trace:start_trace(migration, Node, {devo, node()})
            end
    end;
start_profiling(trace, message_queue_len, {RegName, [Node|_]}) ->
  %%  erlang:start_timer(1, self(), <<"Online profiling started...!">>),
    Res=rpc:call(Node, devo_sampling, start,
                 [[{message_queue_len, RegName}], infinity, none,{devo, node()}]),
    case Res of 
        {badrpc, Reason} ->
            io:format("Devo failed to start profiling for reason:\n~p\n",
                      [Reason]);
        _ -> Res
    end;
start_profiling(trace, inter_node, Nodes=[N|_Ns]) ->
    case get_init_s_group_config(N) of 
        {badrpc, Reason} ->
            io:format("Devo failed to start profiling for reason:\n~p\n",
                      [Reason]);
        undefined ->
            self()!{s_group_init_config, []},
            erlang:start_timer(1, self(), <<"Online profiling started...!">>),
            devo_trace:start_trace(inter_node, Nodes, {devo, node()});
        {ok, NodeGrps} ->
            self()!{s_group_init_config, NodeGrps},
            erlang:start_timer(1, self(), <<"Online profiling started...!">>),
            devo_trace:start_trace(inter_node, Nodes, {devo, node()})            
    end;
start_profiling(trace, s_group, Nodes=[N|_Ns]) ->
    case get_init_s_group_config(N) of 
        {badrpc, Reason} ->
            io:format("Devo failed to start profiling for reason:\n~p\n",
                      [Reason]);
        undefined ->
            self()!{s_group_init_config, []},
            devo_trace:start_trace(s_group, Nodes, {devo, node()});
        {ok, NodeGrps} ->
            self()!{s_group_init_config, NodeGrps},
            devo_trace:start_trace(s_group, Nodes, {devo, node()})
    end;
start_profiling(trace, Cmd, _Nodes) ->
    io:format("start_profiling: unnexpected command:~p\n", [Cmd]),
    ok;

start_profiling(dtrace, rq, [Node|_]) ->
    case rpc:call(Node, erlang, system_info, [cpu_topology]) of
        {badrpc, Reason} ->
            io:format("Devo failed to start profiling for reason:\n~p\n",
                      [Reason]);
        Cpu ->
            self() ! {cpu, Cpu},
            case rpc:call(Node, devo_dtrace, start_trace, 
                          [[run_queue_size], {devo, node()}]) of
                {badrpc, Reason} ->
                    io:format("Devo failed to start profiling for reason:\n~p\n",
                              [Reason]);
                Result -> 
                    Result
            end
    end;
start_profiling(dtrace, migration, [Node|_]) ->
    case rpc:call(Node, erlang, system_info, [cpu_topology]) of
        {badrpc, Reason} ->
            io:format("Devo failed to start profiling for reason:\n~p\n",
                      [Reason]);
        Cpu ->
            self() ! {cpu, Cpu},
            erlang:start_timer(1, self(), <<"Online profiling started...!">>),
            case rpc:call(Node, devo_dtrace, start_trace,
                          [[process_migration], {devo, node()}]) of
                {badrpc, Reason} ->
                    io:format("Devo failed to start profiling for reason:\n~p\n",
                              [Reason]);
                Result ->
                    Result
            end
    end;
start_profiling(dtrace, rq_migration, [Node|_]) ->
    case rpc:call(Node, erlang, system_info, [cpu_topology]) of
        {badrpc, Reason} ->
            io:format("Devo failed to start profiling for reason:\n~p\n",
                      [Reason]);
        Cpu ->
            self() ! {cpu, Cpu},
            erlang:start_timer(1, self(), <<"Online profiling started...!">>),
            case rpc:call(Node, devo_dtrace, start_trace,
                          [[process_migration, run_queue_size], 
                          {devo, node()}]) of
                {badrpc, Reason} ->
                    io:format("Devo failed to start profiling for reason:\n~p\n",
                              [Reason]);
                Result ->     
                    Result
            end
    end;
start_profiling(dtrace, message_queue_len, {RegName, [Node|_]}) ->
    case rpc:call(Node, devo_dtrace, start_trace,
                  [[{message_queue_size, RegName}], {devo, node()}]) of
        {badrpc, Reason} ->
            io:format("Devo failed to start profiling for reason:\n~p\n",
                      [Reason]);
        Result ->
            Result
    end;
start_profiling(dtrace, inter_node, Nodes=[N|_Ns]) ->
    case get_init_s_group_config(N) of
        {badrpc, Reason} ->
            io:format("Devo failed to start profiling for reason:\n~p\n",
                      [Reason]);
        undefined ->
            self() ! {s_group_init_config, []},
            erlang:start_timer(1, self(), <<"Online profiling started...!">>),
            erlang:foreach(fun(Node) -> 
                               case rpc:call(Node, devo_dtrace, start_trace,
                                             [[inter_node_communication], 
                                             {devo, node()}]) of
                                   {badrpc, Reason} ->
                                       io:format("Devo failed to start profiling for reason:\n~p\n",
                                                 [Reason]);
                                   Result ->
                                       Result
                               end
                           end, Nodes);
        {ok, NodeGrps} ->
            self() ! {s_group_init_config, NodeGrps},
            erlang:start_timer(1, self(), <<"Online profiling started...!">>),
            erlang:foreach(fun(Node) ->
                               case rpc:call(Node, devo_dtrace, start_trace,
                                             [[inter_node_communication],
                                             {devo, node()}]) of
                                   {badrpc, Reason} ->
                                       io:format("Devo failed to start profiling for reason:\n~p\n",
                                                 [Reason]);
                                   Result ->
                                       Result
                               end
                           end, Nodes)
    end;
start_profiling(dtrace, s_group, Nodes=[N|_Ns]) ->
    case get_init_s_group_config(N) of
        {badrpc, Reason} ->
            io:format("Devo failed to start profiling for reason:\n~p\n",
                      [Reason]);
        undefined ->
            self() ! {s_group_init_config, []},
            erlang:foreach(fun(Node) ->
                               case rpc:call(Node, devo_dtrace, start_trace,
                                             [[s_group], {devo, node()}]) of
                                   {badrpc, Reason} ->
                                       io:format("Devo failed to start profiling for reason:\n~p\n",
                                                 [Reason]);
                                   Result ->
                                       Result
                               end
                           end, Nodes);
        {ok, NodeGrps} ->
            self()!{s_group_init_config, NodeGrps},
            erlang:foreach(fun(Node) ->
                               case rpc:call(Node, devo_dtrace, start_trace,
                                             [[s_group], {devo, node()}]) of
                                   {badrpc, Reason} ->
                                       io:format("Devo failed to start profiling for reason:\n~p\n",
                                                 [Reason]);
                                   Result ->
                                       Result
                               end
                           end, Nodes)
    end;
start_profiling(dtrace, Cmd, _Nodes) ->
    io:format("start_profiling: unnexpected command:~p\n", [Cmd]),
    ok;

start_profiling(Mechanism, _Cmd, _Nodes) ->
    io:format("start_profiling: unexpected tracing mechanism:~p\n", [Mechanism]),
    ok.


stop_profiling(trace, rq, [Node|_]) ->
    Res=rpc:call(Node, devo_sampling, stop,[]),
    case Res of 
        {badrpc, Reason} ->
            io:format("Devo failed to stop profiling for reason:\n~p\n",
                      [Reason]);
        _ -> 
           Res
    end;
stop_profiling(trace, message_queue_len, [Node|_]) ->
    Res=rpc:call(Node, devo_sampling, stop,[]),
    case Res of 
        {badrpc, Reason} ->
            io:format("Devo failed to stop profiling for reason:\n~p\n",
                      [Reason]);
        _ -> 
           Res
    end;

stop_profiling(trace, migration, [_Node|_]) ->
    devo_trace:stop_trace();
stop_profiling(trace, rq_migration, [Node|_]) ->
    Res=rpc:call(Node, devo_sampling, stop,[]),
    case Res of 
        {badrpc, Reason} ->
            io:format("Devo failed to stop profiling for reason:\n~p\n",
                      [Reason]);
        _ -> 
            Res
    end,
    erlang:start_timer(1, self(), stop_profile),
    devo_trace:stop_trace();
stop_profiling(trace, inter_node, _Nodes) ->
    erlang:start_timer(1, self(), stop_profile),
    devo_trace:stop_trace();
stop_profiling(trace, s_group, _Nodes) ->
    devo_trace:stop_trace();
stop_profiling(trace, undefined,_) ->
    ok;
stop_profiling(trace, Cmd, _Nodes) ->
    io:format("stop_profiling: unnexpected command:~p\n", [Cmd]),
    ok;

stop_profiling(dtrace, rq, [Node|_]) ->
    case rpc:call(Node, devo_dtrace, stop_trace, []) of
        {badrpc, Reason} ->
            io:format("Devo failed to stop profiling for reason:\n~p\n",
                      [Reason]);
        Result ->
           Result
    end;
stop_profiling(dtrace, message_queue_len, [Node|_]) ->
    case rpc:call(Node, devo_dtrace, stop_trace, []) of
        {badrpc, Reason} ->
            io:format("Devo failed to stop profiling for reason:\n~p\n",
                      [Reason]);
        Result ->
           Result
    end;    
stop_profiling(dtrace, migration, [Node|_]) ->
    case rpc:call(Node, devo_dtrace, stop_trace, []) of
        {badrpc, Reason} ->
            io:format("Devo failed to stop profiling for reason:\n~p\n",
                      [Reason]);
        Result ->
           Result
    end;
stop_profiling(dtrace, rq_migration, [Node|_]) ->
    case rpc:call(Node, devo_dtrace, stop_trace, []) of
        {badrpc, Reason} ->
            io:format("Devo failed to stop profiling for reason:\n~p\n",
                      [Reason]);
        Result ->
           Result
    end,
    erlang:start_timer(1, self(), stop_profile);
stop_profiling(dtrace, inter_node, Nodes) ->
    lists:foreach(fun(Node) ->
                      case rpc:call(Node, devo_dtrace, stop_trace, []) of
                          {badrpc, Reason} ->
                              io:format("Devo failed to stop profiling for reason:\n~p\n",
                                        [Reason]);
                          Result ->
                              Result
                      end
                  end, Nodes),
    erlang:start_timer(1, self(), stop_profile);
stop_profiling(dtrace, s_group, Nodes) ->
    lists:foreach(fun(Node) ->
                      case rpc:call(Node, devo_dtrace, stop_trace, []) of
                          {badrpc, Reason} ->
                              io:format("Devo failed to stop profiling for reason:\n~p\n",
                                        [Reason]);
                          Result ->
                              Result
                      end
                  end, Nodes);
stop_profiling(dtrace, undefined,_) ->
    ok;
stop_profiling(dtrace, Cmd, _Nodes) ->
    io:format("stop_profiling: unexpected command:~p\n", [Cmd]),
    ok;

stop_profiling(Mechanism, _Cmd, _Nodes) ->
    io:format("stop_profiling: unnexpected tracing mechanism:~p\n", [Mechanism]),
    ok. 

get_init_s_group_config(Node) ->
    case rpc:call(Node, application, get_env, [kernel, s_groups]) of
        {badrpc, Reason} ->
            {badrpc, Reason};
        undefined ->
            {ok, []};
        {ok, NodeGrps} ->
            Grps = [grp_tuple(NodeGrp)||NodeGrp<-NodeGrps],
            {ok,Grps}
    end.

grp_tuple({Name, Nodes}) ->
    {Name, Nodes};  
grp_tuple({Name, _, Nodes}) ->
    {Name, Nodes}.


rm_whites(Str) ->
    [C||C<-Str, C=/=$\s, C=/=$\r,  C=/=$\n].
         
         
