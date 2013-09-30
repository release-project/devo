-module(test).

-compile(export_all).

test() ->
    Nodes = ['node1@127.0.0.1', 'node2@127.0.0.1', 'node3@127.0.0.1'],
    rpc:call('node1@127.0.0.1', bench, dist_seq, 
             [fun bench:g124/1, 10000, 8,Nodes]).

