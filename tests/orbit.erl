-module(orbit).

-compile(export_all).

run_on_one_node() ->
    Nodes=['node1@127.0.0.1'],
    bench:dist_seq(fun bench:g124/1, 10000, 8,Nodes).
   
run_on_multi_nodes() ->
    Nodes = ['node1@127.0.0.1', 'node2@127.0.0.1', 'node3@127.0.0.1',
             'node4@127.0.0.1', 'node5@127.0.0.1'],
    bench:dist_seq(fun bench:g124/1, 10000, 8,Nodes).
    
