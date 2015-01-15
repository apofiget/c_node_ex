-module(c_node_ex).

-export([foo/0, bar/0, baz/0,
         quax/0, other/0]).

foo() ->
    c_node_call(foo).

bar() ->
    c_node_call(bar).

baz() ->
    c_node_call(baz).

quax() ->
    c_node_call(quax).

other() ->
    c_node_call(other).

c_node_call(Fun) ->
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("c1@" ++ Host),
    case net_kernel:connect(Node) of
        true ->
            {any, Node} ! {self(),{Fun, some_atom_param}},
            receive
                {cnode, {reply, Some}} ->
                    io:format("C-node reply: ~p~n",[Some]);
                Any ->
                    io:format("Unexpected message: ~p~n",[Any])
            end;
        _ ->
            io:format("Can't connect to node: ~p",[Node])
    end.
