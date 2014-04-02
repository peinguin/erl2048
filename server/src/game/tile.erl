-module(tile).

-export([init/1, init/0, prepare/2]).

prepare(null, _) ->
    null;
prepare(Tile, { X, Y }) ->
    {
        struct,
        [
            {value, proplists:get_value(value, element(2, Tile))},
            {mergedFrom, null},
            {previousPosition, {struct, [{ x, X - 1},{ y, Y - 1 }]}}
        ]
    }.
init(Value) ->
    {
        struct,
        [
            {value, Value},
            {mergedFrom, null},
            {previousPosition, null}
        ]
    }.
init() ->
    init(2).