-module(tile).

-export([init/1, init/0, prepareTiles/2]).

prepareTiles(null, _) ->
    null;
prepareTiles(Tile, Position) ->
    {struct, JsonData} = Tile,
    Value = proplists:get_value(value, JsonData),

    {
        struct,
        [
            {value, Value},
            {mergedFrom, null},
            {previousPosition, Position}
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