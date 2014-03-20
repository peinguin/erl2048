-module(game).

-export([init/1, move/2]).

init(State) ->

    {struct, JsonData} = State,
    Name = proplists:get_value(name, JsonData),

    {
        struct,
        [
            {grid ,addStartTiles(grid:build())},
            {name ,Name},
            {score,0}
        ]
    }.

addStartTiles(Grid, N) ->
    NewGrid = addRandomTile(Grid),
    if
        N > 0 -> addStartTiles(NewGrid, N - 1);
        true -> NewGrid
    end.
addStartTiles(Grid) ->
    addStartTiles(Grid, 2).

addRandomTile(Grid) ->
    case grid:cellsAvailable(Grid) of
        true -> 
            case random:uniform(10) < 9 of
                true -> Tile = 4;
                false -> Tile = 2
            end,
            grid:insertTile(grid:randomAvailableCell(), Tile, Grid);
        false -> Grid
    end.

move(left, State) ->
    {struct, JsonData} = State,
    {struct, mochilists:set_default({lalal, left}, proplists:delete(lalal, JsonData))};
move(right, State) -> 
    {struct, JsonData} = State,
    {struct, mochilists:set_default({lalal, left}, proplists:delete(lalal, JsonData))};
move(up, State) -> 
    {struct, JsonData} = State,
    {struct, mochilists:set_default({lalal, left}, proplists:delete(lalal, JsonData))};
move(down, State) -> 
    {struct, JsonData} = State,
    {struct, mochilists:set_default({lalal, left}, proplists:delete(lalal, JsonData))}.