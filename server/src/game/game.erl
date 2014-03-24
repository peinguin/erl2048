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

addStartTiles(Grid, 0) -> 
    Grid;
addStartTiles(Grid, N) -> 
    NewGrid = addRandomTile(Grid),
    addStartTiles(NewGrid, N - 1).
addStartTiles(Grid) ->
    addStartTiles(Grid, 2).

addRandomTile(Grid) ->
    random:seed(now()),
    case grid:cellsAvailable(Grid) of
        true -> 
            case random:uniform(10) < 9 of
                true -> Tile = tile:init();
                false -> Tile = tile:init(grid:size())
            end,
            grid:insertTile(grid:randomAvailableCell(Grid), Tile, Grid);
        false -> Grid
    end.

getVector(left) ->
    { -1, 0 };
getVector(up) ->
    { 0,  -1 };
getVector(right) ->
    { 1,  0 };
getVector(down) ->
    { 0,  1 }.

buildTraversals() ->
    Traver = lists:seq(1, grid:size()),
    { Traver, Traver }.
buildTraversals({ 1 , _ }) ->
    { T1, T2} = buildTraversals(),
    { lists:reverse(T1), T2 };
buildTraversals({ _ , 1 }) ->
    { T1, T2} = buildTraversals(),
    { T1, lists:reverse(T2) };
buildTraversals({ _ , _ }) ->
    buildTraversals().

prepareTiles( Grid ) ->
    prepareTiles( Grid, 0).
prepareTiles([], _) ->
    null;
prepareTiles([Row | Tail], Y) ->
    [ prepareTileY(Row, 0, Y) | prepareTiles(Tail, Y + 1)].
prepareTileY([], _, _) ->
    null;
prepareTileY([Cell | Tail], X, Y) ->
    [prepareTileX(Cell, X, Y) | prepareTileY(Tail, X + 1, Y) ].
prepareTileX(Tile, X, Y) ->
    tile:prepareTiles(Tile, {X, Y}).

process_travesals_y([], _, Grid, Vector) ->
    Grid;
process_travesals_y(_, [], Grid, Vector) ->
    Grid;
process_travesals_y([ Y | Tail ], TraversalsX, Grid, Vector) ->
    process_travesals_y(
        Tail,
        TraversalsX,
        process_travesals_y( Y, TraversalsX, Grid, Vector),
        Vector
    );
process_travesals_y(Y, [ X | Tail ], Grid, Vector) ->
    process_travesals_y(Y, Tail, process_travesals_y( Y, X, Grid, Vector ), Vector);
process_travesals_y( Y, X, Grid, Vector ) ->
    moveTile({ X, Y }, Grid, Vector).

findFarthestPosition({X, Y}, Grid, Vector) ->
    .

moveTile(null, _, _) -> 
    null;
moveTile(Cell, Grid, Vector) ->
    { Farthest, Next } = findFarthestPosition(Cell, Grid, Vector),
    NextTile  = grid:cellContent(Next),

    Tile = grid:cellContent(Cell, Grid),
    {struct, CurrJsonData} = Tile,
    CurrValue = proplists:get_value(value, CurrJsonData),

    {struct, NextJsonData} = Next,
    NextValue = proplists:get_value(value, NextJsonData),

%    Moved = 
        if
            Next =:= null,
            CurrValue =:= NextValue,
            proplists:get_value(mergedFrom, CurrJsonData) =:= null
            ->
                Merged = {
                    struct,
                    [
                        {value, CurrValue * 2},
                        {mergedFrom, [Tile, Next]},
                        {previousPosition, null}
                    ]
                },
                grid:removeTile(Cell, grid:insertTile(Merged, Grid))

    %          // Converge the two tiles' positions
    %          tile.updatePosition(positions.next);

    %          // Update the score
    %          self.score += merged.value;

    %          // The mighty 2048 tile
    %          if (merged.value === 2048) self.won = true;
            ;
            true ->
                grid:moveTile(Cell, Next, Grid)
        end.

move(left, State) ->
    move(getVector(left), State);
move(right, State) -> 
    {struct, JsonData} = State,
    {struct, mochilists:set_default({lalal, left}, proplists:delete(lalal, JsonData))};
move(up, State) -> 
    {struct, JsonData} = State,
    {struct, mochilists:set_default({lalal, left}, proplists:delete(lalal, JsonData))};
move(down, State) -> 
    {struct, JsonData} = State,
    {struct, mochilists:set_default({lalal, left}, proplists:delete(lalal, JsonData))};
move(Vector, State) ->
    %TODO:
    %if (this.isGameTerminated()) return; // Don't do anything if the game's over

    { TraversalsX, TraversalsY } = buildTraversals(Vector),

    {struct, JsonData} = State,
    Grid = prepareTiles(proplists:get_value(grid, JsonData)),

    NewGrid = process_travesals_y(TraversalsY, TraversalsX, Grid, Vector),
erlang:display(NewGrid),
    {
        struct,
        [
            {grid , NewGrid},
            {name , proplists:get_value(name, JsonData)},
            {score, 0}
        ]
    }

%  if (moved) {
%    this.addRandomTile();

%    if (!this.movesAvailable()) {
%      this.over = true; // Game over!
%    }

%    this.actuate();
    .