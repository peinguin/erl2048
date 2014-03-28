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
    prepareTiles( Grid, 1).
prepareTiles([], _) ->
    [];
prepareTiles([Row | Tail], Y) ->
    [ prepareTileY(Row, 1, Y) | prepareTiles(Tail, Y + 1)].
prepareTileY([], _, _) ->
    [];
prepareTileY([Cell | Tail], X, Y) ->
    [prepareTileX(Cell, X, Y) | prepareTileY(Tail, X + 1, Y) ].
prepareTileX(Tile, X, Y) ->
    tile:prepareTiles(Tile, {X, Y}).

process_travesals_y([], _, _, Grid) ->
    Grid;
process_travesals_y(_, [], _, Grid) ->
    Grid;
process_travesals_y([ Y | Tail ], TraversalsX, Vector, Grid) ->
    process_travesals_y(
        Tail,
        TraversalsX,
        Vector,
        process_travesals_y( Y, TraversalsX, Vector, Grid)
    );
process_travesals_y(Y, [ X | Tail ], Vector, Grid) ->
    process_travesals_y(Y, Tail, Vector, process_travesals_y( Y, X, Vector, Grid ));
process_travesals_y( Y, X, Vector, Grid ) ->
    NewGrid = moveTile({ X, Y }, Vector, Grid),
    if
        NewGrid =:= false -> Grid;
        true -> NewGrid
    end.

findFarthestPosition({X, Y}, {VecX, VecY}, Grid) ->

    Next = { X + VecX, Y + VecY },

    case grid:cellAvailable(Next, Grid) of
        true -> 
            findFarthestPosition(Next, {VecX, VecY}, Grid);
        false -> 
            {
                {X, Y},
                Next % Used to check if a merge is required
            }
    end.

moveTile(Cell, Vector, Grid) ->
    Tile = grid:cellContent(Cell, Grid),

    case Tile =:= null of
        true -> Grid;
        false ->
            { Farthest, Next } = findFarthestPosition(Cell, Vector, Grid),

            {struct, CurrJsonData} = Tile,
            CurrValue = proplists:get_value(value, CurrJsonData),
            CurrMerged = proplists:get_value(mergedFrom, CurrJsonData),

            NextValue = case Next =:= null of
                    false -> 
                        NextTile = grid:cellContent(Next, Grid),
                        case NextTile =:= null of
                            true -> null;
                            false -> proplists:get_value(value, NextTile)
                        end;
                    true -> null
                end,

            if  CurrValue =:= NextValue,
                CurrMerged =:= null
                ->
                    Merged = {
                        struct,
                        [
                            {value, CurrValue * 2},
                            {mergedFrom, [Tile, Next]},
                            {previousPosition, null}
                        ]
                    },
                    grid:removeTile(Cell, grid:insertTile(Cell, Merged, Grid))

        %          // Update the score
        %          self.score += merged.value;

        %          // The mighty 2048 tile
        %          if (merged.value === 2048) self.won = true;
                ;
                true ->
                    erlang:display({149, Cell, Farthest}),
                    grid:moveTile(Cell, Farthest, Grid)
            end
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

    NewGrid = process_travesals_y(TraversalsY, TraversalsX, Vector, Grid),
erlang:display({175,NewGrid}),
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
    .