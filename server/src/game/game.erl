-module(game).

-export([init/1, move/2]).

init(State) ->

    StateUser = proplists:get_value(user, element(2, State)),
    StateUserJsonData = element(2, StateUser),

    User = case proplists:get_value(id, StateUserJsonData) of
        null ->
            Name = proplists:get_value(name, StateUserJsonData),
            {rowid, Id} = db:createUser(Name),
            { struct, [{name, Name},{id, Id}]};
        _Else ->
            StateUser
    end,

    {
        struct,
        [
            {grid ,addStartTiles(grid:build())},
            {user , User},
            {score,0},
            {scores, db:select()},
            {won, false},
            {over, false},
            {keepPlaying, false}
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

prepareTiles( [{_Key, _Value} | _Tail ] ) ->
    JsonData = [{_Key, _Value} | _Tail ],
    [{ grid, prepareTiles(proplists:get_value(grid, JsonData)) } | proplists:delete(grid, JsonData) ];
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
    tile:prepare(Tile, {X, Y}).

process_travesals_y([], _, _, JsonData) ->
    JsonData;
process_travesals_y(_, [], _, JsonData) ->
    JsonData;
process_travesals_y([ Y | Tail ], TraversalsX, Vector, JsonData) ->
    process_travesals_y(
        Tail,
        TraversalsX,
        Vector,
        process_travesals_y( Y, TraversalsX, Vector, JsonData)
    );
process_travesals_y(Y, [ X | Tail ], Vector, JsonData) ->
    process_travesals_y(Y, Tail, Vector, process_travesals_y( Y, X, Vector, JsonData ));
process_travesals_y( Y, X, Vector, JsonData ) ->
    moveTile({ X, Y }, Vector, JsonData).

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

moveTile(Cell, Vector, JsonData) ->

    Grid = proplists:get_value(grid, JsonData),
    Tile = grid:cellContent(Cell, Grid),

    case Tile =:= null of
        true -> JsonData;
        false ->
            { Farthest, Next } = findFarthestPosition(Cell, Vector, Grid),

            {struct, CurrJsonData} = Tile,
            CurrValue = proplists:get_value(value, CurrJsonData),

            NextTile = if
                Next =:= null -> null;
                true ->
                    grid:cellContent(Next, Grid)
            end,

            {NextValue, NextMerged} = if
                NextTile =:= null -> {null, null};
                true ->
                    NextJsonData = element(2, NextTile),
                    {proplists:get_value(value, NextJsonData), proplists:get_value(mergedFrom, NextJsonData)}
            end,

            if  CurrValue =:= NextValue,
                NextMerged =:= null
                ->
                    MergedValue = CurrValue * 2,
                    Merged = {
                        struct,
                        [
                            {value, MergedValue},
                            {mergedFrom, [Tile,NextTile]},
                            {previousPosition, null}
                        ]
                    },
                    NewGrid = grid:insertTile(Next, Merged, grid:removeTile(Cell, Grid)),

                    % Update the score
                    Score = proplists:get_value(score, JsonData) + MergedValue,

                    % The mighty 2048 tile
                    Won = if
                        MergedValue =:= 2048 -> true;
                        true -> false
                    end,

                    Removed = proplists:delete(score, proplists:delete(won, proplists:delete(grid, JsonData))),

                    [
                        {grid,NewGrid},
                        {won,Won},
                        {score,Score} |
                        Removed
                    ];
                true ->
                    [
                        {
                            grid,
                            grid:moveTile(Cell, Farthest, proplists:get_value(grid, JsonData))
                        }
                        | proplists:delete(grid, JsonData)
                    ]
            end
    end.

move(left, State) ->
    move(getVector(left), State);
move(right, State) -> 
    move(getVector(right), State);
move(up, State) -> 
    move(getVector(up), State);
move(down, State) -> 
    move(getVector(down), State);
move(Vector, State) ->
    {struct, JsonData} = State,

    case 
        proplists:get_value(over, JsonData) or (
            proplists:get_value(won, JsonData) and (not proplists:get_value(keepPlaying, JsonData))
        )
    of
        true -> State;
        _Else ->
            PreparedJsonData = updateBestScore(prepareTiles(JsonData)),

            { TraversalsX, TraversalsY } = buildTraversals(Vector),

            NewJsonData = process_travesals_y(
                TraversalsY,
                TraversalsX,
                Vector,
                PreparedJsonData
            ),

            if
                PreparedJsonData =/= NewJsonData -> %If changed - add new tile
                    Grid = proplists:get_value(grid, NewJsonData),
                    {struct, UserJsonData} = proplists:get_value(user, NewJsonData),

                    NewScore = proplists:get_value(score, NewJsonData),
                    Score = proplists:get_value(score, PreparedJsonData),

                    case NewScore > Score of true ->
                        db:insert(
                            proplists:get_value(score, NewJsonData),
                            proplists:get_value(id, UserJsonData)
                        );
                        _Else -> undefined
                    end,

                    Over = case movesAvailable(Grid) of
                        true -> false;
                        fale -> true % Game over!
                    end,
                    Removed = proplists:delete(grid, proplists:delete(over, NewJsonData)),
                    {struct,[{ grid, addRandomTile(Grid) }, { over, Over } | Removed ]};
                true -> %return state otherwise
                    {struct,PreparedJsonData}
            end
    end.

movesAvailable(_) ->
    true.

updateBestScore(JsonData) ->
    [{ scores, db:select() } | proplists:delete(scores, JsonData) ].