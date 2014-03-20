-module(grid).

-export([build/0, cellsAvailable/1, randomAvailableCell/1, insertTile/3]).

build() ->
    [[null || _ <- lists:seq(1, 4)] || _ <- lists:seq(1, 4)].

availableCells(Grid) ->
    availableCells(Grid, 0);
availableCells([[Grid] | Tail ], N) ->
    [availableCells(Grid, 0) | availableCells(Tail, N +1)];
availableCells([[Grid]], N) ->
    ;
availableCells([Grid | Tail ], N) ->
    ;
availableCells([Grid], N) ->
    .

cellsAvailable(Grid) ->
    length(availableCells(Grid)) > 0.

randomAvailableCell(Grid) ->
    {0,0}.

insertTile({X, Y}, Tile, Grid) ->
    Grid.