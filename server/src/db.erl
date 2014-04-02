-module(db).

-export([start_link/0,stop/0]).
-export([insert/2, select/0]).

start_link() ->
    {ok, PID} = sqlite3:open(db, [{file, "db.sqlite3"}]),

    case lists:member("scores", sqlite3:list_tables(db)) of false ->
        sqlite3:create_table(db, scores, [{id, integer, [primary_key]}, {name, text}, {score, integer}]);
        _Else -> undefined
    end,

    {ok, PID}.

stop() -> erlang:display(stop),
    sqlite3:close(db).

select() ->
    [{columns, _Columns}, {rows, Rows}] = sqlite3:sql_exec(db, "select * from scores ORDER BY score desc;"),
    Rows.

insert(Player, Score) ->
    sqlite3:write(db, scores, [{name, Player}, {score, Score}]),
    sqlite3:sql_exec("DELETE FROM scores WHERE id IN (SELECT id FROM scores ORDER BY score OFFSET 10)").