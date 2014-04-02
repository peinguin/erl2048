-module(db).

-export([start_link/0,stop/0]).
-export([insert/2, select/0, createUser/1, changeName/2]).

start_link() ->
    {ok, PID} = sqlite3:open(db, [{file, "db.sqlite3"}]),

    Tables = sqlite3:list_tables(db),

    case lists:member("scores", Tables) of false ->
        sqlite3:create_table(db, scores, [{id, integer, [{primary_key, [asc, autoincrement]}]}, {userid, integer}, {score, integer}])
    end,

    case lists:member("users", Tables) of false ->
        sqlite3:create_table(db, users, [{id, integer, [{primary_key, [asc, autoincrement]}]}, {name, text}])
    end,

    {ok, PID}.

stop() ->
    sqlite3:close(db).

select() ->
    Ret = sqlite3:sql_exec(db, "select users.name, scores.score from scores LEFT JOIN users ON (users.id = scores.userid) ORDER BY score desc;"),
    [{columns,_},{rows,Rows}] = Ret,
    formatScores(Rows).

insert(Score, Player) ->
    sqlite3:delete(db, users, {userid, Player}),
    sqlite3:write(db, scores, [{userid, Player}, {score, Score}]),
    sqlite3:sql_exec(db, "DELETE FROM scores WHERE id IN (SELECT id FROM scores ORDER BY score desc LIMIT 1 OFFSET 10)").

formatScores([]) ->
    [];
formatScores([{Name, Score} | Rows]) ->
    [{struct, [{name, Name},{score, Score}]} | formatScores(Rows)].

createUser(UserName) ->
    sqlite3:write(db, users, [{name, UserName}]).

changeName(Id, NewName) ->
    sqlite3:update(db, users, {id, Id}, [{name, NewName}]).