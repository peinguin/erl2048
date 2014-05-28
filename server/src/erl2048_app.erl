%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(erl2048_app).
-behaviour(application).

%% API.
-export([start/2, start/0]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {file, "../client/index.html"}},
            {"/websocket", ws_handler, []},
            {"/static/[...]", cowboy_static, {dir, "../client/static"}}
        ]}
    ]),
    {ok, _} = db:start_link(),
    cowboy:start_http(http, 100, [{port, 8081}],
        [{env, [{dispatch, Dispatch}]}]).

start() ->
    application:ensure_all_started(erl2048).

stop(_State) ->
    {ok, _} = db:stop(),
    ok.
