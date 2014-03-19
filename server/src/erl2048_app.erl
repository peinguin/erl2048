%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(erl2048_app).
-behaviour(application).

%% API.
-export([start/2]).
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
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]),
    erl2048_sup:start_link().

stop(_State) ->
    ok.
