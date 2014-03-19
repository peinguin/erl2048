-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-import(erl2048game,[init/0]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    State = erl2048game:init(),
    self() ! {timeout, self(), mochijson2:encode(State)},
    {ok, Req, State}.

websocket_handle({text, Msg}, Req, State) ->
    erlang:display(State),
    {reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
    erlang:display(State),
    {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    erlang:display(State),
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    erlang:display(State),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    erlang:display(_State),
    ok.
