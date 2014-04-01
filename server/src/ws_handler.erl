-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    State = {struct, [{name, <<"Player">>}]},
    {ok, Req, State}.

websocket_handle({text, Msg}, Req, State) ->
    case binary_to_list(Msg) of
        "start"      -> NewState = game:init(State);
        "move_left"  -> NewState = game:move(left, State);
        "move_right" -> NewState = game:move(right, State);
        "move_up"    -> NewState = game:move(up, State);
        "move_down"  -> NewState = game:move(down, State);
        _Else -> NewState = State
    end,
    {reply, {text, mochijson2:encode(NewState)}, Req, NewState};

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({send, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
