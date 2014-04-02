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
    Message = mochijson2:decode(Msg, [{format, proplist}]),
    Action =  binary_to_list(proplists:get_value(<<"action">>, Message)),
    NewState = case Action of
        "start" ->
            game:init(State);
        "move"  ->
            game:move(list_to_atom(binary_to_list(proplists:get_value(<<"value">>, Message))), State);
        "newName" ->
            NewName = proplists:get_value(<<"value">>, Message),
            JsonData = element(2, State),

            User = proplists:get_value(user, JsonData),
            {struct,UserJsonData} = User,

            Id = proplists:get_value(id, UserJsonData),

            db:changeName(Id, NewName),
            
            {struct, [
                { user, { struct, [ { name, NewName },{ id, Id } ] } }
                | proplists:delete(user, JsonData)
            ]};
        _Else -> State
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
