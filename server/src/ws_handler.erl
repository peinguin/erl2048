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
    State = {struct, [ 
        { user, { struct, [{id, null},{name, <<"Player">>}] } } 
    ]},
    {ok, Req, State}.

websocket_handle({text, Msg}, Req, State) ->
    Message = mochijson2:decode(Msg, [{format, proplist}]),
    Action =  binary_to_list(proplists:get_value(<<"action">>, Message)),
    {NewState, Response} = case Action of
        "start" ->
            TmpState = game:init(State),
            {TmpState, TmpState};
        "move"  ->
            TmpState = game:move(list_to_atom(binary_to_list(proplists:get_value(<<"value">>, Message))), State),

            %Checking for won or lose
            {struct, JsonData} = State,
            Over = proplists:get_value(over, JsonData),
            Won  = proplists:get_value(won, JsonData),
            KeepPlaying = proplists:get_value(keepPlaying, JsonData),

            Resp = if
                Over =:= true -> {struct, [{over, true}]};
                (Won =:= true) and (KeepPlaying =:= false) -> {struct, [{ask, true}]};
                true -> TmpState
            end,

            {TmpState, Resp};
        "newName" ->
            NewName = proplists:get_value(<<"value">>, Message),
            JsonData = element(2, State),

            User = proplists:get_value(user, JsonData),
            {struct,UserJsonData} = User,

            Id = proplists:get_value(id, UserJsonData),

            db:changeName(Id, NewName),

            TmpState = {struct, [
                    { user, { struct, [ { name, NewName },{ id, Id } ] } }
                    | proplists:delete(user, JsonData)
                ]},
            {
                TmpState,
                {struct, [{ user, { struct, [ { name, NewName },{ id, Id } ] } }]}
            };
        "keepPlaying" ->
            TmpState = {struct, [ { keepPlaying, true } | proplists:delete(keepPlaying, element(2, State)) ]},
            {TmpState, TmpState};
        _Else -> State
    end,
    {reply, {text, mochijson2:encode(Response)}, Req, NewState};

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({send, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
