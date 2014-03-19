-module(starter).
-export([start/0]).

start() ->
	application:start(ranch),
	application:start(crypto),
	application:start(cowlib),
	application:start(cowboy),
	application:start(inets),
	application:start(mochiweb),
	application:start(erl2048).