-module(erl2048game).

-export([init/0]).

-record(robot, {
	score=0,
	grid=[]
}).

init() ->
	#robot{}.