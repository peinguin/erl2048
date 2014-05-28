-module(my_proplists).
-export([delete_several/2]).

delete_several([Key | Tail], Proplist) -> 
	proplists:delete(Key, delete_several(Tail,Proplist));
delete_several([], Proplist) -> 
	Proplist.