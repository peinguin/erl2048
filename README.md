erl2048
=======

2048 written in erlang

#cd server
#rebar g-d
#rebar co
#rebar compile skip_deps=true; erl -pa ebin deps/*/ebin -eval 'starter:start().' -noshell -detached