#!/bin/sh
rebar compile skip_deps=true; erl -pa ebin -pa deps/*/ebin -s erl2048_app start -noshell -detached