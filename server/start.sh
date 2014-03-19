#!/bin/sh
rebar compile skip_deps=true; erl -pa ebin deps/*/ebin -eval 'starter:start().' -noshell