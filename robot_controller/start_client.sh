#!/bin/bash


if [ $# -gt 0 ]; then
	WORLD_ARG="-world $1"
fi

erl -pa apps/*/ebin -pa deps/*/ebin/ -sname client -setcookie agh -eval "application:start(roboss_client)" $WORLD_ARG
