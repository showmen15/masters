#!/bin/bash

ROBOT_NAME=`hostname`

if [ $# -gt 0 ]; then
	WORLD_ARG="-world $1"
else
	echo "Pass world name parameter"
	exit 1
fi

erl -pa apps/*/ebin -pa deps/*/ebin/ -sname amber_client -setcookie agh -eval "application:start(amber_client)" $WORLD_ARG -robot_name "$ROBOT_NAME"
