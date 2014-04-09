#!/bin/bash

erl -pa apps/*/ebin -pa deps/*/ebin/ -sname client -setcookie agh -eval "application:start(client)"
