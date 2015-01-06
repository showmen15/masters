#!/bin/bash

erl -pa apps/*/ebin -pa deps/*/ebin/ -sname state_manager@sm -setcookie agh -eval "application:start(state_manager)"
