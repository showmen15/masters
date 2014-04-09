#!/bin/bash

werl -pa apps/roboss/ebin -pa deps/*/ebin/ -sname roboss -setcookie agh -eval "application:start(roboss)."
