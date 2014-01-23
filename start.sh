#!/bin/sh
erl -name devo@127.0.0.1  -setcookie "secret" -pa ../devo/ebin deps/*/ebin -s devo 

 
