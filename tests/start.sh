#!/bin/sh
erl -name $1  -setcookie "secret" -pa ../ebin -config s_group.config 

 
