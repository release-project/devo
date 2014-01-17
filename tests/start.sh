#!/bin/sh
erl -name $1 $2 -setcookie "secret" -pa ../ebin -config s_group.config 

 
