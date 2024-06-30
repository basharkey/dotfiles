#!/usr/bin/env bash
environment=$1
override=$2

if [ -z "$environment" ]; then
    echo "Install dotfiles"
    echo
    echo "Usage: $0 user|system <override>"
    echo "e.g. $0 user music"
    echo
    exit 1
fi

if [ "$environment" = "user" ]; then
   cp -rT ./user/base ~/

   if [ -n "$override" ]; then
       if [ -d "./user/overrides/$override" ]; then
	   cp -rT "./user/overrides/$override" ~/
       else
	   echo "Failed to copy \"user\" overrides, directory \"./user/overrides/$override\" not found"
       fi
   fi

elif [ "$environment" = "system" ]; then
    sudo cp -rT ./system/base /

    if [ -n "$override" ]; then
	if [ -d "./system/overrides/$override" ]; then
	    sudo cp -rT "./system/overrides/$override" /
	else
	    echo "Failed to copy \"system\" overrides, directory \"./system/overrides/$override\" not found"
	fi
    fi
fi
