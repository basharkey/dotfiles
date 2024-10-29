#!/usr/bin/env bash

usage() {
    echo "Usage: $0 -e [user|system] [-o <override>]"
    exit
}

while getopts "he:o:" opt; do
    case "$opt" in
	h)
	    usage
	    ;;
	e)
	    environment="$OPTARG"
	    if [ "$environment" != "user" ] && [ "$environment" != "system" ]; then
	       usage
	    fi
	    ;;
	o)
	    override="$OPTARG"
	    ;;
	*)
	    usage
	    ;;
    esac
done

if [ -z "$environment" ]; then
    usage
fi

if [ -n "$override" ] && [ ! -d "./$environment/overrides/$override" ]; then
    echo "overrides directory \"./$environment/overrides/$override\" not found"
    usage
fi


if [ "$environment" = "user" ]; then
   cp -rT ./user/base ~/

   if [ -n "$override" ]; then
       cp -rT "./user/overrides/$override" ~/
   fi

elif [ "$environment" = "system" ]; then
    sudo cp -rT ./system/base /

    if [ -n "$override" ]; then
	sudo cp -rT "./system/overrides/$override" /
    fi
fi
