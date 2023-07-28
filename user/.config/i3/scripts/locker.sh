#!/usr/bin/env bash
xset +dpms dpms $1 $1 $1
i3lock -n
xset -dpms dpms 0 0 0
