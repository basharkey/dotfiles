#!/usr/bin/env bash
launch_app () {
  echo -n "launching \"$1\""
  swaymsg "exec $1"
  
  swaymsg -t get_tree | grep -q "$2"
  while [ $? -ne 0 ]
  do
    sleep 0.1
    swaymsg -t get_tree | grep -q "$2"
  done
}

swaymsg "workspace 8"
time launch_app keepassxc '"app_id": "org.keepassxc.KeePassXC"'

swaymsg "workspace 7"
time launch_app "flatpak run io.github.spacingbat3.webcord" '"class": "WebCord"'
swaymsg "splith"
time launch_app evolution '"app_id": "evolution"'

swaymsg "workspace 3"
