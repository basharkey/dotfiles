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
time launch_app thunderbird '"class": "thunderbird"'

swaymsg "workspace 5"
time launch_app "flatpak run com.discordapp.Discord" '"name": "Discord"'

swaymsg "workspace 3"
