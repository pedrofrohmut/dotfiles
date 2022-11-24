#! /usr/bin/env bash

function start_apps() {
    xset r rate 250 30 &
    setxkbmap -layout us -option caps:escape &
    pa-applet &
    nm-applet &
    pamac-tray &
    lxsession &
    nitrogen --restore &
    xfce4-power-manager &
    unclutter --timeout 2 --ignore-scrolling &
    redshift -l -23.52:-46.35 -t 6500:4500 &
    picom --config ~/.config/picom/picom.conf &
}

case "$1" in
    "start"  ) start_apps ;;
    *        ) echo "You may enter 'start' as arg" ;;
esac
