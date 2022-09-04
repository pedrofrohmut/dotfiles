#! /usr/bin/env bash

### APP Dependencies: systemd, i3lock

option=$1

if [ ! $option ]; then
    echo "Enter your choice [poweroff, reboot, lock, lock-suspend, suspend, cancel(Ctrl-c)]:"
    read option
fi

case $option in
    poweroff ) 
        systemctl poweroff
        ;;
    reboot )
        systemctl reboot
        ;;
    lock ) 
        i3lock -u --image=/home/pedro/media/images/wallpaper/lock.png
        ;;
    lock-suspend )
        i3lock -u --image=/home/pedro/media/images/wallpaper/lock.png
        systemctl suspend
        ;;
    suspend )
        systemctl suspend
        ;;
    * ) 
        echo "Please inform an option: poweroff, lock, lock-suspend, suspend" 
        ;;
esac
