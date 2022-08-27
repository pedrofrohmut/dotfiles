#! /usr/bin/env bash

POWEROFF="Poweroff"
SUSPEND="Suspend"
REBOOT="Reboot"
LOCK_SUSPEND="Lock & Suspend"
LOCK="Lock"

themestr="
listview { 
    lines: 5; 
} 

window {
    padding: 20;
    width: 280;
}
"

rofi_cmd() {
    rofi -i -dmenu \
        -lines 4 \
        -theme-str "${themestr}" \
        -theme ~/.config/rofi/themes/my_dracula.rasi
}

run_rofi() {
    echo -e "$POWEROFF\n$SUSPEND\n$REBOOT\n$LOCK\n$LOCK_SUSPEND" | rofi_cmd
}

option="$(run_rofi)"

case $option in
    $POWEROFF ) 
        systemctl poweroff
        ;;
    $SUSPEND )
        systemctl suspend
        ;;
    $REBOOT )
        systemctl reboot
        ;;
    $LOCK ) 
        i3lock -u --image=/home/pedro/media/images/wallpaper/lock.png
        ;;
    $LOCK_SUSPEND )
        i3lock -u --image=/home/pedro/media/images/wallpaper/lock.png
        systemctl suspend
        ;;
    * ) 
        exit
        ;;
esac
