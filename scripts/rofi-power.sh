#! /usr/bin/env bash

POWEROFF="Poweroff"
SUSPEND="Suspend"
REBOOT="Reboot"
LOCK_SUSPEND="Lock & Suspend"
LOCK="Lock"
CANCEL="Cancel"
YES="Yes"
NO="No"

maintheme="
    listview { lines: 5; } 
    window { padding: 20; width: 280; }"

confirmtheme="
    listview { lines: 3; } 
    window { padding: 20; width: 280; }"

rofi_cmd() {
    rofi -i -dmenu \
        -lines 4 \
        -theme-str "${maintheme}" \
        -theme ~/.config/rofi/themes/my_dracula.rasi
}

run_rofi() {
    echo -e "$POWEROFF\n$SUSPEND\n$REBOOT\n$LOCK\n$LOCK_SUSPEND" | rofi_cmd
}

rofi_confirm_cmd() {
    rofi -i -dmenu \
        -lines 3 \
        -theme-str "${confirmtheme}" \
        -theme ~/.config/rofi/themes/my_dracula.rasi
}

run_rofi_confirm() {
    echo -e "$CANCEL\n$NO\n$YES" | rofi_confirm_cmd 
}

option="$(run_rofi)"

case $option in
    $POWEROFF ) 
        confirm=$(run_rofi_confirm)
        if [ $confirm = $YES ]; then
            systemctl poweroff
        fi
        ;;
    $REBOOT )
        confirm=$(run_rofi_confirm)
        if [ $confirm = $YES ]; then
            systemctl reboot
        fi
        ;;
    $SUSPEND )
        systemctl suspend
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
