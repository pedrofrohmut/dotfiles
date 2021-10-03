# Keyboard input config [delay/rate]
xset r rate 250 30

# Disable capslock # [caps:escape] map it to Esc
setxkbmap -option caps:none

#  --- Apps ---------------------------------------------------------------------------------

# Wallpaper
nitrogen --restore &

# Night Light
redshift -l -23.52:-46.35 -t 5700:3600 &

# Network Manager
nm-applet &

# Pamac tray
pamac-tray &

# File Manager
#dolphin --deamon &

# Hide Mouse Cursor
unclutter --timeout 2 --ignore-scrolling &

# KDE Polkit
exec --no-startup-id /usr/lib/polkit-kde-authentication-agent-1 &

# Composer
picom -i 1.00 --inactive-dim 0.25 &
