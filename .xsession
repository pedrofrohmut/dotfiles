#Keyboard layout
setxkbmap br &

# Disable capslock # [caps:escape] map it to Esc
setxkbmap -option caps:none  &

# Keyboard input config [delay/rate]
xset r rate 250 30 &

# --- Apps ---------------------------------------------------------------------------------

# Set up an icon tray
trayer --edge top --align right --SetPartialStrut true \
       --width 5 --tint 0x212121 --height 21 --alpha 0 &

# Wallpaper
nitrogen --restore &

# Composer
picom --fading --inactive-opacity 1.0 --inactive-dim 0.35 &

# Night Light [default -t 6500/4500]
redshift -l -23.52:-46.35 -t 6500:4500 &

# Network Manager
nm-applet &

# Pamac tray
pamac-tray &

# Power Management
xfce4-power-manager &

# Hide Mouse Cursor
unclutter --timeout 2 --ignore-scrolling &

# KDE Polkit
exec /usr/lib/polkit-kde-authentication-agent-1 &
