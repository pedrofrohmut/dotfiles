#############
# .XSession #
#############

#Keyboard layout
setxkbmap br &

# Disable capslock # [caps:escape] map it to Esc
setxkbmap -option caps:none  &

# Keyboard input config [delay/rate]
xset r rate 250 30 &

# --- Tray ------------------------------------------------------------------------------

# Vomulme
volumeicon &

# Network Manager
nm-applet &

# Pamac tray
pamac-tray &

# --- Apps ------------------------------------------------------------------------------

# Wallpaper
nitrogen --restore; sleep 2 &

# Composer
picom --fading --inactive-opacity 1.0 --inactive-dim 0.35 &

# Night Light [default -t 6500/4500]
redshift -l -23.52:-46.35 -t 6500:4500 &

# Power Management
xfce4-power-manager &

# Hide Mouse Cursor
unclutter --timeout 2 --ignore-scrolling &

# Polkit
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
