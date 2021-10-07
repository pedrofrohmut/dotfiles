# Keyboard input config [delay/rate]
xset r rate 250 30

# Disable capslock # [caps:escape] map it to Esc
setxkbmap -option caps:none

# --- Apps ---------------------------------------------------------------------------------

# Wallpaper
nitrogen --restore &

# Composer
picom -i 0.9 --inactive-dim 0.35 &

# Night Light [default -t 6500/4500]
redshift -l -23.52:-46.35 -t 6500:4500 &

# Network Manager
nm-applet &

# Pamac tray
pamac-tray &

# Hide Mouse Cursor
unclutter --timeout 2 --ignore-scrolling &

# KDE Polkit
exec --no-startup-id /usr/lib/polkit-kde-authentication-agent-1 &
