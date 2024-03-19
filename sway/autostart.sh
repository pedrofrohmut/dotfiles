################################################################################
# autostart for Sway ###########################################################
################################################################################

#! /usr/bin/env bash

# --- ENV ----------------------------------------------------------------------

export SHELL=/usr/bin/zsh
export EDITOR=/usr/local/bin/nvim
export GIT_EDITOR=/usr/local/bin/nvim
export PATH=$PATH:$HOME/.local/bin

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

# --- System Tray --------------------------------------------------------------

# Japanese typing
fcitx5 &

# Network (Manage your network connections)
nm-applet &

# --- Background Apps ----------------------------------------------------------

# Change color temperature
wlsunset -T 6500 -t 4500 -g 0.9 -S 06:00 -s 19:00 & ### My preference temp
#wlsunset -l 23.52 -L 46.35 -T 5700 -t 3500 &       ### Temp Recommend

# PolicyKit Authentication Agent (PolicyKit Authentication Agent)
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

# Waybar for sway
waybat &
