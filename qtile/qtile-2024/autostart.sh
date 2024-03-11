################################################################################
# autostart for Qtile X11 ######################################################
################################################################################

#! /usr/bin/env bash

# --- ENV ----------------------------------------------------------------------

export SHELL=/usr/bin/zsh
export EDITOR=/usr/local/bin/nvim
export PATH=$PATH:$HOME/.local/bin

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

# --- System Tray --------------------------------------------------------------

# PulseAudio
pasystray &

# Systray version of redshift (Easy to turn on and off)
redshift-gtk &

# Japanese typing
#fcitx5-autostart &
fcitx5 &

# Network (Manage your network connections)
nm-applet &

# --- Background Apps ----------------------------------------------------------

# Wallpaper
nitrogen --restore &

# Hide Mouse Cursor when idle for 2 seconds
unclutter --timeout 2 --ignore-scrolling &

# PolicyKit Authentication Agent (PolicyKit Authentication Agent)
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

# Emacs &
emacs --daemon &

# Compositor - transparency
picom &

# --- Keyboard config ----------------------------------------------------------

# Keyboard Layout
setxkbmap -layout us -option caps:escape &

# Keyboard delay and repeat rate
xset r rate 250 30 &
