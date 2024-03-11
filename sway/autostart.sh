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

# PulseAudio
pasystray &

# Japanese typing
fcitx5 &

# Network (Manage your network connections)
nm-applet &

# --- Background Apps ----------------------------------------------------------

# Change color temperature
gammastep &

# Hide Mouse Cursor when idle for 2 seconds
unclutter --timeout 2 --ignore-scrolling &

# PolicyKit Authentication Agent (PolicyKit Authentication Agent)
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

# Emacs &
emacs --daemon &

# Waybar for sway
waybat &
