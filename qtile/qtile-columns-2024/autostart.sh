################################################################################
# autostart for Qtile Wayland ##################################################
################################################################################

#! /bin/sh

firefox &
swaybg -m stretch -i $HOME/media/wallpaper/1316292.jpeg &

### # --- ENV ----------------------------------------------------------------------
###
### export SHELL=/usr/bin/zsh
### export EDITOR=/usr/bin/nvim
### export PATH=$PATH:$HOME/.local/bin
###
### export GTK_IM_MODULE=fcitx
### export QT_IM_MODULE=fcitx
### export XMODIFIERS=@im=fcitx
###
### # --- System Tray --------------------------------------------------------------
###
### # PulseAudio
### pasystray &
###
### # Systray version of redshift (Easy to turn on and off)
### redshift-gtk &
###
### # Japanese typing
### fcitx-autostart &
###
### # Network (Manage your network connections)
### nm-applet &
###
### # --- Background Apps ----------------------------------------------------------
###
# Wallpaper
# swaybg -m stretch -i $HOME/media/wallpaper/1316292.jpeg &
###
### # Hide Mouse Cursor when idle for 2 seconds
### unclutter --timeout 2 --ignore-scrolling &
###
### # PolicyKit Authentication Agent (PolicyKit Authentication Agent)
### /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
###
### # Emacs &
### emacs --daemon &
###
### # --- Keyboard config ----------------------------------------------------------
###
### #xmodmap -e "clear lock"
### #xmodmap -e "keycode 66=Escape"
###
### # Keyboard Layout
### setxkbmap -layout us -option caps:escape &
