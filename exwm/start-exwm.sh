#!/bin/sh
# Set the screen DPI (uncomment this if needed!)
xrdb ~/.emacs.d/exwm/Xresources   # set emacs dpi
# xrandr --dpi 168                  # set default app dpi

# set xmodmap
xmodmap ~/.emacs.d/exwm/Xmodmap

# Run the screen compositor
picom &
# picom --config  ~/.config/picom.conf -b

# Enable screen locking on suspend
# xscreensaver --no-splash &
# xss-lock -- slock &
# xss-lock -- i3lock -n -i ~/Pictures/XJu51Ly-arch-linux-wallpaper.png &

# set current emacs as editor if being prompted
export VISUAL=emacsclient
export EDITOR="$VISUAL"

# start polybar wth theme
# bash ~/.config/polybar/launch.sh --material

# Fire it up
# exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.emacs.d/zw-exwm.el
exec dbus-launch emacs -mm --debug-init -l ~/.emacs.d/lisp/zw-exwm.el
