#!/bin/sh
# Set the screen DPI (uncomment this if needed!)
xrdb ~/.cache/emacs/Xresources   # set emacs dpi
# xrandr --dpi 168                  # set default app dpi

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

# fix java display
export _JAVA_AWT_WM_NONREPARENTING=1

# Fire it up
# exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.emacs.d/lisp/zw-exwm.el
# exec emacs -mm --debug-init -l ~/.emacs.d/lisp/zw-exwm.el
dbus-run-session emacs -mm --debug-init -l ~/.emacs.d/lisp/zw-exwm.el
