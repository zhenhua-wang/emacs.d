#!/bin/sh

# remap keys
pkill xremap
xremap ~/.emacs.d/resources/scripts/keymap_exwm.yml --watch=device &

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

# enable gesture in firefox
export MOZ_USE_XINPUT2=1

# set current emacs as editor if being prompted
export VISUAL=emacsclient
export EDITOR="$VISUAL"

# fix java display
export _JAVA_AWT_WM_NONREPARENTING=1

# scale DPI
# export EXWM_DPI=$(xrdb -query | grep dpi | grep -o '[0-9]\+')
# export EXWM_DPI_SCALE=$((EXWM_DPI/96))
# export GDK_SCALE=$((EXWM_DPI_SCALE))
# export GDK_DPI_SCALE=$(awk -v var="$EXWM_DPI_SCALE" "BEGIN {print 1/var}")

# Fire it up
# exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.emacs.d/module/zw-exwm.el
# exec emacs -mm --debug-init -l ~/.emacs.d/module/zw-exwm.el
exec dbus-run-session emacs -mm --debug-init -l ~/.emacs.d/module/zw-exwm.el
