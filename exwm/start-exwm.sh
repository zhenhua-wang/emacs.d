#!/bin/sh
# Set the screen DPI (uncomment this if needed!)
xrdb ~/.emacs.d/exwm/Xresources   # set emacs dpi 
xrandr --dpi 168                  # set default app dpi


# Run the screen compositor
compton &

# Enable screen locking on suspend
xscreensaver --no-splash &
# xss-lock -- slock &
# xss-lock -- i3lock -n -i ~/Pictures/XJu51Ly-arch-linux-wallpaper.png &

# set current emacs as editor if being prompted
export VISUAL=emacsclient
export EDITOR="$VISUAL"

# Fire it up
# exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.emacs.d/emacs-desktop.el 
exec dbus-launch emacs -mm --debug-init -l ~/.emacs.d/emacs-desktop.el 
