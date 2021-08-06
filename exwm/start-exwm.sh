#!/bin/sh
# Set the screen DPI (uncomment this if needed!)
xrdb ~/.emacs.d/exwm/Xresources   # set emacs dpi 
xrandr --dpi 168                  # set default app dpi

# Run the screen compositor
compton &

# Enable screen locking on suspend
xss-lock -- slock &

# Fire it up
exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.emacs.d/emacs-desktop.el 
