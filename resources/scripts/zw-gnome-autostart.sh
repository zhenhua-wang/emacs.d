#!/bin/sh

# xremap
pkill xremap
xremap ~/.emacs.d/resources/scripts/keymap.yml --watch=device
