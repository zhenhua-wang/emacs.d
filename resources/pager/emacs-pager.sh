#!/bin/bash

# Call emacsclient as a pager.
# First, read in the content of stdin into a temporary file

t=$(mktemp /tmp/emacs-pager.XXXXXX) || exit 1

echo "Reading into emacs..."

# Remove terminal escape sequences (color and move, as well as some funky starship stuff)
cat - \
   | sed 's/\x1b\[[0-9;:]*[mGKH]//g' \
   | sed 's/\x1b\][0-9;:]*[AC]\x1b\\//g' \
    >> $t

sh -c 'emacs "$@" < /dev/tty' emacs -nw --init-directory=~/.emacs.d/resources/pager "$t"
rm -f $t
