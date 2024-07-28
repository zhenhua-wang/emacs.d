#!/bin/bash

# Call emacsclient as a pager.
# First, read in the content of stdin into a temporary file

t=$(mktemp /tmp/emacs-pager.XXXXXX) || exit 1

echo "Reading into emacs..."

# Remove terminal escape sequences, empty lines and last character (not to be truncated by emacs)
cat - \
    | sed 's/\x1b\][0-9;:]*[AC]\x1b\\//g' \
    | sed '/^$/d' \
    | sed 's/.$//' \
          >> $t

sh -c 'emacs "$@" < /dev/tty' emacs -nw --init-directory=~/.emacs.d/resources/pager "$t"
rm -f $t
