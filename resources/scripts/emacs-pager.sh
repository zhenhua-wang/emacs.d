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

sh -c 'emacs "$@" < /dev/tty' emacs -Q -nw --eval="(progn (menu-bar-mode -1) (load-theme (quote modus-vivendi) t) (global-set-key (kbd \"q\") (quote kill-emacs)) (add-hook (quote find-file-hook) (lambda () (read-only-mode 1) (setq-local mode-line-format nil) (call-interactively (quote isearch-forward)))))" "$t"
rm -f $t
