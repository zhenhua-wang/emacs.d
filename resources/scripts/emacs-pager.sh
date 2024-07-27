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

sh -c 'emacs "$@" < /dev/tty' emacs -Q -nw --eval="
(progn
  (setq user-emacs-directory (expand-file-name \"~/.cache/emacs/\"))
  (push \"~/.emacs.d/module\" load-path)
  (menu-bar-mode -1)
  (load-theme 'modus-vivendi t)
  (require 'zw-package)
  (use-package kkp
    :init (global-kkp-mode 1))
  (bind-keys
    (\"q\" . kill-emacs)
    (\"s-q\" . kill-emacs)
    (\"<escape>\" . kill-emacs)
    (\"s-f\" . isearch-forward)
    :map isearch-mode-map
    (\"<escape>\" . kill-emacs)
    (\"s-f\" . isearch-repeat-forward))
  (setq isearch-wrap-pause 'no)
  (add-hook 'find-file-hook (lambda ()
                              (read-only-mode 1)
                              (setq-local mode-line-format nil)
                              (call-interactively 'isearch-forward)))
)" "$t"
rm -f $t
