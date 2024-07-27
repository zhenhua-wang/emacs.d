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
  (use-package clipetty
    :bind (\"C-c\" . kill-ring-save))
  (bind-keys
    (\"s-q\" . kill-emacs)
    (\"<escape>\" . kill-emacs)
    (\"s-f\" . isearch-forward)
    :map isearch-mode-map
    (\"<escape>\" . kill-emacs)
    (\"s-f\" . isearch-repeat-forward)
    (\"<down>\" . isearch-repeat-forward)
    (\"<up>\" . isearch-repeat-backward)
    (\"<right>\" . isearch-repeat-forward)
    (\"<left>\" . isearch-repeat-backward))
  (advice-add 'self-insert-command :override
              (lambda (N &optional C)
                (call-interactively 'isearch-forward)
                (isearch-printing-char C N)))
  (setq isearch-lazy-count t
        lazy-count-prefix-format \"%s/%s \"
        isearch-wrap-pause 'no
        visible-bell t)
  (add-hook 'find-file-hook (lambda ()
                              (read-only-mode 1)
                              (global-clipetty-mode 1)
                              (hl-line-mode 1)
                              (setq-local mode-line-format nil)
                              (call-interactively 'isearch-forward)))
)" "$t"
rm -f $t
