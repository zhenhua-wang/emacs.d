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

sh -c 'emacs "$@" < /dev/tty' emacs -Q -nw --eval="(progn (setq user-emacs-directory (expand-file-name \"~/.cache/emacs/\")) (push \"~/.emacs.d/module\" load-path) (menu-bar-mode -1) (load-theme (quote modus-vivendi) t) (require 'zw-compat) (require 'zw-package) (require 'zw-startup) (global-set-key (kbd \"q\") (quote kill-emacs)) (global-set-key (kbd \"<escape>\") (quote kill-emacs)) (define-key isearch-mode-map (kbd \"<escape>\") (quote kill-emacs)) (global-set-key (kbd \"s-f\") (quote isearch-forward)) (define-key isearch-mode-map (kbd \"s-f\") (quote isearch-repeat-forward)) (setq isearch-wrap-pause 'no) (add-hook (quote find-file-hook) (lambda () (read-only-mode 1) (setq-local mode-line-format nil) (call-interactively (quote isearch-forward)))))" "$t"
rm -f $t
