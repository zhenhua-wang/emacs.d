;;; fish-completion-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "fish-completion" "fish-completion.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from fish-completion.el

(autoload 'fish-completion-mode "fish-completion" "\
Turn on/off fish shell completion in all future shells or Eshells.

This is a minor mode.  If called interactively, toggle the
`Fish-Completion mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `fish-completion-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

In `shell', completion is replaced by fish completion.
In `eshell', fish completion is only used when `pcomplete' fails.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "fish-completion" '("fish-completion-" "global-fish-completion-mode" "turn-on-fish-completion-mode"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fish-completion-autoloads.el ends here
