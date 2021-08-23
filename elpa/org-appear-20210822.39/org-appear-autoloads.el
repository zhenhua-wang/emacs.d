;;; org-appear-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-appear" "org-appear.el" (0 0 0 0))
;;; Generated autoloads from org-appear.el

(autoload 'org-appear-mode "org-appear" "\
A minor mode that automatically toggles elements in Org mode.

If called interactively, enable Org-Appear mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-appear" '("org-appear-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-appear-autoloads.el ends here
