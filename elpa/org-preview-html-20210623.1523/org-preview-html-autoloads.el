;;; org-preview-html-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-preview-html" "org-preview-html.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org-preview-html.el

(autoload 'org-preview-html/preview "org-preview-html" "\
Export current 'org-mode' buffer to a temp file and call `eww-open-file' to preview it.

\(fn)" t nil)

(autoload 'org-preview-html-mode "org-preview-html" "\
Preview current org file in eww whenever you save it.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-preview-html" '("org-preview-html/")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-preview-html-autoloads.el ends here
