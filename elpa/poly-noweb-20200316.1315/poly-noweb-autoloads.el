;;; poly-noweb-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "poly-noweb" "poly-noweb.el" (0 0 0 0))
;;; Generated autoloads from poly-noweb.el
 (autoload 'poly-noweb-mode "poly-noweb")

(add-to-list 'auto-mode-alist '("\\.nw\\'" . poly-noweb-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "poly-noweb" '("noweb-code-mode" "poly-noweb-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; poly-noweb-autoloads.el ends here
