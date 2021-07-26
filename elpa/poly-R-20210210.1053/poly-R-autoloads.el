;;; poly-R-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "poly-R" "poly-R.el" (0 0 0 0))
;;; Generated autoloads from poly-R.el

(define-obsolete-function-alias 'poly-noweb+R-mode 'poly-noweb+r-mode "v0.2")

(define-obsolete-variable-alias 'pm-poly/noweb+R 'poly-noweb+r-polymode "v0.2")
 (autoload 'poly-noweb+r-mode "poly-R")

(define-obsolete-function-alias 'poly-markdown+R-mode 'poly-markdown+r-mode "v0.2")

(define-obsolete-variable-alias 'poly-markdown+R-mode-map 'poly-markdown+r-mode-map "v0.2")
 (autoload 'poly-markdown+r-mode "poly-R")
 (autoload 'poly-rapport-mode "poly-R")

(define-obsolete-function-alias 'poly-html+R-mode 'poly-html+r-mode "v0.2")
 (autoload 'poly-html+r-mode "poly-R")

(define-obsolete-function-alias 'poly-brew+R-mode 'poly-brew+r-mode "v0.2")
 (autoload 'poly-brew+r-mode "poly-R")

(define-obsolete-function-alias 'poly-R+C++-mode 'poly-r+c++-mode "v0.2")
 (autoload 'poly-r+c++-mode "poly-R")

(define-obsolete-function-alias 'poly-C++R-mode 'poly-c++r-mode "v0.2")
 (autoload 'poly-c++r-mode "poly-R")
 (autoload 'poly-r-help-examples-mode "poly-R")
 (autoload 'poly-rd-mode "poly-R")

(add-to-list 'auto-mode-alist '("\\.Snw\\'" . poly-noweb+r-mode))

(add-to-list 'auto-mode-alist '("\\.[rR]nw\\'" . poly-noweb+r-mode))

(add-to-list 'auto-mode-alist '("\\.[rR]md\\'" . poly-markdown+r-mode))

(add-to-list 'auto-mode-alist '("\\.rapport\\'" . poly-rapport-mode))

(add-to-list 'auto-mode-alist '("\\.[rR]html\\'" . poly-html+r-mode))

(add-to-list 'auto-mode-alist '("\\.[rR]brew\\'" . poly-brew+r-mode))

(add-to-list 'auto-mode-alist '("\\.[Rr]cpp\\'" . poly-r+c++-mode))

(add-to-list 'auto-mode-alist '("\\.cpp[rR]\\'" . poly-c++r-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "poly-R" '("poly-" "pm--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; poly-R-autoloads.el ends here
