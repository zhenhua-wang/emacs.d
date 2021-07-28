(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Load path for manually installed packages
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; load my theme
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(require 'init-usepackage)

;; *************** my customization ***********************
(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))

(add-to-list 'default-frame-alist
             '(ns-appearance . light)) ;; or dark - depending on your theme

(require 'init-benchmarking) ;; Measure startup time

(require 'init-gui)

(require 'init-dired)

(require 'init-completion)

(require 'init-ivy)

(require 'init-org)

(require 'init-eshell)

(require 'init-IDE)

;; load my customization

;; load some random packages
(load-file "~/.emacs.d/some-packages/htmlize.el")


;; Some key-bindings
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
