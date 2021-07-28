(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
	       '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Load path for manually installed packages
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; load my theme
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))


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
