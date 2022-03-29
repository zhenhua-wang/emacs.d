;; Load path for manually installed packages
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load path for customied themes
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-always-ensure t)
(setq use-package-verbose t)

(require 'use-package)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering
  :if (or (eq system-type 'gnu/linux) (eq system-type 'darwin)))

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
          (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(use-package emacs
  :custom
  ;; completion
  (completion-cycle-threshold nil)
  (tab-always-indent 'complete)
  (completions-detailed t)
  (completion-ignore-case t)
  ;; Revert Dired and other buffers
  (global-auto-revert-non-file-buffers t)
  ;; Use spaces instead of tabs for indentation
  (indent-tabs-mode nil)
  ;; echo area show only 1 line of doc
  (eldoc-echo-area-use-multiline-p nil)
  ;; fix minibuffer size
  (resize-mini-windows nil)
  :init
  ;; ------------------- simplify yes no ---------------
  (defun yes-or-no-p->-y-or-n-p (orig-fun &rest r)
    (cl-letf (((symbol-function 'yes-or-no-p) #'y-or-n-p))
      (apply orig-fun r)))
  (advice-add 'kill-buffer :around #'yes-or-no-p->-y-or-n-p)
  ;; ------------------- modes    ---------------------
  (global-visual-line-mode 1)
  ;; Revert buffers when the underlying file has changed
  (global-auto-revert-mode 1)
  ;; hightlight current row
  (global-hl-line-mode 1)
  ;; ------------------- key bind ---------------------
  ;; Keybonds
  (global-set-key (kbd "s-z") 'undo)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-c") 'kill-ring-save)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "s-l") 'goto-line)
  (global-set-key (kbd "s-q") 'kill-current-buffer)
  ;; vterm
  (global-set-key (kbd "s-e") 'vterm)
  ;; eldoc
  ;; (global-set-key (kbd "s-d") 'eldoc-doc-buffer)
  ;; winner undo/redo
  (global-set-key (kbd "s-u") 'winner-undo)
  (global-set-key (kbd "s-U") 'winner-redo)
  ;; projectile find file
  (global-set-key (kbd "s-p") 'counsel-projectile-switch-project)
  ;; Make ESC quit prompts
  ;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (global-set-key (kbd "<escape>") (kbd "C-g"))
  ;; window operations
  (global-set-key (kbd "s-w") 'delete-window)
  (global-set-key (kbd "s-t") 'split-window-sensibly-prefer-horizontal)
  (global-set-key [s-left] 'windmove-left)          ; move to left window
  (global-set-key [s-right] 'windmove-right)        ; move to right window
  (global-set-key [s-up] 'windmove-up)              ; move to upper window
  (global-set-key [s-down] 'windmove-down)          ; move to lower window
  ;; check dict
  (global-set-key (kbd "C-c w") 'wordnut-search)
  (global-set-key (kbd "C-c W") 'wordnut-lookup-current-word)
  ;; toggle transparency
  (global-set-key (kbd "C-c t") 'zw/toggle-transparency)
  ;; get passwed
  ;; (global-set-key (kbd "C-c p") 'zw/get-passwd)
  ;; toggle input
  (global-set-key (kbd "C-\\") 'toggle-input-method)
  ;; consistent with EXWM
  (pcase system-type
    ('darwin
     (progn
       (setq mac-command-modifier 'super)
       (setq mac-option-modifier 'meta)))))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-preselect-first nil)
  (corfu-quit-no-match nil)
  (corfu-on-exact-match 'insert)
  (corfu-preview-current nil)
  (corfu-echo-documentation nil)
  (corfu-scroll-margin 5)
  (corfu-min-width 20)
  (corfu-max-width 80)

  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
	("TAB" . corfu-insert)
	([tab] . corfu-insert)
	([escape] . corfu-quit)
	([return] . corfu-insert)
	("M-d" . corfu-show-documentation)
	("M-l" . corfu-show-location))
  :init
  (corfu-global-mode)
  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

;; Add extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))
