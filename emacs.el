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

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; (require 'init-benchmarking)

(server-start)

(use-package spacegray-theme)
(use-package doom-themes)
(use-package gruvbox-theme)
(use-package nord-theme)
(use-package nano-theme)
(use-package bespoke-themes
  :straight (:host github :repo "mclear-tools/bespoke-themes" :branch "main")
  :config
  ;; Set use of italics
  (setq bespoke-set-italic-comments t
        bespoke-set-italic-keywords t)
  ;; Set variable pitch
  (setq bespoke-set-variable-pitch t)
  ;; Set initial theme variant
  (setq bespoke-set-theme 'light)
  ;; remove taskbar box
  (set-face-attribute 'tab-bar-tab nil
                      :box nil))

;; Load theme
(load-theme 'bespoke t)

;; (load-theme 'nano-light t)
;; (set-face-attribute 'mode-line nil
;;                     :background "#eceff1"
;;                     :box nil)
;; (set-face-attribute 'mode-line-inactive nil
;;                     :background "#eceff1"
;;                     :box nil)

(pcase system-type
  ((or 'gnu/linux 'windows-nt 'cygwin)
   (setq zw/font-size 140))
  ('darwin
   (setq zw/font-size 140)))

;; set the default face
(setq zw/default-font "FiraMono Nerd Font")

(set-face-attribute 'default nil
                       :font zw/default-font
                       ;; make fonts less tranparent
                       :weight 'medium
                       :height zw/font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrainsMono Nerd Font"
                    :weight 'normal
                    :height zw/font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    :font "Iosevka Aile" ;"Cantarell"
                    :weight 'light
                    :height zw/font-size)

(use-package bespoke-modeline
  :straight (:type git :host github :repo "mclear-tools/bespoke-modeline")
  :custom
  (bespoke-modeline-space-top 0)
  (bespoke-modeline-space-bottom 0)
  :init
  ;; Set header line
  (setq bespoke-modeline-position 'bottom)
  ;; Set mode-line height
  (setq bespoke-modeline-size 3)
  ;; Show diff lines in mode-line
  (setq bespoke-modeline-git-diff-mode-line t)
  ;; Set mode-line cleaner
  (setq bespoke-modeline-cleaner t)
  ;; Use mode-line visual bell
  (setq bespoke-modeline-visual-bell nil)
  ;; Set vc symbol
  (setq  bespoke-modeline-vc-symbol "G:")
  :config
  ;; (set-face-attribute 'mode-line nil :height 120)
  ;; (set-face-attribute 'mode-line-inactive nil :height 120)
  (bespoke-modeline-mode))

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :disabled
  :hook (after-init . doom-modeline-init)
  :custom
  (doom-modeline-height 10)
  ;; (doom-modeline-bar-width 5)
  (doom-modeline-lsp t)
  (doom-modeline-github t)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc t)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon t)
  :config
  (setq display-time-format "%a %I:%M %p %D"
        display-time-default-load-average nil)
  (doom-modeline-mode 1)
  (set-face-attribute 'mode-line nil :height 120)
  (set-face-attribute 'mode-line-inactive nil :height 120)
  (pcase system-type
    ('darwin
     (progn
       (display-battery-mode)
       (display-time-mode)))))

(use-package emacs
  :config
  (tab-bar-mode 1)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-truncated)
  (setq tab-bar-new-tab-choice "*scratch*")
  (setq tab-bar-close-button
        (propertize "  "
                    'close-tab t
                    :help "Click to close tab"))
  (setq tab-bar-new-button 
        (propertize "  "
                    'new-tab t
                    :help "Click to create tab"))
  (global-set-key (kbd "s-1") (lambda () (interactive) (tab-select 1)))
  (global-set-key (kbd "s-2") (lambda () (interactive) (tab-select 2)))
  (global-set-key (kbd "s-3") (lambda () (interactive) (tab-select 3)))
  (global-set-key (kbd "s-4") (lambda () (interactive) (tab-select 4)))
  (global-set-key (kbd "s-5") (lambda () (interactive) (tab-select 5)))
  (global-set-key (kbd "s-n") 'tab-new)
  ;; (global-set-key (kbd "s-d") 'tab-close)
  (set-face-attribute 'tab-bar-tab nil
                      ;; :background (face-background 'mode-line)
                      :background (face-background 'default)
                      :underline "#950b96"
                      :font zw/default-font)
  (set-face-attribute 'tab-bar-tab-inactive nil
                      :background (face-background 'default)
                      :underline nil
                      :font zw/default-font)
  (set-face-background 'tab-bar (face-background 'default)))

;; line number mode
(column-number-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Sets the background of HTML color strings in buffers to be the color mentioned.
(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode)
  (text-mode . rainbow-mode))

(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (set-face-foreground 'show-paren-match "#BF616A") ;; set matched color red
  (show-paren-mode 1))

(defun zw/toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 85) '(100 . 100)))))

(use-package good-scroll
  :disabled
  :init
  (good-scroll-mode 1))

;; Dim inactive windows
(use-package dimmer
  :disabled
  :hook (after-init . dimmer-mode)
  :custom
  (dimmer-fraction 0.3)
  (dimmer-adjustment-mode :background)
  (dimmer-use-colorspace :rgb)
  (dimmer-watch-frame-focus-events nil)
  :config
  (fringe-mode 0)
  (dimmer-configure-which-key)
  (dimmer-configure-magit))

(use-package auto-dim-other-buffers
  :init (auto-dim-other-buffers-mode)
  :config
  (fringe-mode 0))

(use-package emacs
  :hook
  (text-mode . visual-line-mode)
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
  ;; Revert buffers when the underlying file has changed
  (global-auto-revert-mode 1)
  ;; hightlight current row
  (global-hl-line-mode t)
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
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
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
  ;; -------------- disable mouse and trackboard ----------------
  ;; (global-unset-key (kbd "<down-mouse-1>"))
  ;; (global-unset-key (kbd "<mouse-1>"))
  ;; (global-unset-key (kbd "<down-mouse-3>"))
  ;; (global-unset-key (kbd "<mouse-3>"))
  ;; (mouse-wheel-mode -1)
  ;; (global-set-key [wheel-down] 'ignore)
  ;; (global-set-key [double-wheel-up] 'ignore)
  ;; (global-set-key [double-wheel-down] 'ignore)
  ;; (global-set-key [triple-wheel-up] 'ignore)
  ;; (global-set-key [triple-wheel-down] 'ignore))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Set default connection mode to SSH
(setq tramp-default-method "ssh")

(use-package vterm
  :ensure t
  :bind
  ((:map vterm-copy-mode-map
         ("<return>" . vterm-copy-mode))
   (:map vterm-mode-map
         ("s-e" . delete-window))))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-quit-no-match nil)
  (corfu-on-exact-match nil)
  (corfu-preview-current nil)
  (corfu-echo-documentation nil)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)  ;; Always have the same width

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

;; Use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)
    :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Add extensions
(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

;; ivy
(use-package ivy
  :diminish
  :bind (("s-f" . swiper)
         ("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ;; override s-tab from creating another minibuffer and make it behave mac-like
         ("s-<tab>" . ivy-next-line) ; "C-j"
         ;; ("s-SPC" . ivy-next-line)
         ("<backtab>" . ivy-previous-line))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  (setq confirm-nonexistent-file-or-buffer t)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . switch-to-buffer)
	 ("C-x C-f" . counsel-find-file)
	 ;; ("C-c b" . counsel-switch-buffer)
	 ("C-c i" . counsel-imenu)
         ("C-c l" . 'counsel-search)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (pcase system-type
    ('darwin
     (setq browse-url-browser-function 'xwidget-webkit-browse-url))
    ('gnu/linux
     (setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser")))
  (setq counsel-search-engine 'google)
  (counsel-mode 1))

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :after ivy
  :defer 1
  :init
  (setq ivy-flx-limit 10000))

;; precscient
(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1)
  :custom
  (setq ivy-prescient-enable-filtering t)
  )

(use-package prescient
  :after counsel
  :config
  (prescient-persist-mode 1)
  (setq prescient-sort-length-enable t)
  ;; (setq prescient-history-length 20)
  )

(use-package all-the-icons-ivy
  :hook
  (after-init . all-the-icons-ivy-setup)
  :config
  (setq all-the-icons-ivy-file-commands
        '(counsel-find-file counsel-recentf counsel-ibuffer counsel-switch-buffer)))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1)
  :config
  (setq all-the-icons-ivy-rich-color-icon t))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :after counsel
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     ((ivy-rich-candidate (:width 40))
                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
                      (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
                     :predicate
                     (lambda (cand)
                       (if-let ((buffer (get-buffer cand)))
                           ;; Don't mess with EXWM buffers
                           (with-current-buffer buffer
                             (not (derived-mode-p 'exwm-mode)))))))))

(use-package projectile
  :config (projectile-mode +1)
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Workspace/Documents/Graduate/Mizzou")
    (setq projectile-project-search-path '("~/Workspace/Documents/Graduate/Mizzou"))))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode))

(use-package openwith
  :if (eq system-type 'gnu/linux)
  :config
  (setq openwith-associations
        (list
         ;; (list (openwith-make-extension-regexp
         ;;        '("xbm" "pbm" "pgm" "ppm" "pnm"
         ;;          "png" "gif" "bmp" "tif" "jpeg" "jpg"))
         ;;       "feh"
         ;;       '(file))
         ;;I promise I will get rid of this someday..
         (list (openwith-make-extension-regexp
                '("doc" "docx" "ppt" "pptx"))
               "libreoffice"
               '(file))
         (list (openwith-make-extension-regexp
                '("pdf"))
               ;; "zathura"
               ;; "okular"
               "evince"
               ;; "PDF Tools"
               '(file))))
  (openwith-mode 1))

(use-package which-key
  :defer 1
  :init
  :diminish which-key-mode
  :config
  ;; (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package neotree
  :commands neotree)

;; check word spelling
(use-package flyspell
  :init
  (flyspell-mode 1)
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-list-command "--list"))

;; check code syntax
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  )

(use-package winner
  :config
  (winner-mode))

;; set preference to horizontal split
(defun split-window-sensibly-prefer-horizontal (&optional window)
  "Based on split-window-sensibly, but designed to prefer a horizontal split,
i.e. windows tiled side-by-side."
  (interactive)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically
             (with-selected-window window
               (split-window-below)))
        (and
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
         (not (window-minibuffer-p window))
         (let ((split-width-threshold 0))
           (when (window-splittable-p window t)
             (with-selected-window window
               (split-window-right))))))))

(setq split-width-threshold  80
      split-height-threshold 80
      xsplit-window-preferred-function 'split-window-sensibly-prefer-horizontal
      )

(use-package popper
  :bind (("s-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("s-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("[Oo]utput\\*$"
          "^\\*Warnings\\*"
          "^\\*Compile-Log\\*"
          "^\\*Messages\\*"
          "^\\*Backtrace\\*"
          "^\\*ielm\\*"
          "^\\*Tex Help\\*"
          "^\\*Shell Command Output\\*"
          "^\\*Async Shell Command\\*"
          "^\\*WordNut\\*"
          "^\\*help[R].*"
          "^\\*polymode export\\*"
          help-mode
          eshell-mode
          message-mode
          compilation-mode))
  ;; only show the popper in the same project
  ;; (setq popper-group-function #'popper-group-by-project)
  ;; (popper-mode -1)
  (popper-mode +1))

(setq display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

(setq display-buffer-alist
      '(;; top side window
        ("\\*\\(Flymake\\|Package-Lint\\|vc-git :\\).*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 0))
        ("\\*Messages.*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 1))
        ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 2))
        ("\\*polymode export.*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 1))
        ;; right side window
        ("\\*[Hh]elp.*"            ; See the hooks for `visual-line-mode'
         (display-buffer-in-side-window)
         (window-width . 0.5)
         (side . right)
         (slot . -1))
        ("\\*eglot doc.*"
         (display-buffer-in-side-window)
         (window-width . 0.5)
         (side . right)
         (slot . -1))
        ("\\*\\(R\\|Python\\).*"
         (display-buffer-reuse-mode-window display-buffer-in-side-window)
         (side . right)
         (slot . -1)
         (window-width . 0.3))
        ;; bottom buffer (NOT side window)
        ("\\*.*\\(e?shell\\|v?term\\).*"
         ;; (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (display-buffer-in-side-window)
         (window-height . 0.15)
         (side . bottom))
        ;; ("\\*R.*"
        ;;  (display-buffer-reuse-mode-window display-buffer-at-bottom)
        ;;  (window-height . 0.3))
        ;; below current window
        ("\\*Calendar.*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . shrink-window-if-larger-than-buffer))))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(use-package sudo-edit
  :commands (sudo-edit))

(when (eq system-type 'gnu/linux)
  (org-babel-load-file "~/.emacs.d/emacs-desktop.org"))

(org-babel-load-file "~/.emacs.d/emacs-development.org")

(org-babel-load-file "~/.emacs.d/emacs-text.org")

(when (eq system-type 'gnu/linux)
  (org-babel-load-file "~/.emacs.d/emacs-system.org"))
