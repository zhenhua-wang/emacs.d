;; Load path for manually installed packages
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load path for customied themes
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)
    ))

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

(global-visual-line-mode 1)
;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
;; hightlight current row
(global-hl-line-mode 1)

;; save clipboard before kill ring
(setq save-interprogram-paste-before-kill t)
;; completion
(setq completion-cycle-threshold nil)
(setq tab-always-indent t)
(setq completions-detailed t)
(setq completion-ignore-case t)
;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)
;; Use spaces instead of tabs for indentation
(setq indent-tabs-mode nil)
;; simplify yes no
;; (defalias 'yes-or-no-p 'y-or-n-p)
;; set fringe to 0
(fringe-mode 0)

;; ------------------- key bind ---------------------
;; Keybinds
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-l") 'goto-line)
(global-set-key (kbd "s-q") 'kill-current-buffer)
(global-set-key (kbd "s-f") 'isearch-forward)
(define-key isearch-mode-map (kbd "s-f") 'isearch-repeat-forward)
(global-set-key (kbd "<C-tab>") 'completion-at-point)
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
;; ivy bib
(global-set-key (kbd "C-c b") 'ivy-bibtex)
;; get passwed
;; (global-set-key (kbd "C-c p") 'zw/get-passwd)
;; toggle input
(global-set-key (kbd "C-\\") 'toggle-input-method)
;; consistent with EXWM
(pcase system-type
  ('darwin
   (progn
     (setq mac-command-modifier 'super)
     (setq mac-option-modifier 'meta))))

(use-package all-the-icons
  :if (display-graphic-p))

;; (use-package spacegray-theme)
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))
;; (use-package gruvbox-theme)
;; (use-package nord-theme)
;; (use-package nano-theme)
(use-package bespoke-themes
  :straight (:host github :repo "mclear-tools/bespoke-themes" :branch "main")
  :config
  ;; Set evil cursor colors
  (setq bespoke-set-evil-cursors t)
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
(load-theme 'doom-nord-light t)

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
  :disabled
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
  (display-time-format "%a %I:%M %p %D")
  (display-time-default-load-average nil)
  :config
  (doom-modeline-mode 1)
  (display-time-mode)
  (set-face-attribute 'mode-line nil :height 120)
  (set-face-attribute 'mode-line-inactive nil :height 120)
  (pcase system-type
    ('darwin
     (progn
       (display-battery-mode)))))

(tab-bar-mode 1)
(setq tab-bar-tab-name-function 'tab-bar-tab-name-truncated)
(setq tab-bar-new-tab-choice "*scratch*")
(setq tab-bar-new-button-show nil)
(setq tab-bar-close-button-show nil)
(global-set-key (kbd "s-1") (lambda () (interactive) (tab-select 1)))
(global-set-key (kbd "s-2") (lambda () (interactive) (tab-select 2)))
(global-set-key (kbd "s-3") (lambda () (interactive) (tab-select 3)))
(global-set-key (kbd "s-4") (lambda () (interactive) (tab-select 4)))
(global-set-key (kbd "s-5") (lambda () (interactive) (tab-select 5)))
(global-set-key (kbd "s-n") 'tab-new)
;; (global-set-key (kbd "s-d") 'tab-close)
(set-face-attribute 'tab-bar-tab nil
                    ;; :background (face-background 'mode-line)
		    :foreground (face-foreground 'default)
                    :background (face-background 'default)
                    :underline "#950b96"
                    :font zw/default-font)
(set-face-attribute 'tab-bar-tab-inactive nil
		    :foreground (face-foreground 'default)
                    :background (face-background 'default)
                    :underline nil
                    :font zw/default-font)
(set-face-background 'tab-bar (face-background 'default))

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
  (set-face-attribute 'show-paren-match nil :background (face-foreground 'default))
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (set-face-foreground 'show-paren-match "red")
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
  ;; :config
  ;; (fringe-mode 0)
  :init (auto-dim-other-buffers-mode))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Set default connection mode to SSH
(setq tramp-default-method "ssh")

(use-package vterm
  :straight `(:pre-build (("rm" "-fr" "build")
			  ("mkdir" "build")
			  ("bash" "-c" "cd \"$1\" && cmake .. && make" "--"  ,(concat (straight--repos-dir "emacs-libvterm") "build"))
			  ;;or
			  ;; (shell-command "rm -fr build && mkdir build && cd $_ && cmake .. && make")
			  ))
  :bind
  ((:map vterm-copy-mode-map
         ("<return>" . vterm-copy-mode))
   (:map vterm-mode-map
         ("s-e" . delete-window))))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-preselect-first t)
  (corfu-quit-no-match t)
  (corfu-on-exact-match 'insert)
  (corfu-preview-current nil)
  (corfu-echo-documentation nil)
  (corfu-scroll-margin 5)
  (corfu-min-width 20)
  (corfu-max-width 80)
  :bind
  (:map corfu-map
	("TAB" . corfu-insert)
        ([tab] . corfu-insert)
        ([escape] . corfu-quit)
        ([return] . corfu-insert)
        ("M-d" . corfu-show-documentation)
        ("M-l" . corfu-show-location)
	("SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

(use-package dabbrev
  :custom
  ;; since cape-dabbrev cannot replace case, I will set it to nil for now.
  (dabbrev-case-fold-search nil)
  (dabbrev-case-replace t))

(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons nil)
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu-doc
  :hook
  (corfu-mode . corfu-doc-mode)
  :bind
  (:map corfu-map
        ("M-p" . corfu-doc-scroll-down)
        ("M-n" . corfu-doc-scroll-up)))

;; Add extensions
(use-package cape
  :custom
  (cape-dabbrev-min-length 1)
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; ivy
(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("s-<tab>" . ivy-next-line)
         ("<backtab>" . ivy-previous-line))
  :custom
  (ivy-wrap t)
  (ivy-height 15)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "[%d/%d] ")
  (enable-recursive-minibuffers t)
  (confirm-nonexistent-file-or-buffer t)
  (swiper-use-visual-line nil)
  (swiper-use-visual-line-p (lambda (a) nil))
  :config
  (ivy-mode 1))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1) 
  :custom
  (all-the-icons-ivy-rich-color-icon t))

(use-package ivy-rich
  :after counsel
  :init
  (all-the-icons-ivy-rich-mode 1)
  (ivy-rich-mode 1)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . switch-to-buffer)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c i" . counsel-imenu)
         ("C-c l" . 'counsel-search)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (if (featurep 'xwidget-internal)
      (setq browse-url-browser-function 'xwidget-webkit-browse-url))
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
  (setq ivy-prescient-enable-filtering t))

(use-package prescient
  :after counsel
  :config
  (prescient-persist-mode 1)
  (setq prescient-sort-length-enable t))

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

(use-package which-key
  :defer 1
  :init
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package treemacs
  :commands treemacs)

(use-package treemacs-all-the-icons
  :config
  (treemacs-load-theme "all-the-icons"))

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
         (window-height . 0.1)
         (side . top)
         (slot . 0))
        ("\\*Messages.*"
         (display-buffer-in-side-window)
         (window-height . 0.1)
         (side . top)
         (slot . 1))
        ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.1)
         (side . top)
         (slot . 2))
        ("\\*polymode export.*"
         (display-buffer-in-side-window)
         (window-height . 0.1)
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
         (display-buffer-reuse-mode-window)
         (side . right)
         (slot . -1)
         (window-width . 0.3))
        ;; bottom buffer (NOT side window)
        ("\\*.*\\(e?shell\\|v?term\\).*"
         ;; (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (display-buffer-in-side-window)
         (window-height . 0.2)
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

(use-package yasnippet
  :defer 1
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/yasnippet"))
  (yas-global-mode 1))

(use-package ivy-yasnippet
  :bind
  ("M-<tab>" . ivy-yasnippet))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :bind
  ("s-z" . undo-tree-undo)
  ("s-Z" . undo-tree-redo)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t))

(use-package hydra)

(org-babel-load-file "~/.emacs.d/emacs-programming.org")

(org-babel-load-file "~/.emacs.d/emacs-writing.org")

(when (getenv "WSL_DISTRO_NAME")
  (progn
    (cua-mode 1)
    (global-set-key (kbd "C-{") 'windmove-left)          ; move to left window
    (global-set-key (kbd "C-|") 'windmove-right)        ; move to right window
    (global-set-key (kbd "C-}") 'windmove-up)              ; move to upper window
    (global-set-key (kbd "C-\"") 'windmove-down)          ; move to lower window
    (global-set-key (kbd "M-#") 'winner-undo)
    (global-set-key (kbd "M-*") 'counsel-projectile-switch-project)
    (global-set-key (kbd "C-(") 'delete-window)
    (global-set-key (kbd "C-t") 'split-window-sensibly-prefer-horizontal)
    (global-set-key (kbd "C-!") 'kill-current-buffer)))
