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

(org-babel-load-file "~/.emacs.d/emacs-text.org")

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

(use-package dabbrev
  :custom
  ;; since cape-dabbrev cannot replace case, I will set it to nil for now.
  (dabbrev-case-fold-search nil)
  (dabbrev-case-replace t))

;; Add extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Enable richer annotations using the Marginalia package
  (use-package marginalia
    ;; Either bind `marginalia-cycle` globally or only in the minibuffer
    :bind (("M-A" . marginalia-cycle)
           :map minibuffer-local-map
           ("M-A" . marginalia-cycle))
    :custom
    (marginalia-max-relative-age 0)
    (marginalia-align 'right)
    (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
    ;; The :init configuration is always executed (Not lazy!)
    :init
    ;; Must be in the :init section of use-package such that the mode gets
    ;; enabled right away. Note that this forces loading the package.
    (marginalia-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("s-f" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
)
