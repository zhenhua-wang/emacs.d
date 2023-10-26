;; -*- lexical-binding: t -*-

;; * helper
(defun zw/icon-displayable-p ()
  "Return non-nil if icons are displayable."
  (or (display-graphic-p) (daemonp)))

(defun zw/hidden-buffer-p (&optional buffer)
  (string-match "^[[:space:]].*$" (buffer-name buffer)))

(defun zw/side-window-p ()
  "Return non-nil if current window is a side window."
  (window-parameter (get-buffer-window) 'window-side))

(defun zw/merge-list-to-list (dst src &optional prepend)
  "Merges content of the 2nd list with the 1st one"
  (if prepend
      (set dst
           (append src (eval dst)))
    (set dst
         (append (eval dst) src))))

(defun zw/get-face-attr-recur (face attr)
  "Get face background/foreground recursively"
  (let ((face-attr (face-attribute face attr)))
    (if (and face-attr
             (not (eq face-attr 'unspecified)))
        face-attr
      (let ((parent-face (face-attribute face :inherit)))
        (if (and parent-face
                 (not (eq parent-face 'unspecified)))
            (zw/get-face-attr-recur parent-face attr)
          nil)))))

(defun zw/get-face-bg-recur (face)
  "Get face background recursively"
  (zw/get-face-attr-recur face :background))

(defun zw/get-face-fg-recur (face)
  "Get face foreground recursively"
  (zw/get-face-attr-recur face :foreground))

(defun zw/translate-shift-number (i)
  "Translate S-i to character."
  (pcase i
    (1 "!") (2 "@") (3 "#") (4 "$")
    (5 "%") (6 "^") (7 "&") (8 "*")
    (9 "(") (0 ")")))

;; keep track active UI
(defvar zw/active-frame nil)
(defvar zw/active-window nil)
(defun zw/update-active-ui (&rest arg)
  "Update active UI."
  (let ((frame (selected-frame))
        (window (selected-window)))
    (unless (minibufferp (window-buffer window))
      (setq zw/active-frame frame))
    (setq zw/active-window (selected-window))))
(add-hook 'window-selection-change-functions #'zw/update-active-ui)

;; * Config
(setq-default default-directory (concat (getenv "HOME") "/")
              confirm-kill-emacs 'yes-or-no-p)

;; default coding
(set-default-coding-systems 'utf-8)

;; disable saving for buffers not visiting a file
(defadvice save-buffer (around interactive-no-visited-file-name activate)
  "When called interactively, disable for buffers not visiting a file."
  (when (or (not (called-interactively-p 'any))
            buffer-file-name)
    ad-do-it))
;; make scratch and dashboard unkillable
(add-hook 'kill-buffer-query-functions #'zw/dont-kill-scratch)
(defun zw/dont-kill-scratch ()
  (if (not (or (equal (buffer-name) "*scratch*")
               (equal (buffer-name) "*dashboard*")))
      t
    ;; (message "Not allowed to kill %s, burying instead" (buffer-name))
    (bury-buffer)
    nil))

;; * Appearance
;; ** UI
(dolist (mode '(window-divider-mode
                blink-cursor-mode
                ;; fringe (nil is default)
                fringe-mode
                ;; right click menu
                context-menu-mode))
  (add-hook 'after-init-hook mode))

(setq-default use-dialog-box nil
              visible-bell t
              cursor-type '(bar . 2)
              cursor-in-non-selected-windows nil
              indent-tabs-mode nil
              enable-recursive-minibuffers t)

;; ** Buffer face mode
;; Set fixed-font faces for prog
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local buffer-face-mode-face 'fixed-pitch)
            (buffer-face-mode)))

;; ** Line number mode
;; line number mode
(dolist (mode '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook mode 'display-line-numbers-mode))
;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook markdown-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; ** Highlight line
(add-hook 'after-init-hook 'global-hl-line-mode)
(dolist (mode '(eshell-mode-hook shell-mode-hook term-mode-hook vterm-mode-hook))
  (add-hook mode (lambda () (setq-local global-hl-line-mode nil))))

;; ** Warp long line
(add-hook 'after-init-hook 'global-visual-line-mode)

;; ** So long
(add-hook 'after-init-hook 'global-so-long-mode)
;; HACK: disable bidi for long lines
(setq-default bidi-display-reordering nil
              bidi-inhibit-bpa t
              long-line-threshold 1000
              large-hscroll-threshold 1000
              syntax-wholeline-max 1000)

;; ** Whitespace
(dolist (mode '(prog-mode-hook
                text-mode-hook))
  (add-hook mode (lambda ()
                   (if buffer-file-name
                       (setq-local show-trailing-whitespace t)))))

;; ** Paren
(add-hook 'after-init-hook 'show-paren-mode)
(setq show-paren-when-point-inside-paren nil
      show-paren-when-point-in-periphery nil
      show-paren-context-when-offscreen 'child-frame)
(add-to-list 'show-paren--context-child-frame-parameters '(child-frame-border-width . 4))

;; * Tool
;; ** Comint
;; Make processes’ outputs read-only.
(setq comint-prompt-read-only t
      comint-scroll-to-bottom-on-input t
      comint-scroll-to-bottom-on-output nil
      comint-move-point-for-output nil)

;; ** Recentf
(add-hook 'after-init-hook 'recentf-mode)
(setq recentf-max-saved-items 300
      recentf-exclude
      '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
        "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
        "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
        "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/" "~/.emacs.d/straight/"
        no-littering-var-directory no-littering-etc-directory
        (lambda (file) (file-in-directory-p file package-user-dir))))
(with-eval-after-load "recentf"
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
  ;; save recentf-list before closing frame
  (advice-add 'save-buffers-kill-terminal :before 'recentf-save-list))

;; ** Savehist
(add-hook 'after-init-hook 'savehist-mode)

;; ** Scroll
(setq scroll-step 0
      scroll-margin 1
      scroll-conservatively 101
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2
      mouse-wheel-progressive-speed nil
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t)
  (setq touch-screen-precision-scroll t)
  (bind-keys :map pixel-scroll-precision-mode-map
             ("<prior>" . nil)
             ("<next>" . nil)))

;; ** Save place
(add-hook 'after-init-hook 'save-place-mode)

;; ** Open address
(add-hook 'text-mode-hook 'goto-address-mode)
(add-hook 'prog-mode-hook 'goto-address-prog-mode)

;; ** Isearch
(setq isearch-lazy-count t
      lazy-count-prefix-format "%s/%s ")

;; ** Winner mode
(add-hook 'after-init-hook 'winner-mode)
(setq winner-dont-bind-my-keys t)

;; * Editor
;; ** Copy
(setq-default
 ;; save clipboard before kill ring
 save-interprogram-paste-before-kill t
 select-enable-clipboard t
 ;; copy while draging mouse
 mouse-drag-copy-region t)

;; ** Auto revert
(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      revert-buffer-quick-short-answers t)

;; ** Delete selection
(add-hook 'after-init-hook 'delete-selection-mode)

;; * Disabled
(add-hook 'after-init-hook (lambda () (global-eldoc-mode -1)))

;; * Keymap
;; modifiers
(pcase system-type
  ('darwin
   (progn
     (setq mac-right-command-modifier 'hyper)
     (setq mac-command-modifier 'super)
     (setq mac-option-modifier 'meta))))

;; keys
(bind-keys :map global-map
           ("<escape>" . keyboard-quit) ;keyboard-escape-quit
           ("<f5>" . zw/update-emacs-tangle-dotfiles)
           ("<f11>" . zw/toggle-presentation)
           ;; disable some weird keys
           ("C-z" . nil)
           ("<prior>" . nil)
           ("<next>" . nil)
           ("<home>" . nil)
           ("<end>" . nil)
           ;; disable scroll zoom
           ("C-<mouse-4>" . nil)
           ("C-<mouse-5>" . nil)
           ("C-<wheel-down>" . nil)
           ("C-<wheel-up>" . nil)
           ;; completion
           ("<C-tab>" . completion-at-point)
           ;; editing
           ("s-<backspace>" . (lambda () (interactive) (kill-line 0)))
           ("s-z" . undo)
           ("s-x" . kill-region)
           ("s-c" . kill-ring-save)
           ("s-v" . yank)
           ("s-a" . mark-whole-buffer)
           ("s-s" . save-buffer)
           ("s-S" . write-file)
           ;; term/shell
           ("s-e" . vterm)
           ;; buffer operations
           ("s-r" . revert-buffer-quick)
           ("s-q" . kill-current-buffer)
           ("s-=" . text-scale-increase)
           ("s--" . text-scale-decrease)
           ;; window operations
           ("s-w" . delete-window)
           ("s-t" . zw/split-window-sensibly-prefer-horizontal)
           ("s-<left>" . windmove-left)
           ("s-<right>" . windmove-right)
           ("s-<up>" . windmove-up)
           ("s-<down>" . windmove-down)
           ("s-+" . enlarge-window-horizontally)
           ("s-_" . shrink-window-horizontally)
           ("s-^" . enlarge-window)
           ("s-u" . winner-undo)
           ("s-U" . winner-redo)
           ("s-B" . zw/side-window-toggle)
           ("C-x 1" . zw/maximize-window)
           ;; misc commands
           ("s-i" . zw/show-info)
           ("s-o" . zw/open-in-external)
           ("s-h" . display-local-help)
           ("s-\\" . toggle-input-method)
           ;; minibuffer
           :map minibuffer-mode-map
           ("<escape>" . minibuffer-keyboard-quit)
           :map isearch-mode-map
           ([remap isearch-delete-char] . isearch-del-char)
           ("s-f" . isearch-repeat-forward)
           :map prog-mode-map
           ("<tab>" . zw/smart-tab))

;; * Provide
(provide 'zw-base)
