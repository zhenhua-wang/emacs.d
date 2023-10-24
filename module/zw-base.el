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

;; * Global mode
;; modes run after init
(dolist (mode '(window-divider-mode
                blink-cursor-mode
                ;; fringe (nil is default)
                fringe-mode
                ;; warp long line
                global-visual-line-mode
                ;; yank overwrite what is selected
                delete-selection-mode
                ;; record last location in the file
                save-place-mode
                ;; handle large file
                global-so-long-mode
                ;; right click menu
                context-menu-mode))
  (add-hook 'after-init-hook mode))

;; modes disable after init
(add-hook 'after-init-hook
          (lambda ()
            (global-eldoc-mode -1)
            (tooltip-mode -1)))

;; * Global config
(setq-default default-directory (concat (getenv "HOME") "/")
              confirm-kill-emacs 'yes-or-no-p
              use-dialog-box nil
              visible-bell t
              cursor-type '(bar . 2)
              cursor-in-non-selected-windows nil
              ;; use spaces for indent
              indent-tabs-mode nil
              ;; save clipboard before kill ring
              save-interprogram-paste-before-kill t
              select-enable-clipboard t
              ;; copy while draging mouse
              mouse-drag-copy-region t)
;; default coding
(set-default-coding-systems 'utf-8)

;; * Global keymap
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
           ("s-{" . shrink-window-horizontally)
           ("s-}" . enlarge-window-horizontally)
           ("s-^" . enlarge-window)
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
           ("s-f" . isearch-repeat-forward)
           :map prog-mode-map
           ("<tab>" . zw/smart-tab))

;; * Misc
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

;; * Provide
(provide 'zw-base)
