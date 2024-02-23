;; -*- lexical-binding: t -*-

;; * helper
(defun zw/icon-displayable-p ()
  "Return non-nil if icons are displayable."
  (or (display-graphic-p) (daemonp)))

(defun zw/hidden-buffer-p (&optional buffer)
  (string-match "^[[:space:]].*$" (buffer-name buffer)))

(defun zw/window-side (window)
  "Get window's side, return nil if it is not side window."
  (window-parameter window 'window-side))

(defun zw/merge-list-symbols (dst src &optional prepend)
  "Merge lists, possibly symbols."
  (if prepend
      (set dst
           (append (if (symbolp src) (eval src) src)
                   (if (symbolp dst) (eval dst) dst)))
    (set dst
         (append (if (symbolp dst) (eval dst) dst)
                 (if (symbolp src) (eval src) src)))))

(defun zw/list-same-elements (list1 list2)
  "Test if LIST1 and LIST2 hold the same values.
The order of values may be different."
  (not (cl-set-exclusive-or list1 list2)))

(defun zw/translate-shift-number (i)
  "Translate S-i to character."
  (pcase i
    (1 "!") (2 "@") (3 "#") (4 "$")
    (5 "%") (6 "^") (7 "&") (8 "*")
    (9 "(") (0 ")")))

(defun zw/indirect-buffers (base-buffer)
  "List all indirect buffers of BASE-BUFFER."
  (when base-buffer
    (cl-remove-if-not
     (lambda (buffer)
       (with-current-buffer buffer
         (eq (buffer-base-buffer) base-buffer)))
     (buffer-list))))

(defun zw/insert-after (list after-item new-item &optional replace)
  "Insert NEW-ITEM into LIST after the first occurrence of AFTER-ITEM."
  (let ((pos (cl-position after-item list)))
    (if pos
        (append (cl-subseq list 0 pos)
                (unless replace (list after-item))
                (list new-item)
                (cl-subseq list (1+ pos)))
      (throw 'not-found "after-item not found in list"))))

;; keep track active UI
(defvar zw/previous-frame nil)
(defvar zw/active-frame nil)
(defvar zw/active-window nil)
(defvar zw/active-window-non-minibufer nil)
(defun zw/update-active-ui (&rest arg)
  "Update active UI."
  (let ((frame (selected-frame))
        (window (selected-window)))
    (unless (or (minibufferp (window-buffer window))
                (eq zw/active-frame frame))
      (setq zw/previous-frame zw/active-frame)
      (setq zw/active-frame frame))
    (setq zw/active-window window)
    (unless (minibuffer-window-active-p (minibuffer-window))
      (setq zw/active-window-non-minibufer window))))
(add-hook 'window-selection-change-functions #'zw/update-active-ui)

;; * Config
(setq-default default-directory (concat (getenv "HOME") "/")
              confirm-kill-emacs 'yes-or-no-p
              large-file-warning-threshold nil)

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
                fringe-mode))
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
(setq-default display-line-numbers-width 3
              display-line-numbers-widen t)
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

;; ** Window placement
;; window split
(setq split-width-threshold  80
      split-height-threshold 80
      split-window-preferred-function 'split-window-sensibly)

(defun zw/display-buffer-in-largest-window (buffer alist)
  (let ((largest-window (get-largest-window (selected-frame) nil)))
    (window--display-buffer buffer largest-window 'reuse alist)))

;; default buffer placement rules
(setq display-buffer-base-action '((display-buffer--maybe-same-window
                                    zw/display-buffer-in-largest-window)))

;; popup buffers
(dolist (mode '(magit-mode-hook
                git-commit-setup-hook))
  (add-hook mode
            (lambda () (setq-local display-buffer-base-action
                                   '((display-buffer--maybe-same-window
                                      display-buffer--maybe-pop-up-frame-or-window))))))

;; buffer placement rules
(setq display-buffer-alist
      '(;; largest window
        ("\\.\\(?:pdf\\)\\'"
         (display-buffer-reuse-mode-window
          zw/display-buffer-in-largest-window))
        ("\\*\\([Hh]elp\\|Man\\|eglot doc\\).*"
         (display-buffer-reuse-mode-window
          zw/display-buffer-in-largest-window))
        ;; top side window
        ("\\*\\(Messages\\|Warnings\\|Backtrace\\).*"
         (display-buffer-in-side-window)
         (window-height . 0.2)
         (side . top)
         (slot . -1))
        ("\\*\\(polymode export\\|compilation\\).*"
         (display-buffer-in-side-window)
         (window-height . 0.2)
         (side . top)
         (slot . 1))
        ;; right side window
        ("\\*\\(R\\|Python\\).*"
         (display-buffer-in-side-window)
         (window-width . 0.3)
         (side . right)
         (slot . 1)
         (dedicated . t))
        ;; bottom side buffer
        ("\\*.*\\(e?shell\\|v?term\\).*"
         (display-buffer-in-side-window)
         (window-height . 0.2)
         (side . bottom)
         (dedicated . t))
        ;; below current window
        ("\\*Calendar.*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . shrink-window-if-larger-than-buffer))))

;; ** Right side window
(defcustom zw/right-side-window-buffer-mode '(inferior-ess-r-mode inferior-python-mode)
  "List of modes of buffer displayed in right side window.")

(defcustom zw/right-side-window-buffer-regex nil
  "List of name regex of buffer displayed in right side window.")

(defvar zw/right-side-window--buffer-opened nil)

(defun zw/right-side-window--update ()
  (setq zw/right-side-window--buffer-opened
        (cl-remove-if-not
         (lambda (buffer)
           (with-current-buffer buffer
             (or (member major-mode zw/right-side-window-buffer-mode)
                 (cl-some (lambda (regex)
                            (string-match-p regex (buffer-name buffer)))
                          zw/right-side-window-buffer-regex))))
         (buffer-list))))

(defun zw/right-side-window-toggle ()
  "Toggle side windows."
  (interactive)
  (zw/right-side-window--update)
  (if zw/right-side-window--buffer-opened
      (if (cl-some (lambda (buffer) (get-buffer-window buffer))
                   zw/right-side-window--buffer-opened)
          (dolist (buffer zw/right-side-window--buffer-opened)
            (let ((buffer-window (get-buffer-window buffer)))
              (when buffer-window
                (if  (eq buffer-window (window-main-window))
                    (previous-buffer)
                  (delete-window buffer-window)))))
        (cl-mapcar 'display-buffer zw/right-side-window--buffer-opened))
    (message "No buffer in side window.")))

;; ** Left side window
(defvar zw/left-side-window-open-functions nil
  "List of functions to open left side window.")

(defun zw/left-side-window-toggle ()
  "Open left side window."
  (interactive)
  (if (eq (zw/window-side (selected-window)) 'left)
      (dolist (left-side-window (cl-remove-if-not
                                 (lambda (window)
                                   (eq (zw/window-side window) 'left))
                                 (window-list)))
        (with-selected-window left-side-window
          (zw/kill-bufer-quit-window)))
    (dolist (func zw/left-side-window-open-functions)
      (funcall func))))

(define-minor-mode zw/left-side-window-mode
  "Toggle left side window."
  :global nil
  :keymap `((,(kbd "s-b") . zw/left-side-window-toggle)
            (,(kbd "s-q") . zw/left-side-window-toggle)))

;; * Tool
;; ** Tramp
(setq tramp-default-method "ssh"
      tramp-auto-save-directory (expand-file-name "tramp-auto-save" user-emacs-directory)
      tramp-persistency-file-name (expand-file-name "tramp-connection-history" user-emacs-directory))
(with-eval-after-load "tramp"
  (setq password-cache-expiry nil
        remote-file-name-inhibit-cache nil
        vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp))
  ;; respect the PATH variable on the remote machine
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

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
      scroll-margin 0
      scroll-conservatively 97
      scroll-preserve-screen-position t
      make-cursor-line-fully-visible nil
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
             ("<next>" . nil)
             ("<header-line> <wheel-up>" . nil)
             ("<header-line> <wheel-down>" . nil)))

;; ** Save place
(add-hook 'after-init-hook 'save-place-mode)

;; ** Open address
(add-hook 'text-mode-hook 'goto-address-mode)
(add-hook 'prog-mode-hook 'goto-address-prog-mode)

;; ** Isearch
(setq isearch-lazy-count t
      lazy-count-prefix-format "%s/%s "
      isearch-wrap-pause 'no)

(bind-keys :map isearch-mode-map
           ([remap isearch-delete-char] . isearch-del-char)
           ("s-f" . isearch-repeat-forward)
           ("s-v" . isearch-yank-kill)
           ("S-<insert>" . isearch-yank-kill))

;; ** Winner mode
(add-hook 'after-init-hook 'winner-mode)
(setq winner-dont-bind-my-keys t)

;; ** Custom tools
(defun zw/dumb-function ()
  (interactive))

(defun zw/quit-window-kill-bufer ()
  "Quit window then kill buffer."
  (interactive)
  (quit-window 'kill))

(defun zw/kill-bufer-quit-window ()
  "Kill buffer then quit window."
  (interactive)
  (if (one-window-p)
      (kill-buffer)
    (kill-buffer-and-window)))

(defun zw/maximize-window ()
  "maximize window (also works for side windows)."
  (interactive)
  (let ((current-buffer-name (buffer-name (current-buffer))))
    (if (window-parameter (get-buffer-window) 'window-side)
        (progn (select-window
                (get-window-with-predicate
                 (lambda (window)
                   (not (window-parameter window 'window-side)))))
               (delete-other-windows)
               (switch-to-buffer current-buffer-name))
      (delete-other-windows))))

(defun zw/update-emacs-tangle-dotfiles ()
  "update zw/emacs and tangle dotfiles"
  (interactive)
  (require 'org)
  (let ((msg (string-trim (shell-command-to-string "cd ~/.emacs.d && git pull"))))
    (org-babel-tangle-file "~/.emacs.d/OrgFiles/dotfiles.org")
    (message (concat "emacs update:\n " msg))))

;; show scratch buffer in new window
(defun zw/new-window ()
  (interactive)
  (split-window-right)
  (other-window 1 nil)
  (switch-to-buffer "*scratch*"))

;; https://xenodium.com/emacs-quick-kill-process/
(defun zw/quick-kill-process ()
  "quick-kill-process"
  (interactive)
  (require 'proced)
  (require 'map)
  (let* ((pid-width 5)
         (comm-width 25)
         (user-width 10)
         (processes (proced-process-attributes))
         (candidates
          (mapcar (lambda (attributes)
                    (let* ((process (cdr attributes))
                           (pid (format (format "%%%ds" pid-width) (map-elt process 'pid)))
                           (user (format (format "%%-%ds" user-width)
                                         (truncate-string-to-width
                                          (map-elt process 'user) user-width nil nil t)))
                           (comm (format (format "%%-%ds" comm-width)
                                         (truncate-string-to-width
                                          (map-elt process 'comm) comm-width nil nil t)))
                           (args-width (- (window-width) (+ pid-width user-width comm-width 3)))
                           (args (map-elt process 'args)))
                      (cons (if args
                                (format "%s %s %s %s" pid user comm (truncate-string-to-width args args-width nil nil t))
                              (format "%s %s %s" pid user comm))
                            process)))
                  processes))
         (selection (map-elt candidates
                             (completing-read "kill process: "
                                              (cl-sort
                                               candidates
                                               (lambda (p1 p2)
                                                 (string-lessp (nth 2 (split-string (string-trim (car p1))))
                                                               (nth 2 (split-string (string-trim (car p2)))))))
                                              nil t)))
         (prompt-title (format "%s %s %s"
                               (map-elt selection 'pid)
                               (map-elt selection 'user)
                               (map-elt selection 'comm))))
    (when (y-or-n-p (format "Kill? %s" prompt-title))
      (if (eq (signal-process (map-elt selection 'pid) 9) 0)
          (message "killed: %s" prompt-title)
        (message "error: could not kill %s" prompt-title)))))

(define-minor-mode zw/presentation-mode
  "Toggle presentation"
  :global nil
  (if zw/presentation-mode
      (setq-local mode-line-format nil)
    (setq-local mode-line-format (default-value 'mode-line-format)))
  (when (eq major-mode 'pdf-view-mode)
    (pdf-view-fit-page-to-window))
  (force-mode-line-update)
  (redraw-display))

(defun zw/smart-tab ()
  "Tab indent or toggle hide show or toggle outline"
  (interactive)
  (cond
   ((and outline-minor-mode
         (or (outline-on-heading-p)
             (outline-invisible-p)))
    (outline-toggle-children))
   ((and hs-minor-mode (hs-already-hidden-p)) (zw/toggle-fold))
   (t (indent-for-tab-command))))

(defun zw/install-fonts ()
  "Install required fonts."
  (interactive)
  (let ((font-dest (cond
                    ;; Default Linux install directories
                    ((member system-type '(gnu gnu/linux gnu/kfreebsd))
                     (concat (or (getenv "XDG_DATA_HOME")
                                 (concat (getenv "HOME") "/.local/share"))
                             "/fonts/"))
                    ;; Default MacOS install directory
                    ((eq system-type 'darwin)
                     (concat (getenv "HOME")
                             "/Library/Fonts/")))))
    (dolist (font (directory-files-recursively "~/.emacs.d/fonts" ""))
      (copy-file font font-dest t))
    (async-shell-command "fc-cache -fv")))

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
           ("<f11>" . zw/presentation-mode)
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
           ;; disable header line mouse scroll
           ("<header-line> <wheel-up>" . zw/dumb-function)
           ("<header-line> <wheel-down>" . zw/dumb-function)
           ;; completion
           ("<C-tab>" . completion-at-point)
           ;; editing
           ("s-<backspace>" . (lambda () (interactive) (kill-line 0)))
           ("S-<home> <delete>" . (lambda () (interactive) (kill-line 0)))
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
           ("C-<f5>" . revert-buffer-quick)
           ("s-r" . revert-buffer-quick)
           ("s-q" . kill-current-buffer)
           ("s-=" . text-scale-increase)
           ("s--" . text-scale-decrease)
           ;; window operations
           ("C-<f4>" . delete-window)
           ("s-w" . delete-window)
           ("s-t" . zw/new-window)
           ("s-<left>" . windmove-left)
           ("s-<right>" . windmove-right)
           ("s-<up>" . windmove-up)
           ("s-<down>" . windmove-down)
           ("s-+" . enlarge-window-horizontally)
           ("s-_" . shrink-window-horizontally)
           ("s-^" . enlarge-window)
           ("s-T" . winner-undo)
           ("s-u" . winner-undo)
           ("s-U" . winner-redo)
           ("s-b" . zw/left-side-window-toggle)
           ("s-B" . zw/right-side-window-toggle)
           ("C-x 1" . zw/maximize-window)
           ;; misc commands
           ("s-i" . imenu)
           ("s-o" . zw/open-in-external)
           ("s-h" . display-local-help)
           ("s-d" . eldoc)
           ("s-\\" . toggle-input-method)
           ("s-p" . zw/repl-run-in-path)
           :map minibuffer-mode-map
           ("<escape>" . minibuffer-keyboard-quit)
           :map prog-mode-map
           ("<tab>" . zw/smart-tab)
           ("TAB" . zw/smart-tab))

;; * Provide
(provide 'zw-base)
