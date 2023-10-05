;; -*- lexical-binding: t -*-

(require 'map)
(require 'proced)
(require 'seq)

;; * Tramp
;; Set default connection mode to SSH
(setq tramp-default-method "ssh")
(setq tramp-auto-save-directory
      (expand-file-name "tramp-auto-save" user-emacs-directory))
(setq tramp-persistency-file-name
      (expand-file-name "tramp-connection-history" user-emacs-directory))
(setq password-cache-expiry nil)
(setq remote-file-name-inhibit-cache nil)
(setq tramp-use-ssh-controlmaster-options nil)
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
(with-eval-after-load "tramp"
  (customize-set-variable 'tramp-ssh-controlmaster-options
                          (concat
                           "-o ControlPath=/tmp/ssh-tramp-%%r@%%h:%%p "
                           "-o ControlMaster=auto -o ControlPersist=yes"))
  ;; respect the PATH variable on the remote machine
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; * Comint
(use-package comint
  :straight (:type built-in)
  :config
  ;; Make processes’ outputs read-only. The prompt is easy.
  (setq comint-prompt-read-only t
        comint-scroll-to-bottom-on-input t
        comint-scroll-to-bottom-on-output nil
        comint-move-point-for-output nil))

;; * Vterm
(use-package vterm
  :bind ((:map vterm-copy-mode-map
               ("<return>" . vterm-copy-mode))
         (:map vterm-mode-map
               ("s-e" . quit-window)
               ("s-z" . vterm-undo)
               ("M-:" . nil)
               ("<escape>" . nil)
               ("<f5>" . nil)))
  :config
  (setq vterm-kill-buffer-on-exit t
        vterm-always-compile-module t)
  (add-hook 'vterm-mode-hook
            (lambda ()
              (vterm-send-string "source ~/.cache/emacs/vterm_conf.sh\n"))))

(use-package multi-vterm
  :commands (multi-vterm)
  :bind (("s-E" . multi-vterm)))

;; * Dired
;; ** main
(use-package dired
  :straight (:type built-in)
  :hook
  (dired-mode . dired-async-mode)
  (dired-mode . dired-omit-mode)
  (dired-mode . (lambda () (visual-line-mode 0)))
  :bind ((:map global-map
               ("s-b" . zw/dired-sidebar-toggle))

         (:map dired-mode-map
               ("<tab>" . dired-subtree-toggle)
               ("q" . zw/kill-bufer-quit-window)))
  :init
  (setq dired-dwim-target t
        dired-kill-when-opening-new-dired-buffer t
        dired-create-destination-dirs t
        dired-create-destination-dirs-on-trailing-dirsep t
        dired-mouse-drag-files t
        dired-free-space 'separate
        dired-use-ls-dired t
        dired-listing-switches "-al --no-group --human-readable --group-directories-first"
        dired-omit-extensions '("~")
        dired-omit-files "^\\.$\\|^\\.\\.$")
  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls")))

(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and preview as well
   (dirvish-directory-view-mode . diredfl-mode)))

(use-package dired-subtree
  :commands (dired-subtree-toggle))

;; ** dired side bar
(defun zw/dired-sidebar--modeline-major-mode ()
  "Sidebar modeline major mode."
  (propertize "Sidebar " 'face (zw/modeline-set-face 'zw/modeline-major-mode-active
                                                     'zw/modeline-default-inactive)))

(defun zw/dired-sidebar--modeline-directory (dir)
  "Sidebar modeline directory."
  (car (last (split-string dir "/") 2)))

(defun zw/dired-sidebar--modeline-format (sidebar-name)
  (list "%e" " "
        `(:eval (propertize ,(concat sidebar-name zw/modeline-separator)
                            'face (zw/modeline-set-face 'zw/modeline-major-mode-active
                                                        'zw/modeline-default-inactive)))
        '(:eval (zw/modeline-remote))
        '(:eval (zw/modeline-middle-space (zw/dired-sidebar--modeline-major-mode)))))

(defun zw/dired-siderbar-display (buffer)
  ;; bury current dired buffer when it has the same root as sidebar
  (when (eq (current-buffer) buffer)
    (bury-buffer))
  ;; display sidebar
  (display-buffer-in-side-window
   buffer `((side . left) (slot . 0)
            (window-width . 0.2)
            (preserve-size . (t . nil))))
  (select-window (get-buffer-window buffer)))

(defun zw/dired-sidebar-enable (buffer)
  (interactive)
  (with-current-buffer buffer
    (when (eq major-mode 'dired-mode)
      (zw-dired-sidebar-mode 1)
      (let* ((dir (abbreviate-file-name (dired-current-directory)))
             (current-dir (zw/dired-sidebar--modeline-directory dir))
             (name (concat " :" dir)))
        (dired-hide-details-mode t)
        (rename-buffer name)
        (setq-local mode-line-format (zw/dired-sidebar--modeline-format current-dir))
        (zw/dired-siderbar-display buffer)
        (set-window-dedicated-p (get-buffer-window buffer) t)))))

(defun zw/dired-sidebar-disable (buffer)
  (interactive)
  (with-current-buffer buffer
    (when zw-dired-sidebar-mode
      (zw-dired-sidebar-mode 0)
      (let* ((dir (abbreviate-file-name (dired-current-directory))))
        (dired-hide-details-mode 0)
        (rename-buffer dir)
        (setq-local mode-line-format (default-value 'mode-line-format))
        ;; close sidebar
        (quit-window) (display-buffer buffer)
        (set-window-dedicated-p (get-buffer-window buffer) nil)))))

(defun zw/dired-sidebar-toggle ()
  "Toggle dired on left side."
  (interactive)
  ;; open current directory in sidebar
  (let* ((dir (abbreviate-file-name
               (or (vc-root-dir)
                   (ignore-errors (file-name-directory (buffer-file-name)))
                   default-directory)))
         (buffer (dired-noselect dir)))
    (zw/dired-sidebar-enable buffer)))

(defun zw/dired-sidebar-find-file ()
  (interactive)
  (dired-find-file)
  (zw/dired-sidebar-enable (current-buffer)))

(defun zw/dired-sidebar-up-directory ()
  (interactive)
  (dired-up-directory)
  (zw/dired-sidebar-enable (current-buffer)))

(defun zw/dired-sidebar-maximize ()
  (interactive)
  (zw/maximize-window)
  (zw/dired-sidebar-disable (current-buffer)))

(define-minor-mode zw-dired-sidebar-mode
  "Toggle zw-dired-sidebar mode."
  :lighter " Dired-Sidebar"
  :keymap
  `((,(kbd "s-q") . zw/kill-bufer-quit-window)
    (,(kbd "q") . zw/kill-bufer-quit-window)
    (,(kbd "s-b") . zw/kill-bufer-quit-window)
    (,(kbd "^") . zw/dired-sidebar-up-directory)
    (,(kbd "RET") . zw/dired-sidebar-find-file)
    (,(kbd "<mouse-2>") . zw/dired-sidebar-find-file)
    (,(kbd "C-x 1") . zw/dired-sidebar-maximize)))

;; * Openwith
(defvar open-app-command (pcase system-type
                           ('gnu/linux "xdg-open")
                           (_ "open"))
  "Shell command used to open in external apps.")

(use-package openwith
  :hook
  (after-init . openwith-mode)
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("doc" "docx" "xls" "xlsx" "ppt" "pptx" "odt" "ods" "odg" "odp"
                  "mpg" "mpeg" "mp3" "mp4" "avi" "wmv" "wav" "mov" "flv" "ogm"
                  "ogg" "mkv"))
               open-app-command
               '(file)))))

(defun zw/open-in-external (arg)
  "Open visited file in default external program."
  (interactive "P")
  (when buffer-file-name
    (call-process-shell-command
     (concat open-app-command " " (shell-quote-argument buffer-file-name))
     nil 0)))

;; * Recentf
(use-package recentf
  :straight (:type built-in)
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/" "~/.emacs.d/straight/"
                no-littering-var-directory no-littering-etc-directory
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
  ;; save recentf-list before closing frame
  (advice-add 'save-buffers-kill-terminal :before 'recentf-save-list))

;; * Savehist
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :hook (after-init . savehist-mode)
  :config (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
                history-length 25))

;; * Helpful
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

;; * Key hints
(use-package hydra
  :hook (emacs-lisp-mode . hydra-add-imenu))

(use-package which-key
  :diminish
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

;; * Open address
(use-package goto-addr
  :straight (:type built-in)
  :hook
  (text-mode . goto-address-mode)
  (prog-mode . goto-address-prog-mode))

;; * Web search
(use-package emacs-websearch
  :straight '(emacs-websearch :host github :repo "zhenhua-wang/emacs-websearch")
  :bind (("s-l" . emacs-websearch)))

;; * Winner-mode
(use-package winner
  :straight (:type built-in)
  :hook (after-init . winner-mode)
  :bind (("s-T" . winner-undo)
         ("s-u" . winner-undo)
         ("s-U" . winner-redo)))

;; * Isearch
(use-package isearch
  :straight (:type built-in)
  :bind (:map isearch-mode-map
              ([remap isearch-delete-char] . isearch-del-char))
  :config
  (setq isearch-lazy-count t
        lazy-count-prefix-format "%s/%s "))

;; * Custom tools
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
        (progn (select-window (window-main-window))
               (delete-other-windows)
               (switch-to-buffer current-buffer-name))
      (delete-other-windows))))

(defun zw/update-emacs-tangle-dotfiles ()
  "update zw/emacs and tangle dotfiles"
  (interactive)
  (require 'org)
  (shell-command "cd ~/.emacs.d && git pull")
  (org-babel-tangle-file "~/.emacs.d/OrgFiles/dotfiles.org")
  (message "Emacs updated & dotfiles tangled!"))

(defun zw/show-info ()
  "show buffer info"
  (interactive)
  (message (if buffer-file-name
               (concat "File: "
                       (buffer-file-name)
                       ", Encoding:"
                       (zw/modeline-encoding))
             (concat "Buffer: "
                     (buffer-name)
                     ", Encoding:"
                     (zw/modeline-encoding)))))

;; set preference to horizontal split
(defun zw/split-window-sensibly-prefer-horizontal (&optional window)
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
               (split-window-right)))))))
  ;; switch to scratch buffer after creating new window
  (other-window 1 nil)
  (switch-to-buffer "*scratch*"))

;; https://xenodium.com/emacs-quick-kill-process/
(defun zw/quick-kill-process ()
  "quick-kill-process"
  (interactive)
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
                                              (seq-sort
                                               (lambda (p1 p2)
                                                 (string-lessp (nth 2 (split-string (string-trim (car p1))))
                                                               (nth 2 (split-string (string-trim (car p2))))))
                                               candidates) nil t)))
         (prompt-title (format "%s %s %s"
                               (map-elt selection 'pid)
                               (map-elt selection 'user)
                               (map-elt selection 'comm))))
    (when (y-or-n-p (format "Kill? %s" prompt-title))
      (if (eq (signal-process (map-elt selection 'pid) 9) 0)
          (message "killed: %s" prompt-title)
        (message "error: could not kill %s" prompt-title)))))

(defvar-local zw/presentation-on nil)
(defun zw/toggle-presentation ()
  "Toggle presentation"
  (interactive)
  (if zw/presentation-on
      (progn
        (setq-local mode-line-format (default-value 'mode-line-format)
                    zw/presentation-on nil))
    (progn
      (setq-local mode-line-format nil
                  zw/presentation-on t)))
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
      (copy-file font font-dest t)))
  (nerd-icons-install-fonts))

;; * Provide
(provide 'zw-tools)
