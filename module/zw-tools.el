;; -*- lexical-binding: t -*-

;; * Vterm
(use-package vterm
  :bind (("s-e" . vterm)
         (:map vterm-copy-mode-map
               ("<return>" . vterm-copy-mode))
         (:map vterm-mode-map
               ("s-e" . quit-window)
               ("s-z" . vterm-undo)
               ("M-:" . nil)
               ("<escape>" . nil)
               ("<f5>" . nil)))
  :config
  (setq vterm-kill-buffer-on-exit t
        vterm-always-compile-module t
        vterm-tramp-shells '(("ssh" "/usr/bin/bash")
                             ("docker" "/bin/sh")))
  (when (executable-find "/usr/bin/zsh")
    (setq vterm-shell "/usr/bin/zsh"))
  (add-hook 'vterm-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (vterm-send-string "source ~/.cache/emacs/vterm_conf.sh\n")))))

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
  (dired-mode . zw/dired-setup)
  :bind ((:map dired-mode-map
               ("s-f" . isearch-forward)
               ("<tab>" . zw/dired-subtree-toggle)
               ("q" . zw/kill-bufer-quit-window)))
  :init
  (setq dired-dwim-target t
        dired-kill-when-opening-new-dired-buffer t
        dired-create-destination-dirs t
        dired-create-destination-dirs-on-trailing-dirsep t
        dired-mouse-drag-files t
        dired-free-space 'separate
        dired-use-ls-dired t
        dired-listing-switches "-alh --group-directories-first"
        dired-omit-extensions '("~")
        dired-omit-files "^\\..?$"
        dired-omit-verbose nil)
  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls"))
  (defun zw/dired-directory-empty-p ()
    (= (length (directory-files
                (dired-get-filename) nil
                directory-files-no-dot-files-regexp))
       0))
  (defun zw/dired-setup ()
    (visual-line-mode -1)
    (add-hook 'post-command-hook #'force-mode-line-update nil t)))

(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and preview as well
   (dirvish-directory-view-mode . diredfl-mode)))

(use-package dired-subtree
  :commands (dired-subtree-toggle
             dired-subtree--dired-line-is-directory-or-link-p
             dired-subtree--is-expanded-p)
  :init
  (defun zw/dired-subtree-toggle ()
    (interactive)
    (when (and (dired-subtree--dired-line-is-directory-or-link-p)
               (not (zw/dired-directory-empty-p)))
      (dired-subtree-toggle)
      (revert-buffer)))
  :config
  (setq dired-subtree-use-backgrounds nil))

;; ** dired side bar
(defun zw/dired-sidebar--modeline-name ()
  "Sidebar modeline name."
  (let* ((dir (dired-current-directory))
         (name (car (last (split-string dir "/") 2))))
    (propertize (concat " " name " "
                        zw/modeline-separator)
                'face (zw/modeline-set-face 'zw/modeline-major-mode-active
                                            'zw/modeline-default-inactive))))

(defun zw/dired-sidebar--modeline-format ()
  (list "%e"
        '(:eval (zw/modeline-remote))
        '(:eval (zw/dired-sidebar--modeline-name))
        '(:eval (zw/modeline-line-column))
        '(:eval (zw/modeline-bar))))

(defun zw/dired-sidebar-folder-indicator ()
  "Display the icons of files in a dired buffer."
  (interactive)
  (let* ((inhibit-read-only t)
         (icon-face '(:inherit dired-ignored :underline nil :background unspecified))
         (click-func (lambda ()
                       (dired-subtree-toggle)))
         (collapsible-icon (propertize
                            (nerd-icons-octicon
                             "nf-oct-chevron_down"
                             :height 0.6
                             :v-adjust 0.3
                             :face icon-face)
                            'click-func click-func
                            'mouse-face 'highlight
                            'help-echo "mouse-2: Toggle subdirectory"))
         (expandable-icon (propertize
                           (nerd-icons-octicon
                            "nf-oct-chevron_right"
                            :height 0.6
                            :v-adjust 0.3
                            :face icon-face)
                           'click-func click-func
                           'mouse-face 'highlight
                           'help-echo "mouse-2: Toggle subdirectory")))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (dired-move-to-filename nil)
          (dired-move-to-filename)
          (let ((file (dired-get-filename 'verbatim t)))
            (unless (member file '("." ".."))
              (let ((filename (dired-get-filename nil t)))
                (if (and (file-directory-p filename)
                         ;; ignore folders without permission
                         (not (ignore-errors
                                (zw/dired-directory-empty-p))))
                    (if (dired-subtree--is-expanded-p)
                        (insert (concat collapsible-icon " "))
                      (insert (concat expandable-icon " ")))
                  (insert "  "))))))
        (forward-line 1)))))

(defun zw/dired-siderbar-display (buffer)
  ;; bury current dired buffer when it has the same root as sidebar
  (when (eq (current-buffer) buffer)
    (bury-buffer))
  ;; display sidebar
  (let ((window (display-buffer-in-side-window
                 buffer `((side . left) (slot . -99)
                          (window-width . 0.2)
                          (preserve-size . (t . nil))))))
    (select-window window)
    (set-window-dedicated-p window t)))

(defun zw/dired-sidebar-header-line-prefix ()
  (let ((color (face-background 'header-line))
        (width 1)
        (height (floor (* (string-pixel-width " ")
                          2.5))))
    (concat (zw/modeline--bar color width height)
            (nerd-icons-sucicon
             "nf-custom-folder_open"
             :height 0.9
             :v-adjust 0.13)
            " ")))

(defun zw/dired-sidebar-header-line-main ()
  (let* ((abbrev-path (substring-no-properties
                       (abbreviate-file-name default-directory) 0 -1))
         (dirs (split-string abbrev-path "/"))
         (locs (number-sequence 1 (length dirs)))
         (parent-dirs (cl-mapcar
                       (lambda (loc)
                         (string-join
                          (cl-subseq dirs 0 loc) "/"))
                       locs))
         (pairs (cl-mapcar 'cons dirs parent-dirs))
         (create-keymap (lambda (dir)
                          (let ((map (make-sparse-keymap)))
                            (define-key map [header-line mouse-2]
                                        (lambda ()
                                          (interactive)
                                          (dired dir)
                                          (zw/dired-sidebar-enable (current-buffer))))
                            map)))
         (dirs (cl-mapcar
                (lambda (pair)
                  (propertize (car pair)
                              'keymap (funcall create-keymap (cdr pair))
                              'mouse-face 'highlight))
                pairs)))
    (concat (when (string-empty-p (car dirs))
              (propertize "/" 'keymap (funcall create-keymap "/")
                          'mouse-face 'highlight))
            (when (cl-remove-if 'string-empty-p dirs)
              (string-join dirs (nerd-icons-octicon
                                 "nf-oct-chevron_right"
                                 :height 0.9
                                 :v-adjust 0.13
                                 :face 'shadow))))))

(defvar zw/dired-sidebar-header-line-beg 0)
(defun zw/dired-sidebar-header-line-format ()
  (let* ((format (zw/dired-sidebar-header-line-main))
         (format-width (length format))
         (format-prefix (zw/dired-sidebar-header-line-prefix))
         (format-prefix-width (length format-prefix))
         (window-width (- (window-width) format-prefix-width)))
    (concat
     format-prefix
     (substring
      format
      (cond ((or (< zw/dired-sidebar-header-line-beg 0)
                 (< format-width window-width))
             0)
            ((> zw/dired-sidebar-header-line-beg
                (- format-width window-width))
             (- format-width window-width))
            (t zw/dired-sidebar-header-line-beg))))))

(defun zw/dired-sidebar-header-line-wheel-backward-action ()
  (interactive)
  (when (> zw/dired-sidebar-header-line-beg 0)
    (setq zw/dired-sidebar-header-line-beg
          (- zw/dired-sidebar-header-line-beg 1))))

(defun zw/dired-sidebar-header-line-wheel-forward-action ()
  (interactive)
  (when (> (- (length (format-mode-line header-line-format))
              (length (zw/dired-sidebar-header-line-prefix)))
           (window-width))
    (setq zw/dired-sidebar-header-line-beg
          (+ zw/dired-sidebar-header-line-beg 1))))

(defun zw/dired-sidebar-format-header-line ()
  (setq-local
   header-line-format
   (list "%e" "   " '(:eval (zw/dired-sidebar-header-line-format)))))

(defun zw/dired-sidebar-hide-information-line ()
  (save-excursion
    (let* ((beg (point-min))
           (end (progn
                  (beginning-of-buffer)
                  (+ 1 (line-end-position))))
           (overlay-highlight (make-overlay beg end)))
      (overlay-put overlay-highlight 'invisible t))))

(defun zw/dired-sidebar-enable (buffer)
  (with-current-buffer buffer
    (when (eq major-mode 'dired-mode)
      ;; rename buffer
      (let* ((dir (abbreviate-file-name (dired-current-directory)))
             (name (concat " :" dir)))
        (rename-buffer name))
      ;; enable modes
      (zw-dired-sidebar-mode 1)
      (dired-hide-details-mode t)
      (zw/dired-sidebar-format-header-line)
      (zw/dired-siderbar-display buffer)
      ;; set local variables
      (add-hook 'dired-after-readin-hook
                'zw/dired-sidebar-folder-indicator :append :local)
      (add-hook 'dired-after-readin-hook
                'zw/dired-sidebar-hide-information-line :append :local)
      (setq-local mode-line-format (zw/dired-sidebar--modeline-format)
                  ;; display header line from beginning
                  zw/dired-sidebar-header-line-beg (- (+ (length (zw/dired-sidebar-header-line-main))
                                                         (length (zw/dired-sidebar-header-line-prefix)))
                                                      (window-width)))
      ;; refresh display
      (dired-revert))))

(defun zw/dired-sidebar-disable (buffer)
  (with-current-buffer
      (when zw-dired-sidebar-mode
        (let* ((dir (abbreviate-file-name (dired-current-directory))))
          (kill-buffer buffer)
          (dired dir)))))

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

(defun zw/dired-sidebar-mouse-find-file (event)
  (interactive "e")
  (let (pos)
    (save-excursion
      (setq pos (posn-point (event-start event)))
      (let ((click-func (get-text-property pos 'click-func)))
        (if click-func
            (funcall click-func)
          (dired-mouse-find-file event)))))
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
  `((,(kbd "q") . zw/kill-bufer-quit-window)
    (,(kbd "^") . zw/dired-sidebar-up-directory)
    (,(kbd "RET") . zw/dired-sidebar-find-file)
    (,(kbd "<mouse-2>") . zw/dired-sidebar-mouse-find-file)
    (,(kbd "C-x 1") . zw/dired-sidebar-maximize)
    (,(kbd "<header-line> <triple-wheel-right>") . zw/dired-sidebar-header-line-wheel-forward-action)
    (,(kbd "<header-line> <triple-wheel-left>") . zw/dired-sidebar-header-line-wheel-backward-action)
    (,(kbd "<header-line> <triple-wheel-down>") . zw/dired-sidebar-header-line-wheel-forward-action)
    (,(kbd "<header-line> <triple-wheel-up>") . zw/dired-sidebar-header-line-wheel-backward-action)
    (,(kbd "M-n") . zw/dired-sidebar-header-line-wheel-forward-action)
    (,(kbd "M-p") . zw/dired-sidebar-header-line-wheel-backward-action)))

;; register in zw/left-side-window
(add-to-list 'zw/left-side-window-open-functions 'zw/dired-sidebar-toggle)
(add-hook 'zw-dired-sidebar-mode-hook 'zw/left-side-window-mode)

;; * Openwith
(use-package openwith
  :hook
  (after-init . openwith-mode)
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("doc" "docx" "xls" "xlsx" "ppt" "pptx" "odt" "ods" "odg" "odp"
                  "mpg" "mpeg" "mp3" "mp4" "avi" "wmv" "wav" "mov" "flv" "ogm"
                  "webm" "ogg" "mkv"))
               open-app-command
               '(file)))))

;; * Helpful
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

;; * Which Key
(use-package which-key
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

;; * keycast
(use-package keycast
  :bind ("s-k" . keycast-tab-bar-mode))

;; * Web search
(use-package emacs-websearch
  :straight '(emacs-websearch :host github :repo "zhenhua-wang/emacs-websearch")
  :bind (("s-l" . emacs-websearch)))

;; * Image scroll
(use-package iscroll
  :hook
  (image-mode . iscroll-mode)
  (org-mode . iscroll-mode)
  (markdown-mode . iscroll-mode))
;; * Psearch
(use-package psearch
  :straight (:host github :repo "twlz0ne/psearch.el"))

;; * Provide
(provide 'zw-tools)
