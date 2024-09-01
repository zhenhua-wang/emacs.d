;; -*- lexical-binding: t -*-

;; * Vterm
(use-package vterm
  :bind ((:map vterm-copy-mode-map
               ("<return>" . vterm-copy-mode))
         (:map vterm-mode-map
               ([xterm-paste] . vterm-xterm-paste)
               ("s-e" . quit-window)
               ("s-E" . quit-window)
               ("s-S-e" . quit-window)
               ("s-z" . vterm-undo)
               ("M-:" . nil)
               ("<escape>" . nil)
               ("<f1>" . nil)
               ("<f9>" . nil)
               ("<f10>" . nil)
               ("<f11>" . nil)
               ("<f12>" . nil)))
  :init
  (setq zw/term-function 'vterm)
  :config
  (setq vterm-kill-buffer-on-exit t
        vterm-always-compile-module t
        vterm-tramp-shells '(("ssh" "/usr/bin/bash")
                             ("scp" "/usr/bin/bash")
                             ("docker" "/bin/sh")))
  (when (executable-find "/usr/bin/zsh")
    (setq vterm-shell "/usr/bin/zsh"))
  (add-hook 'vterm-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (if (string-match-p "zsh" vterm-shell)
                    (vterm-send-string
                     "source ~/.emacs.d/resources/scripts/zw-vterm-zsh-config.sh\n")
                  (vterm-send-string
                   "source ~/.emacs.d/resources/scripts/zw-vterm-bash-config.sh\n"))))))

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
               ("TAB" . zw/dired-subtree-toggle)
               ("q" . zw/kill-bufer-quit-window)
               ("o" . zw/dired-open-externally)))
  :init
  (setq dired-dwim-target t
        dired-kill-when-opening-new-dired-buffer t
        dired-create-destination-dirs t
        dired-create-destination-dirs-on-trailing-dirsep t
        dired-mouse-drag-files nil
        dired-free-space 'separate
        dired-use-ls-dired t
        dired-listing-switches "-alh --group-directories-first"
        dired-omit-extensions '("~"))
  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls"))
  (defun zw/dired-directory-empty-p ()
    (= (length (directory-files
                (dired-get-filename) nil
                directory-files-no-dot-files-regexp))
       0))
  (defun zw/dired-setup ()
    (zw/visual-line-disable)
    (add-hook 'post-command-hook #'force-mode-line-update nil t))
  (defun zw/dired-open-externally ()
    "In dired, open the file named on this line."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (message "Opening %s..." file)
      (call-process "xdg-open" nil 0 nil file)
      (message "Opening %s done" file))))

(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode)))

(use-package dired-subtree
  :commands (dired-subtree-toggle
             dired-subtree--dired-line-is-directory-or-link-p
             dired-subtree--is-expanded-p)
  :init
  (defun zw/dired-subtree-toggle ()
    (interactive)
    ;; move forward if invisible
    (while (invisible-p (point))
      (right-char 1))
    (when (and (dired-subtree--dired-line-is-directory-or-link-p)
               (not (zw/dired-directory-empty-p)))
      (save-excursion
        (dired-move-to-filename)
        (if (dired-subtree--is-expanded-p)
            (progn
              (dired-next-line 1)
              (dired-subtree-remove)
              (when (bobp)
                (dired-next-line 1)))
          (dired-subtree-insert)))
      (revert-buffer)))
  :config
  (setq dired-subtree-use-backgrounds t))

;; ** dired icon
(defun zw/dired-icon ()
  "Display the icons of files in a dired buffer."
  (let* ((inhibit-read-only t)
         (icon-face '(:inherit dired-ignored :underline nil :background unspecified))
         (collapsible-icon (propertize
                            (nerd-icons-octicon
                             "nf-oct-chevron_down"
                             :height 0.6
                             :v-adjust 0.3
                             :face icon-face)
                            'click-func 'dired-subtree-toggle
                            'mouse-face 'highlight
                            'help-echo "mouse-2: Toggle subdirectory"))
         (expandable-icon (propertize
                           (nerd-icons-octicon
                            "nf-oct-chevron_right"
                            :height 0.6
                            :v-adjust 0.3
                            :face icon-face)
                           'click-func 'dired-subtree-toggle
                           'mouse-face 'highlight
                           'help-echo "mouse-2: Toggle subdirectory")))
    (save-excursion
      (goto-char (point-min))
      ;; hide information line (the first line)
      (when zw-dired-sidebar-mode
        (let* ((beg (point-min))
               (end (+ 1 (line-end-position)))
               (overlay-highlight (make-overlay beg end)))
          (overlay-put overlay-highlight 'invisible t)))
      (while (not (eobp))
        ;; hide leading whitespace
        (when zw-dired-sidebar-mode
          (beginning-of-line)
          (let* ((beg (point))
                 (end (progn (dired-move-to-filename) (point)))
                 (overlay-highlight (make-overlay beg end)))
            (overlay-put overlay-highlight 'invisible t)))
        ;; add folder indicator and icon
        (when (dired-move-to-filename nil)
          (dired-move-to-filename)
          (let* ((file (dired-get-filename 'relative t))
                 (icon (if (file-directory-p file)
                           (nerd-icons-icon-for-dir file
                                                    :face 'dired-directory
                                                    :v-adjust 0.01)
                         (nerd-icons-icon-for-file file :v-adjust 0.01))))
            (when zw-dired-sidebar-mode
              (if (and (file-directory-p file)
                       ;; ignore folders without permission
                       (not (ignore-errors
                              (zw/dired-directory-empty-p))))
                  (if (dired-subtree--is-expanded-p)
                      (insert (concat collapsible-icon " "))
                    (insert (concat expandable-icon " ")))
                (insert "  ")))
            (insert (concat icon " "))))
        (forward-line 1)))))
(add-hook 'dired-after-readin-hook 'zw/dired-icon :append)

;; ** dired side bar
(defun zw/dired-sidebar--modeline-format ()
  (list "%e"
        '(:eval (zw/modeline-bar))
        '(:eval (zw/modeline-remote))
        '(:eval (propertize
                 (zw/modeline-line-column)
                 'face (zw/modeline-set-face
                        'zw/modeline-buffer-name-active
                        'zw/modeline-default-inactive)))))

(defun zw/dired-siderbar-display (buffer)
  ;; bury dired buffers that have the same root as sidebar
  (dolist (window (window-list))
    (let ((buf (window-buffer window)))
      (when (eq buf buffer)
        (with-selected-window window
          (bury-buffer)))))
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
            " "
            (nerd-icons-mdicon
             "nf-md-home_analytics"
             :height 1
             :v-adjust 0.1)
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
                              'face '(:height 0.9)
                              'mouse-face 'highlight))
                pairs)))
    (concat (when (string-empty-p (car dirs))
              (propertize "/" 'keymap (funcall create-keymap "/")
                          'mouse-face 'highlight))
            (when (cl-remove-if 'string-empty-p dirs)
              (string-join dirs (nerd-icons-octicon
                                 "nf-oct-triangle_right"
                                 :height 0.9
                                 :v-adjust 0.1
                                 :face 'shadow)))
            " ")))

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

(defun zw/dired-sidebar-header-line-max ()
  (- (+ (length (zw/dired-sidebar-header-line-main))
        (length (zw/dired-sidebar-header-line-prefix)))
     (window-width)))

(defun zw/dired-sidebar-header-line-wheel-backward-action ()
  (interactive)
  (when (> zw/dired-sidebar-header-line-beg 0)
    (setq zw/dired-sidebar-header-line-beg
          (- zw/dired-sidebar-header-line-beg 1))))

(defun zw/dired-sidebar-header-line-wheel-forward-action ()
  (interactive)
  (when (< zw/dired-sidebar-header-line-beg
           (zw/dired-sidebar-header-line-max))
    (setq zw/dired-sidebar-header-line-beg
          (+ zw/dired-sidebar-header-line-beg 1))))

(defun zw/dired-sidebar-format-header-line ()
  (setq-local
   header-line-format
   (list "%e" '(:eval (zw/dired-sidebar-header-line-format)))))

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
      (dired-omit-mode 1)
      (zw/dired-sidebar-format-header-line)
      (zw/dired-siderbar-display buffer)
      ;; refresh display
      (dired-revert)
      (setq-local dired-omit-size-limit nil
                  mode-line-format (zw/dired-sidebar--modeline-format)
                  display-buffer-base-action '((zw/display-buffer-in-largest-window))
                  zw/dired-sidebar-header-line-beg (zw/dired-sidebar-header-line-max)))))

(defun zw/dired-sidebar-disable (buffer)
  (with-current-buffer buffer
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
      (let ((click-func (ignore-errors (get-text-property pos 'click-func))))
        (if click-func
            (funcall click-func)
          (dired-mouse-find-file event)))))
  (zw/dired-sidebar-enable (current-buffer)))

(defun zw/dired-sidebar-up-directory ()
  (interactive)
  (goto-char (point-min))
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
    (,(kbd "M-p") . zw/dired-sidebar-header-line-wheel-backward-action))
  (unless (derived-mode-p 'dired-mode)
    (error "`zw-dired-sidebar-mode' should be enabled only in `dired-mode'")))

;; register in zw/left-side-window
(add-to-list 'zw/left-side-window-open-functions 'zw/dired-sidebar-toggle)
(add-hook 'zw-dired-sidebar-mode-hook 'zw/left-side-window-mode)

;; ** favorite
(defun zw/dired-favorite ()
  "Select a favorite directory from a list and open it in dired mode."
  (interactive)
  (let* ((directories '(("Pictures" . "~/Pictures")
                        ("Documents" . "~/Documents")
                        ("Downloads" . "~/Downloads")
                        ("Workspace" . "~/Workspace")))
         (dirs-icon (cl-mapcar
                     (lambda (dir)
                       (cons
                        (concat (nerd-icons-icon-for-dir
                                 (car dir)
                                 :face 'dired-directory
                                 :v-adjust 0.01)
                                " "
                                (car dir))
                        (cdr dir)))
                     directories))
         (selection (completing-read "Select directory: " dirs-icon nil t))
         (path (cdr (assoc selection dirs-icon))))
    (dired path)))

(define-key global-map (kbd "s-n") 'zw/dired-favorite)
(define-key zw-dired-sidebar-mode-map (kbd "f") 'zw/dired-favorite)

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
  :hook (after-init . which-key-mode))

;; * keycast
(use-package keycast
  :bind ("s-k" . keycast-tab-bar-mode))

;; * Web search
(use-package emacs-websearch
  :straight '(emacs-websearch :host github :repo "zhenhua-wang/emacs-websearch")
  :bind (("s-l" . emacs-websearch))
  :config
  (setq emacs-websearch-async t))

;; * Image scroll
(use-package iscroll
  :hook
  (image-mode . iscroll-mode)
  (org-mode . iscroll-mode)
  (markdown-mode . iscroll-mode))

;; * Flyspell correct
(use-package flyspell-correct
  :after flyspell
  :bind ((:map flyspell-mode-map ("M-$" . flyspell-correct-at-point))))

;; * Rime
(use-package rime
  :init
  (setq default-input-method "rime"
        rime-show-preedit t
        rime-user-data-dir (expand-file-name "rime/" user-emacs-directory)
        rime-show-candidate 'posframe
        rime-posframe-properties (list :internal-border-width 2))
  :config
  ;; init user config
  (let* ((dir rime-user-data-dir)
         (config (expand-file-name "default.custom.yaml" dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (unless (file-exists-p config)
      (make-symbolic-link (expand-file-name "default.custom.yaml"
                                            "~/.config/ibus/rime/")
                          config t)))
  ;; rime finalize
  (add-hook 'kill-emacs-hook (lambda () (ignore-errors (rime-lib-finalize)))))

;; * Provide
(provide 'zw-tools)
