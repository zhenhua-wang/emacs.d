;; -*- lexical-binding: t -*-
;; * exwm init
(straight-use-package 'exwm)

(setq
 ;; disable conformation to kill processes on Emacs exit
 confirm-kill-processes nil
 ;; Window focus should follow the mouse pointer
 mouse-autoselect-window nil
 focus-follows-mouse nil
 ;; Automatically send the mouse cursor to the selected workspace's display
 exwm-workspace-warp-cursor t
 exwm-workspace-number 1
 ;; show buffer in all workspace
 exwm-workspace-show-all-buffers nil
 ;; able to move to buffer in inactive space
 exwm-layout-show-all-buffers nil
 exwm-manage-force-tiling t
 exwm-floating-border-width 3)

;; * exwm applications
(defun zw/exwm-run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

;; initialization
(defun zw/exwm-init-hook ()
  ;; Launch apps that will run in the background
  (when (executable-find "dunst")
    (zw/exwm-run-in-background "dunst"))
  (when (executable-find "nm-applet")
    (zw/exwm-run-in-background "nm-applet"))
  (when (executable-find "pasystray")
    (zw/exwm-run-in-background "pasystray"))
  (when (executable-find "blueman-applet")
    (zw/exwm-run-in-background "blueman-applet"))
  (when (executable-find "udiskie")
    (zw/exwm-run-in-background "udiskie --no-automount -t"))
  (when (executable-find "cbatticon")
    (zw/exwm-run-in-background "cbatticon"))
  (when (executable-find "onboard")
    (zw/exwm-run-in-background "onboard"))
  ;; set ibus to use "system keyboard layout" in advanced setting
  ;; (zw/exwm-run-in-background "ibus-daemon -drxR")
  (when (executable-find "polybar")
    (zw/exwm-run-in-background "polybar panel")))

;; When EXWM starts up, do some extra configuration
(add-hook 'exwm-init-hook #'zw/exwm-init-hook)

;; * exwm appearance
;; ** window management
(defun zw/exwm-update-title ()
  (if (and exwm-title
           (string= (downcase exwm-title)
                    (downcase exwm-class-name)))
      (exwm-workspace-rename-buffer exwm-class-name)
    (exwm-workspace-rename-buffer (format "%s: %s" exwm-class-name exwm-title))))

(defun zw/exwm-configure-window-by-class ()
  (pcase exwm-class-name
    ;; floating utils
    ("URxvt"
     (exwm-floating-toggle-floating))
    ("Emacs"
     (exwm-floating-toggle-floating))))

(add-hook 'exwm-manage-finish-hook #'zw/exwm-configure-window-by-class)
(add-hook 'exwm-update-class-hook #'zw/exwm-update-title)
(add-hook 'exwm-update-title-hook #'zw/exwm-update-title)

;; ** wallpaper
(defun zw/exwm-set-wallpaper ()
  (unless (file-exists-p "~/.cache/emacs/wallpaper.png")
    (copy-file "~/.emacs.d/exwm/wallpaper.png" "~/.cache/emacs/wallpaper.png"))
  (with-current-buffer "*scratch*"
    (display-line-numbers-mode 0))
  (start-process-shell-command
   "feh" nil  "feh --bg-scale ~/.cache/emacs/wallpaper.png"))

(zw/exwm-set-wallpaper)

;; ** transparency
(defun zw/set-opacity (predicate)
  (if predicate
      (set-frame-parameter (selected-frame) 'alpha-background 98)
    (set-frame-parameter (selected-frame) 'alpha-background 0)))

(defun zw/exwm-set-scratch-ui (predicate)
  (if predicate
      (setq-local cursor-type (default-value 'cursor-type)
                  mode-line-format (default-value 'mode-line-format))
    (setq-local cursor-type nil
                mode-line-format nil)))

(defun zw/exwm-scratch-hide-ui ()
  (let ((n-window (length (window-list))))
    (if (and (= n-window 1)
             (string= (buffer-name) "*scratch*")
             (= (buffer-size) 0))
        (zw/exwm-set-scratch-ui nil)
      (zw/exwm-set-scratch-ui t))))

(defun zw/exwm-scratch-transparent-frame ()
  (let ((n-window (length (window-list))))
    (if (and (= n-window 1)
             (string= (buffer-name) "*scratch*")
             (= (buffer-size) 0))
        (zw/set-opacity nil)
      (zw/set-opacity t))))

(advice-add 'zw/exwm-update-title :after (lambda () (zw/set-opacity t)))
(add-hook 'window-configuration-change-hook 'zw/exwm-scratch-transparent-frame)
(with-current-buffer "*scratch*"
  (zw/exwm-set-scratch-ui nil)
  (add-hook 'post-command-hook
            (lambda () (when this-command
                         (zw/exwm-scratch-hide-ui)
                         (zw/exwm-scratch-transparent-frame)))
            nil t))

;; ** modeline
(set-face-attribute 'mode-line nil :box nil)

(defun zw/exwm-modeline-float ()
  '(:eval
    (let ((window-type (if exwm--floating-frame "float" "tile")))
      (concat " " (propertize window-type
                              'help-echo "mouse-1: Toggling floating"
                              'mouse-face 'mode-line-highlight
                              'local-map (make-mode-line-mouse-map 'mouse-1 'exwm-floating-toggle-floating))))))

(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (setq-local mode-line-process
                        (append mode-line-process (list (zw/exwm-modeline-float))))))

;; ** tab bar
(unless (executable-find "polybar")
  (set-face-attribute 'zw/tab-bar-menu-bar nil
                      :background (let ((bg (face-background 'mode-line)))
                                    (pcase (frame-parameter nil 'background-mode)
                                      ('light (doom-darken bg 0.1))
                                      ('dark (doom-lighten bg 0.1))))
                      :weight 'regular)
  (defun zw/tab-bar-format-exwm-workspace ()
    "Produce menu that shows current exwm workspace."
    `((menu-bar menu-item (propertize (format " %d " exwm-workspace-current-index)
                                      'face 'zw/tab-bar-menu-bar)
                tab-bar-menu-bar :help (format "Current EXWM workspace: %d" exwm-workspace-current-index))))

  (setq tab-bar-show t
        tab-bar-format '(zw/tab-bar-format-exwm-workspace
                         tab-bar-separator
                         zw/tab-bar-format-file-path
                         tab-bar-format-align-right
                         tab-bar-separator
                         tab-bar-separator
                         tab-bar-separator
                         zw/tab-bar-format-global))
  (tab-bar-mode 1)
  ;; (add-to-list 'tab-bar-format 'zw/tab-bar-format-function-def 'append)

  ;; time
  (setq display-time-format "%b %-e %a %T %p"
        display-time-interval 1
        display-time-default-load-average nil)
  (display-time-mode 1)

  ;; battery on laptop
  (require 'battery)
  (setq have-battery-status-p
        (let ((perc-charged (assoc ?p (funcall battery-status-function))))
          (and perc-charged
               (not (zerop (string-to-number (cdr perc-charged)))))))
  (when (and have-battery-status-p
             tab-bar-show)
    (display-battery-mode 1)))

;; ** keycast
(use-package keycast
  :config
  (setq keycast-tab-bar-format "%k%c%R "
        keycast-tab-bar-minimal-width 0)
  (add-to-list 'keycast-substitute-alist '(pdf-view-mouse-set-region nil nil))
  (add-to-list 'keycast-substitute-alist '(pdf-util-image-map-mouse-event-proxy nil nil))
  (keycast-tab-bar-mode))

;; ** polybar
(when (executable-find "polybar")
  (let* ((fg (face-foreground 'mode-line))
         (bg (face-background 'mode-line))
         (hl-fg (face-foreground 'mode-line-highlight))
         (hl-bg (face-background 'mode-line-highlight))
         (gn (face-foreground 'success))
         (rd (face-foreground 'error))
         (power-supply (shell-command-to-string "ls -1 /sys/class/power_supply/"))
         (power-lines (split-string power-supply "\n")))

    (setenv "EXWM_BAR_FG" fg)
    (setenv "EXWM_BAR_BG" bg)
    (setenv "EXWM_BAR_BG_ALT" (pcase (frame-parameter nil 'background-mode)
                                ('light (doom-darken bg 0.1))
                                ('dark (doom-lighten bg 0.1))))
    (setenv "EXWM_BAR_HL_FG" hl-fg)
    (setenv "EXWM_BAR_HL_BG" hl-bg)
    (setenv "EXWM_BAR_RED" rd)
    (setenv "EXWM_BAR_GREEN" gn)
    (dolist (line power-lines)
      (cond
       ((string-match "BAT" line)
        (setenv "EXWM_BAR_BATTERY" line))
       ((string-match "AC" line)
        (setenv "EXWM_BAR_ADAPTER" line)))))

  (defun zw/restart-polybar ()
    (interactive)
    (start-process-shell-command "polybar-msg" nil "polybar-msg cmd quit")
    (zw/exwm-run-in-background "polybar panel"))

  (defun zw/exwm-send-polybar-hook (module-name hook-index)
    (call-process-shell-command (format "polybar-msg action %s hook %s" module-name hook-index) nil 0))

  (defun zw/exwm-polybar-buffer-name ()
    (with-current-buffer (window-buffer (selected-window))
      (let* ((tab-name-max (if (buffer-file-name (window-buffer (minibuffer-selected-window)))
                               30 50))
             (tab-name (buffer-name (window-buffer (minibuffer-selected-window)))))
        (truncate-string-to-width
         tab-name tab-name-max nil nil
         zw/tab-bar-ellipsis))))

  (defun zw/exwm-polybar-buffer-path ()
    (with-current-buffer (window-buffer (selected-window))
      (let* ((dir-name (if (buffer-file-name (window-buffer (minibuffer-selected-window)))
                           (abbreviate-file-name default-directory)
                         ""))
             (dir-name-length (length dir-name)))
        (if (< dir-name-length zw/tab-bar-path-max)
            dir-name
          (concat zw/tab-bar-ellipsis
                  "/"
                  (string-join (cdr (split-string (truncate-string-to-width
                                                   dir-name
                                                   dir-name-length
                                                   (- dir-name-length zw/tab-bar-path-max))
                                                  "\\/"))
                               "/"))))))

  (defun zw/exwm-polybar-keycast-key ()
    (let ((key (key-description keycast--this-command-keys)))
      (if (string= key "")
          ""
        (format " %s " key))))

  (defun zw/exwm-polybar-keycast-desc ()
    (if keycast--this-command-desc
        (truncate-string-to-width
         (format "%s" keycast--this-command-desc) 30 nil nil
         "...")
      ""))

  (defun zw/exwm-polybar-update-exwm-workspace ()
    (zw/exwm-send-polybar-hook "exwm-workspace" 0))

  (defun zw/exwm-polybar-update-buffer ()
    (zw/exwm-send-polybar-hook "emacs-buffer-path" 0)
    (zw/exwm-send-polybar-hook "emacs-buffer-name" 0))

  (defun zw/exwm-polybar-update-keycast ()
    (zw/exwm-send-polybar-hook "emacs-keycast-key" 0)
    (zw/exwm-send-polybar-hook "emacs-keycast-desc" 0))

  (setq tab-bar-show nil)
  (tab-bar-mode 1)
  (add-hook 'exwm-workspace-switch-hook #'zw/exwm-polybar-update-exwm-workspace)
  (add-hook 'buffer-list-update-hook 'zw/exwm-polybar-update-buffer)
  (advice-add 'zw/exwm-update-title :after 'zw/exwm-polybar-update-buffer)
  (advice-add 'keycast--update :after 'zw/exwm-polybar-update-keycast))

;; ** exwm systemtray
(require 'exwm-systemtray)
(exwm-systemtray-enable)
(setq exwm-systemtray-background-color (face-background 'mode-line)
      exwm-systemtray-icon-gap 1)

;; ** buffer placement
;; plots
(defvar zw/exwm-plot-buffers
  '("^R_x11.*$"
    "^matplotlib.*$"))

(dolist (buffer zw/exwm-plot-buffers)
  (add-to-list 'display-buffer-alist
               `(,buffer
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . -1)
                 (dedicated . t)
                 (window-height . 0.5))))

;; * exwm tool
;; ** xmodmap
(defun zw/exwm-run-xmodmap ()
  (interactive)
  (shell-command "xmodmap ~/.cache/emacs/Xmodmap"))

;; ** input method
;; use ibus-rime for X11 apps
;; (setenv "GTK_IM_MODULE" "ibus")
;; (setenv "QT_IM_MODULE" "ibus")
;; (setenv "XMODIFIERS" "@im=ibus")
;; (setenv "LC_CTYPE" "zh_CN.UTF-8")
(require 'exwm-xim)
(exwm-xim-enable)
(push ?\C-\\ exwm-input-prefix-keys)
(setenv "GTK_IM_MODULE" "xim")
(setenv "QT_IM_MODULE" "xim")
(setenv "XMODIFIERS" "@im=exwm-xim")
(setenv "CLUTTER_IM_MODULE" "xim")
(setq default-input-method "pyim")

(use-package pyim
  :demand t
  :bind (:map pyim-mode-map
              ("," . pyim-previous-page)
              ("." . pyim-next-page))
  :config
  (setq pyim-page-tooltip 'minibuffer
        pyim-default-scheme 'quanpin
        pyim-page-style 'two-lines
        pyim-page-length 8
        pyim-cloudim 'google)
  (global-set-key (kbd "C-\\") 'toggle-input-method)
  ;; vertico search pinyin
  (defun pyim-orderless-regexp (orig-func component)
    (let ((result (funcall orig-func component)))
      (pyim-cregexp-build result)))
  (advice-add 'orderless-regexp :around #'pyim-orderless-regexp))

(use-package pyim-basedict
  :after pyim
  :config
  (pyim-basedict-enable))

;; ** exwm randr
;; Set the screen resolution (update this to be the correct resolution for your screen!)
(require 'exwm-randr)
(exwm-randr-enable)
(add-hook 'exwm-randr-screen-change-hook #'zw/exwm-set-wallpaper)

(use-package emacs-xrandr
  :straight (:host github :repo "zhenhua-wang/emacs-xrandr"))

;; ** exwm switch to buffer
(defun zw/exwm-switch-to-buffer-annotation (style)
  (with-current-buffer style
    (concat (propertize " " 'display `(space :align-to center))
            (symbol-name major-mode))))

(defun zw/exwm-switch-to-buffer ()
  (interactive)
  (let* ((buffers (seq-filter (lambda (x)
                                (and (not (eq (current-buffer) x))
                                     (not (zw/hidden-buffer-p x))
                                     (not (cl-some 'identity
                                                   (seq-map (lambda (y)
                                                              (string-match y (buffer-name x)))
                                                            zw/exwm-plot-buffers)))
                                     (or (buffer-file-name x)
                                         (with-current-buffer x
                                           (or exwm-class-name
                                               (eq major-mode 'dired-mode))))))
                              (buffer-list)))
         (buffer-names (seq-map 'buffer-name buffers))
         (completion-extra-properties '(:annotation-function zw/exwm-switch-to-buffer-annotation))
         (buffer (completing-read "EXWM switch to buffer: " buffer-names nil t)))
    (switch-to-buffer buffer)))

;; register exwm buffer switch marginalia
(with-eval-after-load "marginalia"
  (add-to-list 'marginalia-prompt-categories '("\\<EXWM switch to buffer\\>" . exwm-buffer))
  (add-to-list 'marginalia-annotator-registry
               '(exwm-buffer marginalia-annotate-exwm-buffer builtin none))
  (defun marginalia-annotate-exwm-buffer (cand)
    (let* ((ann (with-current-buffer cand (symbol-name major-mode)))
           (ann-width (string-width ann)))
      (concat (propertize " " 'display `(space :align-to (- right ,ann-width)))
              (propertize ann 'face 'marginalia-mode)))))

;; add icons
(defun zw/all-the-icons-completion-get-icon (orig-func cand cat)
  (if (eq cat 'exwm-buffer)
      (all-the-icons-completion-get-buffer-icon cand)
    (funcall orig-func cand cat)))

(advice-add 'all-the-icons-completion-get-icon :around #'zw/all-the-icons-completion-get-icon)

;; ** exwm show desktop
(defun zw/exwm-show-desktop ()
  (interactive)
  (if (string= (buffer-name) "*scratch*")
      (switch-to-buffer nil)
    (switch-to-buffer "*scratch*")))

;; ** desktop environment
(use-package desktop-environment
  :after exwm
  :custom
  (desktop-environment-volume-normal-increment "5%+")
  (desktop-environment-volume-normal-decrement "5%-")
  (desktop-environment-brightness-normal-increment "10%+")
  (desktop-environment-brightness-normal-decrement "10%-")
  :config
  (desktop-environment-mode)
  (exwm-input-set-key (kbd "<XF86KbdBrightnessUp>") 'desktop-environment-keyboard-backlight-increment)
  (exwm-input-set-key (kbd "<XF86KbdBrightnessDown>") 'desktop-environment-keyboard-backlight-decrement)
  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>") 'desktop-environment-brightness-increment)
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") 'desktop-environment-brightness-decrement)
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") 'desktop-environment-volume-increment)
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") 'desktop-environment-volume-decrement)
  (exwm-input-set-key (kbd "<XF86AudioMute>") 'desktop-environment-toggle-mute)
  (exwm-input-set-key (kbd "s-#") 'desktop-environment-screenshot)
  (exwm-input-set-key (kbd "s-$") 'desktop-environment-screenshot-part))

;; ** app launcher
(use-package app-launcher
  :straight '(app-launcher :host github :repo "zhenhua-wang/app-launcher"))

;; ** exwm edit
(use-package exwm-edit)

;; * exwm keymap
  ;; ** exwm prefix keys
(setq exwm-input-prefix-keys
      '(?\C-x
        ?\C-u
        ?\C-h
        ?\M-x
        ?\s-`
        ?\M-`
        ?\M-&
        ?\M-:))

  ;; ** exwm x windows simulate keys
(setq exwm-input-simulation-keys
      `((,(kbd "s-r") . ,(kbd "C-r"))
        (,(kbd "s-f") . ,(kbd "C-f"))
        (,(kbd "s--") . ,(kbd "C--"))
        (,(kbd "s-=") . ,(kbd "C-="))
        (,(kbd "s-+") . ,(kbd "C-+"))
        ;; tab
        (,(kbd "s-t") . ,(kbd "C-t"))
        (,(kbd "s-T") . ,(kbd "C-S-t"))
        (,(kbd "s-w") . ,(kbd "C-w"))
        (,(kbd "s-1") . ,(kbd "M-1"))
        (,(kbd "s-2") . ,(kbd "M-2"))
        (,(kbd "s-3") . ,(kbd "M-3"))
        (,(kbd "s-4") . ,(kbd "M-4"))
        (,(kbd "s-5") . ,(kbd "M-5"))
        (,(kbd "s-6") . ,(kbd "M-6"))
        (,(kbd "s-7") . ,(kbd "M-7"))
        (,(kbd "s-8") . ,(kbd "M-8"))
        (,(kbd "s-9") . ,(kbd "M-9"))
        ;; text edit
        (,(kbd "s-<backspace>") . ,(kbd "S-<home> <delete>"))
        (,(kbd "s-a") . ,(kbd "C-a"))
        (,(kbd "s-s") . ,(kbd "C-s"))
        (,(kbd "s-z") . ,(kbd "C-z"))
        (,(kbd "s-Z") . ,(kbd "C-S-z"))
        (,(kbd "s-x") . ,(kbd "C-x"))
        (,(kbd "s-c") . ,(kbd "C-c"))
        (,(kbd "s-v") . ,(kbd "C-v"))
        (,(kbd "C-w") . ,(kbd "C-x"))
        (,(kbd "M-w") . ,(kbd "C-c"))
        (,(kbd "C-y") . ,(kbd "C-v"))
        ;; navigation
        (,(kbd "M-v") . [prior])
        (,(kbd "C-v") . [next])
        (,(kbd "C-a") . ,(kbd "<home>"))
        (,(kbd "C-e") . ,(kbd "<end>"))
        (,(kbd "M-<") . ,(kbd "C-<home>"))
        (,(kbd "M->") . ,(kbd "C-<end>"))
        (,(kbd "C-S-a") . ,(kbd "S-<home>"))
        (,(kbd "C-S-e") . ,(kbd "S-<end>"))))

(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (and exwm-class-name
                       (string= exwm-class-name "weixin"))
              (exwm-input-set-local-simulation-keys
               (append (remove `(,(kbd "s-w") . ,(kbd "C-w")) exwm-input-simulation-keys)
                       `((,(kbd "C-w") . ,(kbd "s-m"))
                         (,(kbd "s-w") . ,(kbd "s-m"))))))))

;; disable simulate keys in kitty
(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (and exwm-class-name
                       (or (string= exwm-class-name "kitty")
                           (string= exwm-class-name "Emacs")))
              (exwm-input-set-local-simulation-keys nil))))

;; ** exwm global keys
(setq exwm-input-global-keys
      `(
        ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
        (,(kbd "s-R") . exwm-reset)
        ;;close current buffer
        (,(kbd "s-q") . (lambda ()
                          (interactive)
                          (if exwm-class-name
                              (when (yes-or-no-p (format "Confirm kill %s? " exwm-class-name))
                                (kill-this-buffer))
                            (kill-this-buffer))))
        ;; window
        (,(kbd "s-D") . zw/exwm-show-desktop)
        (,(kbd "s-m") . bury-buffer)
        (,(kbd "s-}") . enlarge-window-horizontally)
        (,(kbd "s-{") . shrink-window-horizontally)
        (,(kbd "s-^") . enlarge-window)
        (,(kbd "s-<left>") . windmove-left)
        (,(kbd "s-<right>") . windmove-right)
        (,(kbd "s-<up>") . windmove-up)
        (,(kbd "s-<down>") . windmove-down)
        (,(kbd "s-u") . winner-undo)
        (,(kbd "s-U") . winner-redo)
        (,(kbd "s-`") . window-toggle-side-windows)
        ;; update emacs
        (,(kbd "<f5>") . zw/update-emacs-tangle-dotfiles)
        ;; web search
        (,(kbd "s-/") . emacs-websearch)
        ;; Launch applications
        (,(kbd "s-&") . (lambda (command)
                          (interactive (list (read-shell-command "$ ")))
                          (start-process-shell-command command nil command)))
        (,(kbd "s-SPC") . app-launcher-run-app)
        (,(kbd "s-<tab>") . zw/exwm-switch-to-buffer)
        ;; git
        (,(kbd "s-M") . magit-status)
        ;; input
        (,(kbd "C-\\") . toggle-input-method)
        ;; side bar
        (,(kbd "s-b") . dired-jump)
        ;; tab bar
        (,(kbd "s-1") . zw/tab-switch)
        (,(kbd "s-9") . tab-new)
        (,(kbd "s-0") . tab-close)
        ;; vterm
        (,(kbd "s-e") . vterm)
        (,(kbd "s-E") . multi-vterm)
        ;; Switch workspace
        (,(kbd "s-!") . exwm-workspace-switch)
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "M-s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))))

;; ** exwm mode keys
(bind-keys :map exwm-mode-map
           ;; Ctrl+Q will enable the next key to be sent directly
           ("C-q" . exwm-input-send-next-key)
           ;; send C-c to clients
           ("C-c" . nil)
           ("C-c '" . exwm-edit--compose)
           :map vertico-map
           ("s-<tab>" . vertico-next))

;; * exwm enable
(exwm-enable)
(provide 'zw-exwm)
