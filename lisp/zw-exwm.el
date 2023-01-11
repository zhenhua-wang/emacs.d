;; * exwm init
(straight-use-package 'exwm)

(setq
 ;; Window focus should follow the mouse pointer
 mouse-autoselect-window nil
 focus-follows-mouse nil
 ;; Automatically send the mouse cursor to the selected workspace's display
 exwm-workspace-warp-cursor t
 ;; Set the default number of workspaces
 exwm-workspace-number 1
 ;; show buffer in all workspace
 exwm-workspace-show-all-buffers nil
 ;; able to move to buffer in inactive space
 exwm-layout-show-all-buffers nil)

;; * exwm applications
(defun zw/exwm-run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

;; initialization
(defun zw/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 0)

  ;; Launch apps that will run in the background
  (zw/exwm-run-in-background "dunst")
  (zw/exwm-run-in-background "nm-applet")
  (zw/exwm-run-in-background "pasystray")
  (zw/exwm-run-in-background "udiskie --no-automount -t")
  (zw/exwm-run-in-background "blueman-applet")
  ;; set ibus to use "system keyboard layout" in advanced setting
  (zw/exwm-run-in-background "ibus-daemon -drxR")
  (zw/exwm-run-in-background "polybar panel"))

;; When EXWM starts up, do some extra configuration
(add-hook 'exwm-init-hook #'zw/exwm-init-hook)

;; * exwm appearance
;; ** window management
(defun zw/exwm-update-title ()
  (if (and exwm-title
           (string= (downcase exwm-title)
                    (downcase exwm-class-name)))
      (exwm-workspace-rename-buffer (capitalize exwm-class-name))
    (exwm-workspace-rename-buffer (format "%s: %s" (capitalize exwm-class-name) exwm-title))))

;; When window "class" updates, use it to set the buffer name
(add-hook 'exwm-update-class-hook #'zw/exwm-update-title)

;; When window title updates, use it to set the buffer name
(add-hook 'exwm-update-title-hook #'zw/exwm-update-title)

(defun zw/exwm-configure-window-by-class ()
  (pcase exwm-class-name
    ;; floating utils
    ("URxvt"
     (exwm-floating-toggle-floating))
    ("Emacs"
     (exwm-floating-toggle-floating))))

;; Configure windows as they're created
(add-hook 'exwm-manage-finish-hook #'zw/exwm-configure-window-by-class)

;; This function isn't currently used, only serves as an example how to
;; position a window
(defun zw/exwm-position-window ()
  (let* ((pos (frame-position))
         (pos-x (car pos))
         (pos-y (cdr pos)))
    (exwm-floating-move (- pos-x) (- pos-y))))

;; Hide the modeline on all floating X windows
(add-hook 'exwm-floating-setup-hook (lambda () (exwm-layout-hide-mode-line)))

;; ** wallpaper
(defun zw/exwm-set-wallpaper ()
  (when (file-exists-p "~/.cache/emacs/wallpaper.png")
    (with-current-buffer "*scratch*"
      ;; (setq-local cursor-type nil
      ;;             mode-line-format nil)
      (display-line-numbers-mode 0))
    (start-process-shell-command
     "feh" nil  "feh --bg-scale ~/.cache/emacs/wallpaper.png")))

;; set wallpaper
(zw/exwm-set-wallpaper)

;; ** exwm systemtray
(require 'exwm-systemtray)
(exwm-systemtray-enable)
(setq exwm-systemtray-background-color "#2e3440"
      exwm-systemtray-icon-gap 1)

;; ** tab bar
(setq tab-bar-show t
      tab-bar-format '(tab-bar-separator
                       zw/tab-bar-format-menu-bar
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
  (display-battery-mode 1))

;; keycast
(use-package keycast
  :config
  (setq keycast-tab-bar-format "%k%c%R "
        keycast-tab-bar-minimal-width 0)
  (add-to-list 'keycast-substitute-alist '(pdf-view-mouse-set-region nil nil))
  (add-to-list 'keycast-substitute-alist '(pdf-util-image-map-mouse-event-proxy nil nil))
  (keycast-tab-bar-mode))

;; ** transparency
(defun zw/set-transparency (predicate)
  (if predicate
      (progn
        (setq-local cursor-type nil)
        (set-frame-parameter (selected-frame) 'alpha-background 0)
        (pcase (frame-parameter nil 'background-mode)
          ('light (set-face-attribute 'tab-bar nil
                                      :foreground "black"
                                      :background "white"))
          ('dark (set-face-attribute 'tab-bar nil
                                     :foreground "white"
                                     :background "black"))))
    (progn
      (setq-local cursor-type (default-value 'cursor-type))
      (set-frame-parameter (selected-frame) 'alpha-background 90)
      (set-face-attribute 'tab-bar nil
                          :foreground (face-foreground 'default)
                          :background (face-background 'mode-line)))))

(defun zw/exwm-transparent-scratch-post-command ()
  (let ((n-window (length (mapcar #'window-buffer (window-list)))))
    (if (and (= n-window 1)
             (string= (buffer-name) "*scratch*")
             (= (buffer-size) 0))
        (zw/set-transparency t)
      (zw/set-transparency nil))))

(add-hook 'window-configuration-change-hook 'zw/exwm-transparent-scratch-post-command)
(add-hook 'window-state-change-hook 'zw/exwm-transparent-scratch-post-command)
(add-hook 'exwm-update-class-hook 'zw/exwm-transparent-scratch-post-command)
(with-current-buffer "*scratch*"
  (add-hook 'post-command-hook #'zw/exwm-transparent-scratch-post-command nil t))

;; ** polybar
(defun zw/restart-polybar ()
  (interactive)
  (start-process-shell-command "polybar-msg" nil "polybar-msg cmd quit")
  (zw/exwm-run-in-background "polybar panel"))

(defun zw/exwm-polybar-update-buffer ()
  (zw/exwm-send-polybar-hook "emacs-buffer-path" 1)
  (zw/exwm-send-polybar-hook "emacs-buffer-name" 1))

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

(defun zw/exwm-send-polybar-hook (module-name hook-index)
  (call-process-shell-command (format "polybar-msg hook %s %s" module-name hook-index) nil 0))

(when (executable-find "polybar")
  (setq tab-bar-show nil)
  (tab-bar-mode 1)
  (add-hook 'window-configuration-change-hook 'zw/exwm-polybar-update-buffer)
  (add-hook 'window-state-change-hook 'zw/exwm-polybar-update-buffer)
  (add-hook 'exwm-manage-finish-hook 'zw/exwm-polybar-update-buffer)
  (advice-add 'zw/exwm-update-title :after 'zw/exwm-polybar-update-buffer)
  (advice-add 'keycast--update :after (lambda () (zw/exwm-send-polybar-hook "emacs-keycast-key" 1)))
  (advice-add 'keycast--update :after (lambda () (zw/exwm-send-polybar-hook "emacs-keycast-desc" 1))))

;; * exwm tool
;; ** xmodmap
(defun zw/exwm-run-xmodmap ()
  (interactive)
  (shell-command "xmodmap ~/.cache/emacs/Xmodmap"))

;; ** input method
;; use ibus-rime for X11 apps
(setenv "GTK_IM_MODULE" "ibus")
(setenv "QT_IM_MODULE" "ibus")
(setenv "XMODIFIERS" "@im=ibus")
(setenv "LC_CTYPE" "zh_CN.UTF-8")
;; (require 'exwm-xim)
;; (exwm-xim-enable)
;; (push ?\C-\\ exwm-input-prefix-keys)
;; (setenv "GTK_IM_MODULE" "xim")
;; (setenv "QT_IM_MODULE" "xim")
;; (setenv "XMODIFIERS" "@im=exwm-xim")
;; (setenv "CLUTTER_IM_MODULE" "xim")

(use-package pyim
  :config
  (setq default-input-method "pyim"
        pyim-page-tooltip 'posframe
        pyim-default-scheme 'quanpin
        pyim-page-style 'two-lines
        pyim-page-length 8)
  (global-set-key (kbd "C-\\") 'toggle-input-method))

(use-package pyim-basedict
  :config
  (pyim-basedict-enable))

;; ** exwm randr
;; Set the screen resolution (update this to be the correct resolution for your screen!)
(require 'exwm-randr)
(exwm-randr-enable)
(add-hook 'exwm-randr-screen-change-hook #'zw/exwm-set-wallpaper)

(use-package emacs-xrandr
  :straight (:host github :repo "zhenhua-wang/emacs-xrandr"))

;; ** desktop environment
(use-package desktop-environment
  :after exwm
  :custom
  (desktop-environment-volume-normal-increment "5%+")
  (desktop-environment-volume-normal-decrement "5%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-")
  (desktop-environment-keyboard-backlight-normal-increment 70)
  (desktop-environment-keyboard-backlight-normal-decrement -70)
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
        ;; update emacs
        (,(kbd "<f5>") . zw/update-emacs-tangle-dotfiles)
        ;; web search
        (,(kbd "s-/") . emacs-websearch)
        ;; Launch applications via shell command
        (,(kbd "s-&") . (lambda (command)
                          (interactive (list (read-shell-command "$ ")))
                          (start-process-shell-command command nil command)))
        ;; rofi
        (,(kbd "s-SPC") . (lambda ()
                            (interactive)
                            (call-process-shell-command "rofi -show")))
        ;; rofi switch window
        ;; (,(kbd "s-<tab>") . (lambda ()
        ;;                       (interactive)
        ;;                       (call-process-shell-command "rofi -show window")))
        (,(kbd "s-<tab>") . switch-to-buffer)
        (,(kbd "C-M-;") . magit-status)
        ;; side bar
        (,(kbd "s-b") . dired-jump)
        ;; tab bar
        (,(kbd "s-1") . zw/tab-switch)
        (,(kbd "s-9") . tab-new)
        (,(kbd "s-0") . tab-close)
        ;; vterm
        (,(kbd "s-e") . vterm)
        (,(kbd "s-E") . multi-vterm)
        ;; Switch workspace (M-s-num)
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
           :map vertico-map
           ("s-<tab>" . vertico-next))

;; * exwm enable
(exwm-enable)
(provide 'zw-exwm)
