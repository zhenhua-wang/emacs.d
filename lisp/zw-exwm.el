;; * helper functions
(defun exwm/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun exwm/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun exwm/exwm-update-title ()
  (pcase exwm-class-name
    ("firefoxnightly" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))
    ("firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))
    ("qutebrowser" (exwm-workspace-rename-buffer (format "Qutebrowser: %s" exwm-title)))
    ("mpv" (exwm-workspace-rename-buffer (format "mpv: %s" exwm-title)))
    ("libreoffice-writer" (exwm-workspace-rename-buffer (format "Libre-Writer: %s" exwm-title)))
    ("libreoffice-impress" (exwm-workspace-rename-buffer (format "Libre-Slides: %s" exwm-title)))
    ("Zathura" (exwm-workspace-rename-buffer (format "Zathura: %s" exwm-title)))))

;; This function isn't currently used, only serves as an example how to
;; position a window
(defun exwm/position-window ()
  (let* ((pos (frame-position))
         (pos-x (car pos))
         (pos-y (cdr pos)))
    (exwm-floating-move (- pos-x) (- pos-y))))

(defun exwm/configure-window-by-class ()
  (pcase exwm-class-name
    ;; floating utils
    ("URxvt"
     (exwm-floating-toggle-floating))
    ("Emacs"
     (exwm-floating-toggle-floating))))

;; initialization
(defun exwm/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 0)

  ;; Launch apps that will run in the background
  (exwm/run-in-background "dunst")
  (exwm/run-in-background "nm-applet")
  (exwm/run-in-background "pasystray")
  (exwm/run-in-background "udiskie --no-automount -t")
  (exwm/run-in-background "blueman-applet")
  ;; set ibus to use "system keyboard layout" in advanced setting
  (exwm/run-in-background "ibus-daemon -drxR"))

(defun exwm/set-wallpaper ()
  (when (file-exists-p "~/.cache/emacs/wallpaper.png")
    (push '(alpha-background . 90) default-frame-alist)
    (with-current-buffer "*scratch*"
      ;; (setq-local cursor-type nil
      ;;             mode-line-format nil)
      (display-line-numbers-mode 0))
    (start-process-shell-command
     "feh" nil  "feh --bg-scale ~/.cache/emacs/wallpaper.png")))

;; run xmodmap
(defun exwm/run-xmodmap ()
  (interactive)
  (shell-command "xmodmap ~/.cache/emacs/Xmodmap"))

;; * main
(use-package exwm
  :config
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

  ;; When EXWM starts up, do some extra configuration
  (add-hook 'exwm-init-hook #'exwm/exwm-init-hook)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'exwm/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'exwm/exwm-update-title)

  ;; Configure windows as they're created
  (add-hook 'exwm-manage-finish-hook #'exwm/configure-window-by-class)

  ;; Hide the modeline on all X windows
  (add-hook 'exwm-floating-setup-hook
            (lambda ()
              (exwm-layout-hide-mode-line)))

  ;; Set the screen resolution (update this to be the correct resolution for your screen!)
  (require 'exwm-randr)
  (exwm-randr-enable)
  (add-hook 'exwm-randr-screen-change-hook #'exwm/set-wallpaper)

  ;; set wallpaper
  (exwm/set-wallpaper)

  ;; Load the system tray before exwm-init
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  ;; input method
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

  (exwm-enable))

;; * exwm keymap
(with-eval-after-load "exwm"
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
                         (string= exwm-class-name "kitty"))
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
             ("s-<tab>" . vertico-next)))

;; * misc
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
  (exwm-input-set-key (kbd "C-s-5") '(lambda ()
                                       (interactive)
                                       (desktop-environment-screenshot)
                                       (sleep-for 0.2)
                                       (start-process-shell-command "notify-send" nil "notify-send \"screenshot taken!\"")))
  (exwm-input-set-key (kbd "s-<print>") 'desktop-environment-screenshot-part))

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
(setq display-time-format "%b %e %a %T %p"
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

;; ** input
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

;; ** xrandr
(use-package emacs-xrandr
  :straight (:host github :repo "zhenhua-wang/emacs-xrandr"))

;; ** transparent scratch
(defun zw/transparent-scratch ()
  (let ((n-window (length (cl-delete-duplicates (mapcar #'window-buffer (window-list))))))
    (if (and (= n-window 1)
             (string= (buffer-name) "*scratch*"))
        (progn
          (set-frame-parameter (selected-frame) 'alpha-background 30)
          (pcase (frame-parameter nil 'background-mode)
            ('light (set-face-attribute 'tab-bar nil
                                        :foreground "black"
                                        :background "white"))
            ('dark (set-face-attribute 'tab-bar nil
                                       :foreground "white"
                                       :background "black"))))
      (progn
        (set-frame-parameter (selected-frame) 'alpha-background 90)
        (set-face-attribute 'tab-bar nil
                            :foreground (face-foreground 'default)
                            :background (face-background 'mode-line))))))
(add-to-list 'window-configuration-change-hook 'zw/transparent-scratch)

;; * provide zw-exwm
(provide 'zw-exwm)
