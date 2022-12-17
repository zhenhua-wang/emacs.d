(defun exwm/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun exwm/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun exwm/exwm-update-title ()
  (pcase exwm-class-name
    ("firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))
    ("qutebrowser" (exwm-workspace-rename-buffer (format "Qutebrowser: %s" exwm-title)))
    ("mpv" (exwm-workspace-rename-buffer (format "mpv: %s" exwm-title)))
    ("libreoffice-writer" (exwm-workspace-rename-buffer (format "Libre-Writer: %s" exwm-title)))
    ("libreoffice-impress" (exwm-workspace-rename-buffer (format "Libre-Slides: %s" exwm-title)))
    ("okular" (exwm-workspace-rename-buffer (format "Okular: %s" exwm-title)))
    ("Zathura" (exwm-workspace-rename-buffer (format "Zathura: %s" exwm-title)))
    ("Evince" (exwm-workspace-rename-buffer (format "Evince: %s" exwm-title)))))

;; This function isn't currently used, only serves as an example how to
;; position a window
(defun exwm/position-window ()
  (let* ((pos (frame-position))
         (pos-x (car pos))
         (pos-y (cdr pos)))
    (exwm-floating-move (- pos-x) (- pos-y))))

(defun exwm/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ;; media
    ("mpv"
     (exwm-workspace-move-window 3)
     (exwm-workspace-switch-create 3))
    ;; social
    ("Prospect Mail"
     (exwm-workspace-move-window 4)
     (exwm-workspace-switch-create 4))
    ("discord"
     (exwm-workspace-move-window 4)
     (exwm-workspace-switch-create 4))
    ("Slack"
     (exwm-workspace-move-window 4)
     (exwm-workspace-switch-create 4))
    ("Qq"
     (exwm-workspace-move-window 4)
     (exwm-workspace-switch-create 4))
    ;; vm
    ("VirtualBox Manager"
     (exwm-workspace-move-window 5)
     (exwm-workspace-switch-create 5))
    ;; floating utils
    ("URxvt"
     (exwm-floating-toggle-floating))
    ("Emacs"
     (exwm-floating-toggle-floating))))

;; This function should be used only after configuring autorandr!
(defun exwm/update-displays ()
  (exwm/run-in-background "autorandr --change --force")
  (message "Display config: %s"
           (string-trim (shell-command-to-string "autorandr --current"))))

;; initialization
(defun exwm/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)

  ;; Launch apps that will run in the background
  (exwm/run-in-background "dunst")
  (exwm/run-in-background "nm-applet")
  (exwm/run-in-background "pasystray")
  (exwm/run-in-background "udiskie --no-automount -t")
  (exwm/run-in-background "ibus-daemon -drxR")
  (exwm/run-in-background "blueman-applet")
  )

(use-package exwm
  :config
  (require 'exwm)
  (setq
   ;; Window focus should follow the mouse pointer
   mouse-autoselect-window nil
   focus-follows-mouse nil
   ;; Automatically send the mouse cursor to the selected workspace's display
   exwm-workspace-warp-cursor t
   ;; Set the default number of workspaces
   exwm-workspace-number 6
   ;; show buffer in all workspace
   exwm-workspace-show-all-buffers nil
   ;; able to move to buffer in inactive space
   exwm-layout-show-all-buffers nil)

  ;; When EXWM starts up, do some extra confifuration
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

  ;; Load the system tray before exwm-init
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  (exwm-enable))

;; These keys should always pass through to Emacs
(eval-after-load 'exwm
  (lambda ()
    (setq exwm-input-prefix-keys
          '(?\C-x
            ;; ?\C-u
            ?\C-h
            ?\M-x
            ?\s-`
            ?\M-`
            ?\M-&
            ?\M-:
            ?\C-\\
            ;; ?\C-\M-j  ;; Buffer list
            ;; ?\C-\
            ))  ;; Ctrl+Space

    ;; Ctrl+Q will enable the next key to be sent directly
    (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

    ;; Set up global key bindings.
    (setq exwm-input-global-keys
          `(
            ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
            ([?\s-R] . exwm-reset)

            ;; switch buffer
            ([s-tab] . switch-to-buffer)
            ;;close current buffer
            ([?\s-q] . kill-this-buffer)

            ;; resize window
            ([?\s-}] . enlarge-window-horizontally)
            ([?\s-{] . shrink-window-horizontally)
            ([?\s-^] . enlarge-window)
            ;; Move between windows
            ([s-left] . windmove-left)
            ([s-right] . windmove-right)
            ([s-up] . windmove-up)
            ([s-down] . windmove-down)
            ;; winner undo/redo
            ([?\s-u] . winner-undo)
            ([?\s-U] . winner-redo)

            ;; Launch applications via shell command
            ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))

            ;; rofi
            ([?\s-\ ] . (lambda ()
                          (interactive)
                          (call-process-shell-command "rofi -show")))

            ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "M-s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))))

    (exwm-input-set-key (kbd "s-e") 'vterm)
    (exwm-input-set-key (kbd "s-E") 'multi-vterm)))

;; desktop environment
(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-")
  (desktop-environment-keyboard-backlight-normal-increment 70)
  (desktop-environment-keyboard-backlight-normal-decrement -70)
  :config
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

(provide 'zw-exwm)
