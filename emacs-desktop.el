(defun exwm/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun exwm/set-wallpaper ()
  (interactive)
  ;; a good one! "/usr/share/wallpapers/Next/contents/images/5120x2880.png"
  (start-process-shell-command
   "feh" nil  "feh --bg-scale ~/Pictures/pexels-eberhard-grossgasteiger-1287145.jpg"))

(defun exwm/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)

  ;; Start the Polybar panel
  (exwm/start-panel)

  ;; Launch apps that will run in the background
  (exwm/run-in-background "dunst")
  (exwm/run-in-background "nm-applet")
  ;; (exwm/run-in-background "pasystray")
  (exwm/run-in-background "udiskie --no-automount -t")
  ;; (exwm/run-in-background "blueman-applet")
  )

(defun exwm/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun exwm/exwm-update-title ()
  (pcase exwm-class-name
    ("firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))
    ("qutebrowser" (exwm-workspace-rename-buffer (format "Qutebrowser: %s" exwm-title)))
    ("mpv" (exwm-workspace-rename-buffer (format "mpv: %s" exwm-title)))
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
    ("VirtualBox Manager" (exwm-workspace-move-window 3))
    ("qutebrowser" (exwm-workspace-move-window 2))
    ("URxvt" (exwm-floating-toggle-floating)
     (exwm-layout-toggle-mode-line))
    ("mpv" (exwm-workspace-move-window 2))))

;; This function should be used only after configuring autorandr!
(defun exwm/update-displays ()
  (exwm/run-in-background "autorandr --change --force")
  (exwm/set-wallpaper)
  (message "Display config: %s"
           (string-trim (shell-command-to-string "autorandr --current"))))

(use-package exwm
  :init
  (setq
   ;; Window focus should follow the mouse pointer
   mouse-autoselect-window t
   focus-follows-mouse t
   ;; Automatically send the mouse cursor to the selected workspace's display
   exwm-workspace-warp-cursor t
   ;; Set the default number of workspaces
   exwm-workspace-number 5)
  :config
  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'exwm/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'exwm/exwm-update-title)

  ;; Configure windows as they're created
  (add-hook 'exwm-manage-finish-hook #'exwm/configure-window-by-class)

  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook #'exwm/exwm-init-hook)

  ;; load xmodmap for system key mapping
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")

  ;; Set the screen resolution (update this to be the correct resolution for your screen!)
  (require 'exwm-randr)
  (exwm-randr-enable)
  ;; (start-process-shell-command "xrandr" nil "xrandr --output eDP1 --mode 2880x1800 --pos 0x0 --rotate normal --output DP1 --off --output DP2 --off --output HDMI1 --off --output HDMI2 --off --output HDMI3 --off --output VIRTUAL1 --off")
  (start-process-shell-command "xrandr" nil "xrandr --output eDP1 --primary --mode 2880x1800 --pos 0x1080 --rotate normal --output DP1 --off --output DP2 --off --output HDMI1 --off --output HDMI2 --off --output HDMI3 --mode 2560x1080 --pos 0x0 --rotate normal --output VIRTUAL1 --off")


  ;; This will need to be updated to the name of a display!  You can find
  ;; the names of your displays by looking at arandr or the output of xrandr
  (setq exwm-randr-workspace-monitor-plist '(2 "HDMI3" 3 "Virtual-2"))

  ;; NOTE: Uncomment these lines after setting up autorandr!
  ;; React to display connectivity changes, do initial display update
  ;; (add-hook 'exwm-randr-screen-change-hook #'exwm/update-displays)
  ;; (exwm/update-displays)

  ;; Set the wallpaper after changing the resolution
  (exwm/set-wallpaper)

  ;; Hide the modeline on all X windows
  ;; (exwm-layout-hide-mode-line)

  ;; enable resize from right side
  (setq window-divider-default-right-width 1)
  (window-divider-mode)

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
            ;; ?\C-\M-j  ;; Buffer list
            ;; ?\C-\
            ))  ;; Ctrl+Space

    ;; Ctrl+Q will enable the next key to be sent directly
    (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

    ;; Set up global key bindings.
    (setq exwm-input-global-keys
          `(
            ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
            ;; ([?\s-R] . exwm-reset)
            ([?\s-R] . exwm-input-toggle-keyboard)

            ;; switch buffer
            ([s-tab] . switch-to-buffer)
            ;;close current buffer
            ([?\s-q] . kill-this-buffer)

            ;; resize window
            ([?\s-+] . enlarge-window-horizontally)
            ([?\s--] . shrink-window-horizontally)
            ([?\s-^] . enlarge-window)
            ;; Move between windows
            ([s-left] . windmove-left)
            ([s-right] . windmove-right)
            ([s-up] . windmove-up)
            ([s-down] . windmove-down)
            ([?\s-o] . ace-window)
            ;; winner undo/redo
            ([?\s-u] . winner-undo)
            ([?\s-U] . winner-redo)

            ;; Launch applications via shell command
            ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))

            ;; ([?\s-\ ] . counsel-linux-app)
            ([?\s-\ ] . (lambda ()
                          (interactive)
                          (call-process-shell-command "rofi -show"))) ;; interestingly, start-process-shell-command isn't working in this case

            ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "M-s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))))

    (exwm-input-set-key (kbd "<XF86LaunchA>") 'exwm-workspace-switch)
    (exwm-input-set-key (kbd "s-i") 'zw/get-system-info)
    (exwm-input-set-key (kbd "s-e") 'zw/show-eshell)
    ;; unbind keys in EXWM line-mode

    ))

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
                                       (start-process-shell-command "notify-send" nil "notify-send \"screenshot taken!\""))))

(defvar exwm/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun exwm/kill-panel ()
  (interactive)
  (when exwm/polybar-process
    (ignore-errors
      (kill-process exwm/polybar-process)))
  (setq exwm/polybar-process nil))

(defun exwm/start-panel ()
  (interactive)
  (exwm/kill-panel)
  (setq exwm/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun exwm/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun exwm/send-polybar-exwm-workspace ()
  (exwm/send-polybar-hook "exwm-workspace" 1))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'exwm/send-polybar-exwm-workspace)

;; chinese
(use-package chinese-number
  :config
  (setq chinese-number--use-lowercase t))

(defun exwm/polybar-exwm-workspace-chinese ()
  (car (last (split-string (chinese-number--convert-arabic-to-chinese
                       exwm-workspace-current-index)))))

;; roman
(defun exwm/polybar-exwm-workspace-roman ()
  (pcase exwm-workspace-current-index
    (0 "N")
    (1 "I")
    (2 "II")
    (3 "III")
    (4 "IV")
    (5 "V")))

(defun exwm/dunstctl (command)
  (start-process-shell-command "dunstctl" nil (concat "dunstctl " command)))

(exwm-input-set-key (kbd "s-n") (lambda () (interactive) (exwm/dunstctl "close-all")))
(exwm-input-set-key (kbd "s-m") (lambda () (interactive) (exwm/dunstctl "history-pop")))

(defun exwm/disable-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_PAUSE\""))

(defun exwm/enable-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_RESUME\""))

(defun exwm/toggle-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_TOGGLE\""))

(defun zw/battery-status ()
  "Outputs the battery percentage from acpi."
  (let  ((battery-percent  (replace-regexp-in-string 
                            ".*?\\([0-9.]+\\)%.*" "\\1%"
                            (battery))))
    (floor (string-to-number battery-percent))))

;; I messed up with pcase..
(defun zw/battery-status-icon (battery-percent)
  "Outputs icon based the battery percentage from acpi."
  (cond 
   ((<= battery-percent 10) "")
   ((<= battery-percent 20) "")
   ((<= battery-percent 30) "")
   ((<= battery-percent 40) "")
   ((<= battery-percent 50) "")
   ((<= battery-percent 60) "")
   ((<= battery-percent 70) "")
   ((<= battery-percent 80) "")
   ((<= battery-percent 95) "")
   ((> battery-percent 95)  "")))

;; acpi not working for tempurature
(defun zw/temperature ()
  (replace-regexp-in-string
   ".*? \\([0-9\.]+\\) .*" "Temp: \\1°C "
   (substring (shell-command-to-string "acpi -t") 0 -1)))

(defun zw/get-system-info ()
  (interactive)
  (let ((battery-percent (zw/battery-status)))
    (start-process-shell-command "notify-send" nil
                                 (format "notify-send \"%s: %s%%\n: %s\""
                                         (zw/battery-status-icon battery-percent)
                                         battery-percent
                                         (format-time-string "%I:%M %p" (current-time))))))
