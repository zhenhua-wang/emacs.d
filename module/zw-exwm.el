;; -*- lexical-binding: t -*-

;; * exwm init
(use-package exwm
  :demand t
  :straight (:host github :repo "zhenhua-wang/exwm" :files ("*")))

;; start server for ipc
(server-start)

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
 exwm-floating-border-width 0
 exwm-floating-border-color (face-background 'highlight)
 ;; https://github.com/ch11ng/exwm/issues/924
 x-no-window-manager t)

;; * exwm utils
(defun zw/exwm-get-geometry (conn id)
  (let ((reply (xcb:+request-unchecked+reply conn
                   (make-instance 'xcb:GetGeometry :drawable id))))
    (with-slots (x y width height) reply
      (list (cons 'x x)
            (cons 'y y)
            (cons 'width width)
            (cons 'height height)))))

(defun zw/exwm-set-window-type (conn id type)
  (xcb:+request conn
      (make-instance 'xcb:ewmh:set-_NET_WM_WINDOW_TYPE
                     :window id
                     :data (vector type))))

;; * exwm applications
(defun zw/exwm-run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun zw/exwm-run-apps ()
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
    (zw/exwm-run-in-background "udiskie --automount -t"))
  (when (executable-find "fcitx5")
    (zw/exwm-run-in-background "fcitx5"))
  (when (executable-find "plank")
    (zw/exwm-run-in-background "plank"))
  (when (executable-find "polybar")
    (zw/exwm-run-in-background "polybar panel")))

(add-hook 'exwm-init-hook #'zw/exwm-run-apps)

;; * exwm appearance
;; ** window management
;; *** ewmh window type
;; set ewmh type
(defun zw/exwm-workspace-set-type ()
  ;; desktop
  (zw/exwm-set-window-type exwm--connection
                           (frame-parameter exwm-workspace--current
                                            'exwm-outer-id)
                           xcb:Atom:_NET_WM_WINDOW_TYPE_DESKTOP)
  ;; dock
  (zw/exwm-set-window-type exwm--connection
                           (frame-parameter exwm-workspace--minibuffer
                                            'exwm-container)
                           xcb:Atom:_NET_WM_WINDOW_TYPE_DOCK)
  (xcb:flush exwm--connection))
(add-hook 'exwm-workspace-switch-hook 'zw/exwm-workspace-set-type)

;; *** update title
(defun zw/exwm-update-title ()
  (when exwm-class-name
    (if (and exwm-title
             (string= (downcase exwm-title)
                      (downcase exwm-class-name)))
        (exwm-workspace-rename-buffer exwm-class-name)
      (exwm-workspace-rename-buffer (format "%s: %s" exwm-class-name exwm-title)))))

(add-hook 'exwm-update-class-hook #'zw/exwm-update-title)
(add-hook 'exwm-update-title-hook #'zw/exwm-update-title)

;; *** window config
(defun zw/exwm-float-header-line-rhs ()
  (concat (propertize (zw/exwm-modeline-toggle-window-input)
                      'face 'zw/modeline-process-active)
          " "
          (propertize (zw/exwm-modeline-toggle-window-type)
                      'face 'zw/modeline-process-active)
          " "
          (propertize (zw/exwm-modeline-float-hide)
                      'face 'zw/modeline-process-active)
          " "))

(let* ((panel-height (if (executable-find "polybar") (line-pixel-height) 0))
       (float-width (floor (/ (frame-pixel-width) 1.1)))
       (float-height (floor (/ (frame-pixel-height) 1.1)))
       (float-x (/ (- (frame-pixel-width) float-width) 2))
       (float-y (- (/ (- (frame-pixel-height) float-height) 2) panel-height))
       (float-header-line (list " "
                                '(:eval (propertize (zw/modeline-buffer-name 30 "...")
                                                    'face 'zw/modeline-process-active))
                                '(:eval (zw/modeline-middle-space (zw/exwm-float-header-line-rhs)))
                                '(:eval (zw/exwm-float-header-line-rhs))))
       (default-config (list 'tiling-mode-line nil
                             'tiling-header-line nil
                             'floating-mode-line nil
                             'floating-header-line nil)))
  (setq exwm-manage-configurations
        `(;; plot buffer
          ,(append `((and (zw/exwm-plot-buffer-p exwm-class-name)
                          (cl-some 'identity
                                   (cl-mapcar (lambda (buffer)
                                                (with-current-buffer buffer
                                                  (string= "Emacs" exwm-class-name)))
                                              (buffer-list))))
                     floating t
                     x ,(- (+ float-x float-width)
                           (floor (* float-width 0.3)))
                     y ,float-y
                     width ,(floor (* float-width 0.3))
                     height ,(floor (* float-width 0.3)))
                   default-config)
          ;; floating fixed geometry
          ,(append `((string= "Emacs" exwm-class-name)
                     floating t
                     char-mode t
                     x ,float-x
                     y ,float-y
                     width ,float-width
                     height ,float-height)
                   default-config)
          ;; floating char-mode
          ,(append `((string= "kitty" exwm-class-name)
                     floating t
                     char-mode t
                     max-width ,float-width
                     max-height ,float-height)
                   default-config)
          ;; tiling
          ,(append `((or (zw/exwm-plot-buffer-p exwm-class-name)
                         (and (eq (car exwm-window-type)
                                  xcb:Atom:_NET_WM_WINDOW_TYPE_NORMAL)
                              (member exwm-class-name '("firefox"))))
                     floating nil)
                   default-config)
          ;; default
          ,(append `(t floating t
                       max-width ,float-width
                       max-height ,float-height)
                   default-config))))

(defun zw/exwm-floating-setup-hook ()
  (select-frame-set-input-focus exwm--floating-frame))
(add-hook 'exwm-floating-setup-hook 'zw/exwm-floating-setup-hook)

;; focus last frame after closing floating window
(defun zw/exwm-focus-preview-frame ()
  (when (and exwm--floating-frame
             (frame-live-p zw/previous-frame)
             (frame-visible-p zw/previous-frame))
    (select-frame-set-input-focus zw/previous-frame)))
(add-hook 'kill-buffer-hook 'zw/exwm-focus-preview-frame)

(defun zw/exwm-floating-hide ()
  "Hide the current floating X window and focus previous window."
  (interactive)
  (exwm--log)
  (when (and (derived-mode-p 'exwm-mode)
             exwm--floating-frame)
    (make-frame-invisible exwm--floating-frame)
    (exwm-layout--hide exwm--id)
    (select-frame-set-input-focus
     (if (and (frame-live-p zw/previous-frame)
              (frame-visible-p zw/previous-frame))
         zw/previous-frame
       exwm-workspace--current))))

;; *** buffer config
;; plots
(defvar zw/exwm-plot-buffers
  '("^R_x11.*$"
    "^matplotlib.*$"))

(defun zw/exwm-plot-buffer-p (buffer-or-name)
  (when buffer-or-name
    (let ((buffer (get-buffer buffer-or-name)))
      (cl-some 'identity
               (cl-map 'list (lambda (y)
                               (string-match y (buffer-name buffer)))
                       zw/exwm-plot-buffers)))))

(dolist (buffer zw/exwm-plot-buffers)
  (add-to-list 'display-buffer-alist
               `(,buffer
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . -1)
                 (dedicated . t)
                 (window-height . 0.5))))

;; enable right side window mode for plots
(add-hook 'buffer-list-update-hook
          (lambda ()
            (when (zw/exwm-plot-buffer-p (current-buffer))
              (zw/right-side-window-mode 1))))

;; display buffers
(defun zw/exwm-display-buffer-p (x)
  (and (not (zw/hidden-buffer-p x))
       (with-current-buffer x
         (or (buffer-file-name)
             (eq major-mode 'exwm-mode)
             (eq major-mode 'dired-mode)))))

(defun zw/exwm-buffer-display-list ()
  (cl-remove-if-not 'zw/exwm-display-buffer-p (buffer-list)))

(defun zw/exwm-buffer-visible-p (buffer)
  (or (get-buffer-window buffer)
      (with-selected-frame exwm-workspace--current
        (get-buffer-window buffer))))

(defun zw/exwm-buffer--sort-time (x y)
  (< (with-current-buffer x
       (or zw/exwm-buffer-create-time most-positive-fixnum))
     (with-current-buffer y
       (or zw/exwm-buffer-create-time 0))))

(defun zw/exwm-buffer-sorted-display-list ()
  (sort (zw/exwm-buffer-display-list)
        'zw/exwm-buffer--sort-time))

(defvar-local zw/exwm-buffer-create-time nil)
(defun zw/exwm-set-buffer-create-time ()
  (when (zw/exwm-display-buffer-p (current-buffer))
    (setq-local zw/exwm-buffer-create-time
                (time-convert (current-time) 'integer))))
(add-hook 'find-file-hook 'zw/exwm-set-buffer-create-time)
(add-hook 'exwm-mode-hook 'zw/exwm-set-buffer-create-time)
(add-hook 'polymode-init-inner-hook
          (lambda ()
            (setq-local zw/exwm-buffer-create-time
                        (with-current-buffer (buffer-base-buffer)
                          zw/exwm-buffer-create-time))))

;; ** nerd icon
(defun zw/nerd-icons-get-app-icon (name)
  (let* ((get-icon (lambda (name)
                     (or (ignore-errors (nerd-icons-devicon (format "nf-dev-%s" name)))
                         (ignore-errors (nerd-icons-mdicon (format "nf-md-%s" name)))
                         (ignore-errors (nerd-icons-sucicon (format "nf-seti-%s" name)))
                         (ignore-errors (nerd-icons-sucicon (format "nf-custom-%s" name))))))
         (name-splits (split-string name "[- ]+"))
         (full-name (string-join name-splits "_"))
         (icon (funcall get-icon full-name)))
    (if icon
        icon
      (let* ((name-first (car name-splits)))
        (funcall get-icon name-first)))))

;; ** modeline
;; *** hide float window
(defun zw/exwm-modeline-float-hide ()
  (propertize "▼"
              'help-echo "mouse-1: Hide floating window"
              'mouse-face 'mode-line-highlight
              'local-map (let ((map (make-sparse-keymap)))
                           (define-key map (vector 'mode-line 'mouse-1) 'exwm-floating-hide)
                           (define-key map (vector 'header-line 'mouse-1) 'exwm-floating-hide)
                           map)))

;; *** toggle exwn input type
(defun zw/exwm-modeline-toggle-window-input ()
  (when-let ((exwm-buffer (exwm--id->buffer exwm--id)))
    (let (help-echo cmd mode)
      (with-current-buffer exwm-buffer
        (cl-case exwm--input-mode
          (line-mode
           (setq mode "line"
                 help-echo "mouse-1: Switch to char-mode"
                 cmd (lambda ()
                       (interactive)
                       (exwm-input-release-keyboard exwm--id))))
          (char-mode
           (setq mode "char"
                 help-echo "mouse-1: Switch to line-mode"
                 cmd (lambda ()
                       (interactive)
                       (exwm-input-grab-keyboard exwm--id)))))
        (propertize mode
                    'help-echo help-echo
                    'mouse-face 'mode-line-highlight
                    'local-map (let ((map (make-sparse-keymap)))
                                 (define-key map (vector 'mode-line 'mouse-1) cmd)
                                 (define-key map (vector 'header-line 'mouse-1) cmd)
                                 map))))))

;; *** toggle window type
(defun zw/exwm-modeline-toggle-window-type ()
  (let ((window-type (if exwm--floating-frame "float" "tile")))
    (propertize window-type
                'help-echo "mouse-1: Toggling window type"
                'mouse-face 'mode-line-highlight
                'local-map (let ((map (make-sparse-keymap)))
                             (define-key map (vector 'mode-line 'mouse-1) 'exwm-floating-toggle-floating)
                             (define-key map (vector 'header-line 'mouse-1) 'exwm-floating-toggle-floating)
                             map))))

(advice-add 'exwm-input--update-mode-line :after
            (lambda (&rest args)
              (add-to-list 'mode-line-process
                           '(:eval (concat " " (zw/exwm-modeline-toggle-window-type))) t)))

;; *** config
(set-face-attribute 'mode-line nil :box nil)

;; ** tab bar
(unless (executable-find "polybar")
  (require 'zw-tab-bar)
  (setq tab-bar-show t
        tab-bar-format '(zw/tab-bar-format-exwm-workspace
                         tab-bar-separator
                         zw/tab-bar-format-buffers
                         tab-bar-format-align-right
                         zw/tab-bar-begin
                         tab-bar-separator
                         tab-bar-separator
                         tab-bar-separator
                         zw/tab-bar-format-cpu-temp
                         zw/tab-bar-format-time
                         zw/tab-bar-format-battery)))

(when (executable-find "polybar")
  (let* ((power-supply (shell-command-to-string "ls -1 /sys/class/power_supply/"))
         (power-lines (split-string power-supply "\n")))
    (dolist (line power-lines)
      (cond
       ((string-match "BAT" line)
        (setenv "EXWM_BAR_BATTERY" line))
       ((string-match "AC" line)
        (setenv "EXWM_BAR_ADAPTER" line))))
    (with-eval-after-load "emacs-cpu-temperature"
      (add-hook 'cpu-temperature-mode-hook
                (lambda ()
                  (setenv "EXWM_BAR_TEMP" (substring-no-properties cpu-temperature--thermal-zone 12))))))
  (defun zw/exwm-send-polybar-hook (module-name hook-index)
    (call-process-shell-command (format "polybar-msg action %s hook %s" module-name hook-index) nil 0))
  (defun zw/exwm-polybar-buffer-name ()
    (let* ((display-frame (if (frame-live-p zw/active-frame) zw/active-frame exwm-workspace--current))
           (label (buffer-name (window-buffer (frame-selected-window display-frame))))
           (label-max 50)
           (label-ellipsis "..."))
      (if (> (length label) label-max)
          (truncate-string-to-width
           label label-max nil nil label-ellipsis)
        label)))
  (defun zw/exwm-polybar-update-exwm-workspace ()
    (zw/exwm-send-polybar-hook "exwm-workspace" 0))
  (defun zw/exwm-polybar-update-buffer-name ()
    (zw/exwm-send-polybar-hook "emacs-buffer-name" 0))
  (add-hook 'exwm-workspace-switch-hook #'zw/exwm-polybar-update-exwm-workspace)
  (add-hook 'window-state-change-hook 'zw/exwm-polybar-update-buffer-name)
  (advice-add 'zw/exwm-update-title :after 'zw/exwm-polybar-update-buffer-name))

;; ** minibuffer
(vertico-posframe-mode 0)
(setq exwm-workspace-minibuffer-position 'bottom
      exwm-workspace-display-echo-area-timeout 0.1)
;; detached minibuffer freezes on help message
;; https://github.com/ch11ng/exwm/wiki#minor-issues-related-to-the-autohide-echo-area
(global-eldoc-mode 0)
(which-key-mode 0)
(setq echo-keystrokes 0)
(add-hook 'exwm-init-hook
          (lambda ()
            (set-frame-parameter exwm-workspace--minibuffer 'background-color (face-background 'mode-line))))
;; disable message to prevent freezing
(defun zw/exwm-minibuffer-silence-messages-advice (orig-fun &rest args)
  "Advice function that silences all messages in ORIG-FUN."
  (let ((inhibit-message t)
        (message-log-max nil))
    (apply orig-fun args)))
(dolist (func '(pixel-scroll-precision
                touch-screen-scroll))
  (advice-add func :around 'zw/exwm-minibuffer-silence-messages-advice))
;; if ever stuck in exwm minibuffer, use abort-recursive-edit (c-]) to exit
(defun zw/exwm-minibuffer-and-keyboard-quit ()
  (interactive)
  (if (active-minibuffer-window)
      (abort-recursive-edit)
    (keyboard-quit)))
(defun zw/exwm-toggle-minibuffer ()
  (interactive)
  (let* ((geometry (zw/exwm-get-geometry
                    exwm--connection
                    (frame-parameter exwm-workspace--minibuffer
                                     'exwm-container)))
         (height (alist-get 'height geometry)))
    (if (> height 1)
        (exwm-workspace--hide-minibuffer)
      (exwm-workspace--show-minibuffer))))
;; don't clear echo area after every input
(defun zw/exwm-input--clear-echo-area ()
  (when (current-message)
    (message nil)))
(advice-add 'exwm-workspace--init :after
            (lambda ()
              (remove-hook 'echo-area-clear-hook #'exwm-workspace--on-echo-area-clear)
              (remove-hook 'exwm-input--event-hook #'exwm-workspace--on-echo-area-clear)
              (add-hook 'exwm-input--event-hook #'zw/exwm-input--clear-echo-area)))
(defun zw/exwm-focus-minibuffer ()
  (interactive)
  (select-frame-set-input-focus exwm-workspace--minibuffer))
(defun zw/exwm-focus-main ()
  (interactive)
  (select-frame-set-input-focus exwm-workspace--current))
(defun zw/exwm-window-up ()
  (interactive)
  (if (minibufferp)
      (zw/exwm-focus-main)
    (windmove-up)))
(defun zw/exwm-window-down ()
  (interactive)
  (unless (ignore-errors (windmove-down))
    (if (active-minibuffer-window)
        (zw/exwm-focus-minibuffer)
      (message "No buffer below."))))
(bind-keys :map global-map
           ("s-<up>" . zw/exwm-window-up)
           ("s-<down>" . zw/exwm-window-down)
           :map minibuffer-mode-map
           ("s-<up>" . zw/exwm-focus-main)
           ("<down-mouse-1>" . zw/exwm-focus-minibuffer))

;; ** systemtray
(require 'exwm-systemtray)
(exwm-systemtray-enable)
(setq exwm-systemtray-background-color (face-background 'mode-line)
      exwm-systemtray-icon-gap 1)

;; ** desktop
(defun zw/exwm-set-ui (predicate)
  (with-current-buffer "*scratch*"
    (if predicate
        (progn
          (setq-local cursor-type (default-value 'cursor-type)
                      mode-line-format (default-value 'mode-line-format))
          (set-frame-parameter exwm-workspace--current 'alpha-background 98))
      (progn
        (setq-local cursor-type nil
                    mode-line-format nil)
        (set-frame-parameter exwm-workspace--current 'alpha-background 0)))))

(defun zw/exwm-desktop-window-config ()
  (cond ((or (not (eq (selected-frame) exwm-workspace--current))
             (minibufferp)) nil)
        ((string= (buffer-name) "*scratch*")
         (let ((n-window (length (window-list))))
           (if (and (= n-window 1)
                    (= (buffer-size) 0))
               (zw/exwm-set-ui nil)
             (zw/exwm-set-ui t))))
        (t (zw/exwm-set-ui t))))

(defun zw/exwm-scratch-post-command ()
  (when this-command
    (zw/exwm-desktop-window-config)))

(add-hook 'window-configuration-change-hook 'zw/exwm-desktop-window-config)
(with-current-buffer "*scratch*"
  (display-line-numbers-mode -1)
  (add-hook 'post-command-hook 'zw/exwm-scratch-post-command nil t))

;; ** wallpaper
(defvar zw/exwm-wallpaper-type-regexp
  (rx-to-string
   '(: "." (eval `(or "jpg" "jpeg" "png")) eos) t))
(defvar zw/exwm-wallpaper-regexp
  (concat "wallpaper" zw/exwm-wallpaper-type-regexp))

(defun zw/exwm-show-wallpaper ()
  (interactive)
  (let* ((wallpaper (car (directory-files "~/.cache/emacs/" t zw/exwm-wallpaper-regexp))))
    (call-process-shell-command
     (concat "feh --bg-scale " (or wallpaper "~/.emacs.d/exwm/wallpaper.png"))
     nil 0)))

(defun zw/exwm-set-wallpaper (file)
  (interactive
   (list (read-file-name "Set EXWM wallpaper: "
                         default-directory nil
                         t nil
                         (lambda (file-name)
                           (or (file-directory-p file-name)
                               (string-match zw/exwm-wallpaper-type-regexp file-name))))))
  (when file
    ;; delete old wallpaper
    (dolist (wallpaper-old (directory-files "~/.cache/emacs/" t zw/exwm-wallpaper-regexp))
      (delete-file wallpaper-old))
    ;; set new wallpaper
    (let ((suffix (car (last (split-string file "\\.")))))
      (copy-file file (concat "~/.cache/emacs/wallpaper" "." suffix)))
    (zw/exwm-show-wallpaper)))

(zw/exwm-show-wallpaper)

;; ** Window divider
;; enable divider at right and bottom
(setq window-divider-default-places t)
(window-divider-mode 1)

;; * exwm tool
;; ** input method
;; (require 'exwm-xim)
;; (exwm-xim-enable)
;; (setenv "CLUTTER_IM_MODULE" "xim")
(push ?\C-\\ exwm-input-prefix-keys)
(setenv "GTK_IM_MODULE" "fcitx")
(setenv "QT_IM_MODULE" "fcitx")
(setenv "XMODIFIERS" "@im=fcitx")
(setenv "SDL_IM_MODULE" "fcitx")
(setenv "GLFW_IM_MODULE" "ibus")

;; ** exwm randr
;; Set the screen resolution (update this to be the correct resolution for your screen!)
(require 'exwm-randr)
(exwm-randr-enable)
(add-hook 'exwm-randr-screen-change-hook #'zw/exwm-show-wallpaper)

(use-package emacs-xrandr
  :straight (:host github :repo "zhenhua-wang/emacs-xrandr"))

;; ** exwm switch buffer
(defun zw/exwm-floating-function-run-in-main (&rest args)
  (when exwm--floating-frame
    (select-frame-set-input-focus exwm-workspace--current)))
(dolist (func '(find-file
                switch-to-buffer
                exwm-workspace-switch-to-buffer
                vterm multi-vterm
                zw/dired-sidebar-toggle
                zw/right-side-window-toggle
                magit-status
                helpful-variable
                helpful-callable
                helpful-key))
  (advice-add func :before 'zw/exwm-floating-function-run-in-main))

(defun zw/exwm-floating-function-disable (func &rest args)
  (if exwm--floating-frame
      (message "This command is disabled in floating window")
    (apply func args)))
(dolist (func '(split-window-below
                split-window-right))
  (advice-add func :around 'zw/exwm-floating-function-disable))

(defun zw/exwm--next-buffer (buffer-list buffer-length index)
  (let* ((buffer (nth index buffer-list)))
    (cond
     ;; no other invisible buffers
     ((or (not buffer-list)
          (and (length= buffer-list 1) (eq (car buffer-list) (current-buffer))))
      (zw/exwm-dunst-send-message "-r 99 -i gnome-windows" "Window" "\"No other buffers\""))
     ;; next buffer is visible
     ((get-buffer-window buffer)
      (select-frame-set-input-focus exwm-workspace--current)
      (select-window (get-buffer-window buffer)))
     (t (exwm-workspace-switch-to-buffer buffer)))))

(defvar zw/exwm-next-buffer--idle-time 0.7)
(defvar zw/exwm-next-buffer--list nil)
(defvar zw/exwm-next-buffer--timer nil)
(defun zw/exwm-next-buffer--update-list ()
  ;; clear timer
  (when zw/exwm-next-buffer--timer
    (cancel-timer zw/exwm-next-buffer--timer)
    (setq zw/exwm-next-buffer--timer nil))
  ;; update next-buffer--list
  (setq zw/exwm-next-buffer--list (zw/exwm-buffer-display-list)))
(defun zw/exwm-next-buffer ()
  (interactive)
  ;; update list if any buffer removed/added
  (unless (zw/list-same-elements zw/exwm-next-buffer--list
                                 (zw/exwm-buffer-display-list))
    (zw/exwm-next-buffer--update-list))
  ;; set timer
  (setq zw/exwm-next-buffer--timer
        (run-with-idle-timer zw/exwm-next-buffer--idle-time
                             nil 'zw/exwm-next-buffer--update-list))
  (let* ((buffer-list-old (cl-remove-if-not 'buffer-live-p zw/exwm-next-buffer--list))
         (buffer-list (if (and buffer-list-old
                               (zw/list-same-elements zw/exwm-next-buffer--list
                                                      (zw/exwm-buffer-display-list)))
                          buffer-list-old
                        (zw/exwm-buffer-display-list)))
         (buffer-length (length buffer-list))
         (current-index (cl-position (current-buffer) buffer-list))
         (next-index (if current-index
                         (mod (+ current-index 1) buffer-length)
                       0)))
    (zw/exwm--next-buffer buffer-list buffer-length next-index)))

(defun zw/exwm-switch-to-buffer-annotation (style)
  (with-current-buffer style
    (concat (propertize " " 'display `(space :align-to center))
            (symbol-name major-mode))))

(defun zw/exwm-switch-to-buffer ()
  (interactive)
  (let* ((buffers (cl-remove-if
                   (lambda (x) (or (eq (current-buffer) x)
                                   (zw/exwm-plot-buffer-p x)))
                   (zw/exwm-buffer-display-list)))
         (buffer-names (cl-map 'list 'buffer-name buffers))
         (completion-extra-properties '(:annotation-function zw/exwm-switch-to-buffer-annotation))
         ;; (buffer (completing-read "EXWM switch to buffer: " buffer-names nil t)))
         (buffer (consult--read buffer-names
                                :prompt "EXWM switch to buffer: "
                                :state (consult--exwm-buffer-preview)
                                ;; :preview-key '(:debounce 0.2 any)
                                :require-match t)))
    (exwm-workspace-switch-to-buffer buffer)))

(defvar zw/exwm-switch-to-buffer--idle-sec 0.7)
(defvar zw/exwm-switch-to-buffer--timer nil)
(defun zw/exwm-switch-to-buffer--enter ()
  ;; clear timer
  (when zw/exwm-switch-to-buffer--timer
    (cancel-timer zw/exwm-switch-to-buffer--timer)
    (setq zw/exwm-switch-to-buffer--timer nil))
  ;; select buffer
  (when (active-minibuffer-window)
    (if exwm-workspace-minibuffer-position
        (with-selected-frame exwm-workspace--minibuffer
          (vertico-directory-enter))
      (with-current-buffer (window-buffer (active-minibuffer-window))
        (vertico-directory-enter)))))
(defun zw/exwm-switch-to-buffer-enter ()
  (interactive)
  (if (cl-remove-if (lambda (buf) (eq buf (current-buffer))) (zw/exwm-buffer-display-list))
      (progn
        (setq zw/exwm-switch-to-buffer--timer
              (run-with-idle-timer zw/exwm-switch-to-buffer--idle-sec
                                   nil 'zw/exwm-switch-to-buffer--enter))
        (zw/exwm-switch-to-buffer))
    (zw/exwm-dunst-send-message "-r 99 -i gnome-windows" "Window" "\"No other buffers\"")))

;; preview exwm switch buffer
;; BUG: lost focus in exwm char-mode
(defun consult--exwm-buffer-preview ()
  "Exwm buffer preview function."
  (let ((orig-buf (current-buffer))
        preview-bufs)
    (lambda (action cand)
      (pcase action
        ('exit
         (cl-mapc (lambda (buf) (with-current-buffer buf (exwm-floating-hide)))
                  preview-bufs))
        ('preview
         (let ((buf (or (and cand (get-buffer cand)) orig-buf)))
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (zw/exwm-switch-to-buffer-advice)
               (exwm-workspace-switch-to-buffer buf)
               (when (and exwm--floating-frame (not (eq buf orig-buf)))
                 (add-to-list 'preview-bufs buf))
               (x-focus-frame exwm-workspace--minibuffer)))))))))

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
(defun zw/nerd-icons-completion-get-icon (orig-func cand cat)
  (if (eq cat 'exwm-buffer)
      (let* ((icon (with-current-buffer (get-buffer cand)
                     (or (let* ((icons-alist (nerd-icons-match-to-alist cand nerd-icons-regexp-icon-alist)))
                           (if icons-alist
                               (apply (car icons-alist)
                                      (cdr icons-alist))))
                         (if exwm-class-name
                             (zw/nerd-icons-get-app-icon
                              (downcase exwm-class-name)))))))
        (if icon
            (concat icon " ")
          (nerd-icons-completion-get-icon cand 'buffer)))
    (funcall orig-func cand cat)))
(advice-add 'nerd-icons-completion-get-icon :around #'zw/nerd-icons-completion-get-icon)

;; ** exwm show desktop
(defun zw/exwm-show-desktop ()
  (interactive)
  (zw/exwm-floating-hide-all)
  (if (string= (buffer-name) "*scratch*")
      (switch-to-buffer nil)
    (switch-to-buffer "*scratch*")))

;; ** auto hide float
(defun zw/exwm-floating-hide-all ()
  (interactive)
  (let ((exwm-id-list (mapcar 'car exwm--id-buffer-alist)))
    (dolist (exwm-id exwm-id-list)
      (with-current-buffer (exwm--id->buffer exwm-id)
        (when exwm--floating-frame
          (exwm-layout--hide exwm--id)
          (select-frame-set-input-focus exwm-workspace--current))))))

(defun zw/exwm-hide-float (window)
  (with-current-buffer (window-buffer window)
    (unless (or exwm--floating-frame
                (eq window (active-minibuffer-window)))
      (zw/exwm-floating-hide-all))))

(define-minor-mode exwm-float-auto-hide-mode
  "Auto hide exwm float windows."
  :global t
  (if exwm-float-auto-hide-mode
      (advice-add 'exwm-input--update-focus :before 'zw/exwm-hide-float)
    (advice-remove 'exwm-input--update-focus 'zw/exwm-hide-float)))

;; ** dunst
(defun zw/exwm-dunst-send-message (options summary body)
  (when (executable-find "dunst")
    (call-process-shell-command
     (format "dunstify %s %s %s" options summary body) nil 0)
    ;; return t after message sent
    t))

;; ** desktop environment
(use-package desktop-environment
  :demand t
  :bind ((:map desktop-environment-mode-map
               ("s-l" . nil)
               ("S-<print>" . desktop-environment-screenshot)
               ("<print>" . desktop-environment-screenshot-part)
               ("s-$" . desktop-environment-screenshot-part)))
  :config
  (defun zw/desktop-environment-dunst-advice (dunst-options dunst-summary truncate-p alt-msg func &rest args)
    (let* ((inhibit-message t)
           (message-log-max nil)
           (msg (apply func args)))
      (if alt-msg
          (zw/exwm-dunst-send-message dunst-options dunst-summary alt-msg)
        (when (and msg (length> msg 0))
          (if truncate-p
              (zw/exwm-dunst-send-message dunst-options dunst-summary
                                          (format "\"%s\"" (replace-regexp-in-string "[^[:alnum:]%-]" ""
                                                                                     (car (last (split-string msg))))))
            (zw/exwm-dunst-send-message dunst-options dunst-summary (format "\"%s\"" msg)))))))
  (defun zw/desktop-environment-player-metadata ()
    (let* ((status (desktop-environment--shell-command-to-string "playerctl status"))
           (metadata (desktop-environment--shell-command-to-string "playerctl metadata"))
           (metadata-lines (split-string metadata "\n"))
           (title-rx (rx line-start
                         (group-n 1 (+ anything))
                         (+ space)
                         (+ anything) "title"
                         (+ space)
                         (group-n 2 (+ anything))))
           (artist-rx (rx line-start
                          (+ anything) "artist"
                          (+ space)
                          (group-n 1 (+ anything))))
           (icon-rx (rx line-start
                        (+ anything) "artUrl"
                        (+ space)
                        (group-n 1 (+ anything)))))
      (let (device artist icon title)
        (dolist (line metadata-lines)
          (cond
           ((string-match title-rx line)
            (setq device (capitalize (match-string 1 line))
                  title (match-string 2 line)))
           ((string-match artist-rx line)
            (setq artist (match-string 1 line)))
           ((string-match icon-rx line)
            (setq icon (match-string 1 line)))))
        (if (string= status "Playing")
            (zw/exwm-dunst-send-message
             (format "-r 3 -i %s" icon)
             (format "\"%s - Paused\"" device)
             (format "\"%s\n%s\"" artist title))
          (zw/exwm-dunst-send-message
           (format "-r 3 -i %s" icon)
           (format "\"%s - Playing\"" device)
           (format "\"%s\n%s\"" artist title))))))
  ;; config
  (setq desktop-environment-volume-normal-increment "5%+"
        desktop-environment-volume-normal-decrement "5%-"
        desktop-environment-brightness-normal-increment "10%+"
        desktop-environment-brightness-normal-decrement "10%-"
        desktop-environment-screenshot-command "flameshot full"
        desktop-environment-screenshot-partial-command "flameshot gui")
  ;; volume
  (advice-add 'desktop-environment-toggle-mute :around
              (lambda (func)
                (zw/desktop-environment-dunst-advice "-r 1 -i volume-level-high" "Volume" t nil func)))
  (advice-add 'desktop-environment-volume-set :around
              (lambda (func args)
                (zw/desktop-environment-dunst-advice "-r 1 -i volume-level-high" "Volume" t nil func args)))
  ;; brightness
  (advice-add 'desktop-environment-brightness-set :around
              (lambda (func args)
                (zw/desktop-environment-dunst-advice "-r 2 -i xfpm-brightness-lcd" "Brightness" t nil func args)))
  ;; music
  (advice-add 'desktop-environment-toggle-music :around
              (lambda (func)
                (or (zw/desktop-environment-dunst-advice "-r 3 -i xt7-player-mpv" "Player" nil nil func)
                    (zw/desktop-environment-player-metadata))))
  (advice-add 'desktop-environment-music-previous :around
              (lambda (func)
                (or (zw/desktop-environment-dunst-advice "-r 3 -i xt7-player-mpv" "Player" nil nil func)
                    (zw/exwm-dunst-send-message "-r 3 -i xt7-player-mpv" "Player" "Previous"))))
  (advice-add 'desktop-environment-music-next :around
              (lambda (func)
                (or (zw/desktop-environment-dunst-advice "-r 3 -i xt7-player-mpv" "Player" nil nil func)
                    (zw/exwm-dunst-send-message "-r 3 -i xt7-player-mpv" "Player" "Next"))))
  (desktop-environment-mode))

;; ** app launcher
(use-package app-launcher
  :straight '(app-launcher :host github :repo "zhenhua-wang/app-launcher"))

;; add nerd-icons-completion support
(defun app-launcher-nerd-icons-completion-get-icon (orig-func cand cat)
  (if (eq cat 'linux-app)
      (let* ((name (downcase cand))
             (icon (or (zw/nerd-icons-get-app-icon name)
                       (apply (car nerd-icons-default-file-icon)
                              (cdr nerd-icons-default-file-icon)))))
        (concat icon " "))
    (funcall orig-func cand cat)))
(advice-add 'nerd-icons-completion-get-icon :around #'app-launcher-nerd-icons-completion-get-icon)

(defun zw/launch-app ()
  (interactive)
  (if (executable-find "rofi")
      (call-process-shell-command "rofi -show combi -dpi 1")
    (app-launcher-run-app)))

;; ** CPU temperature
(use-package emacs-cpu-temperature
  :demand t
  :straight (:host github :repo "zhenhua-wang/emacs-cpu-temperature")
  :hook ((exwm-init . cpu-temperature-mode)
         (cpu-temperature-mode . zw/exwm-cpu-temperature-init))
  :config
  (setq cpu-temperature-update-interval 1)
  (defun zw/exwm-cpu-temperature-init ()
    (unless cpu-temperature--thermal-zone
      (setq cpu-temperature-thermal-zone-type "acpitz")
      (cpu-temperature-set-thermal-zone))))

;; ** winner mode
(defun zw/winner-remove-dead-frame (&rest args)
  "Remove dead frames"
  (setq winner-modified-list (cl-remove-if-not #'frame-live-p winner-modified-list))
  (setq winner-currents (cl-remove-if-not #'frame-live-p winner-currents)))
(advice-add 'winner-save-old-configurations :before #'zw/winner-remove-dead-frame)

;; ** keycast
(use-package keycast
  :config
  (add-to-list 'keycast-substitute-alist '(pdf-view-mouse-set-region nil nil))
  (add-to-list 'keycast-substitute-alist '(pdf-util-image-map-mouse-event-proxy nil nil))
  (add-to-list 'keycast-substitute-alist '(zw/tab-bar-touchscreen-tab-select nil nil)))

;; * exwm keymap
;; ** exwm prefix keys
(setq exwm-input-prefix-keys
      '(?\C-x
        ?\C-u
        ?\C-h
        ?\M-x
        ?\M-&
        ?\M-:))

;; ** exwm simulate keys
(setq exwm-input-simulation-keys
      `((,(kbd "s-r") . ,(kbd "C-r"))
        (,(kbd "s-f") . ,(kbd "C-f"))
        (,(kbd "s--") . ,(kbd "C--"))
        (,(kbd "s-=") . ,(kbd "C-="))
        (,(kbd "s-p") . ,(kbd "C-p"))
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
        (,(kbd "M-<") . ,(kbd "C-<home>"))
        (,(kbd "M->") . ,(kbd "C-<end>"))
        (,(kbd "C-v") . [next])
        (,(kbd "C-a") . ,(kbd "<home>"))
        (,(kbd "C-e") . ,(kbd "<end>"))
        (,(kbd "C-S-a") . ,(kbd "S-<home>"))
        (,(kbd "C-S-e") . ,(kbd "S-<end>"))))

;; ** exwm global keys
(setq exwm-input-global-keys
      `(;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
        (,(kbd "s-R") . exwm-reset)
        ;; window
        (,(kbd "s-F") . exwm-floating-toggle-floating)
        (,(kbd "s-D") . zw/exwm-show-desktop)
        (,(kbd "s-m") . (lambda ()
                          (interactive)
                          (if exwm--floating-frame (zw/exwm-floating-hide) (bury-buffer))))
        (,(kbd "s-M") . zw/exwm-floating-hide-all)
        (,(kbd "s-+") . enlarge-window-horizontally)
        (,(kbd "s-_") . shrink-window-horizontally)
        (,(kbd "s-^") . enlarge-window)
        (,(kbd "s-u") . winner-undo)
        (,(kbd "s-U") . winner-redo)
        ;; side bar
        (,(kbd "s-b") . zw/dired-sidebar-toggle)
        (,(kbd "s-B") . zw/right-side-window-toggle)
        ;; mininbuffer
        (,(kbd "s-<down>") . zw/exwm-window-down)
        (,(kbd "s-<escape>") . zw/exwm-toggle-minibuffer)
        (,(kbd "S-s-<escape>") . exwm-workspace-toggle-minibuffer)
        ;; tab-bar
        (,(kbd "s-k") . keycast-tab-bar-mode)
        ;; update emacs
        (,(kbd "<f5>") . zw/update-emacs-tangle-dotfiles)
        ;; web search
        (,(kbd "s-l") . emacs-websearch)
        ;; Launch applications
        (,(kbd "s-<return>") . (lambda (command)
                                 (interactive (list (read-shell-command "$ ")))
                                 (recentf-save-list)
                                 (save-some-buffers)
                                 (async-shell-command command)))
        (,(kbd "s-SPC") . zw/launch-app)
        (,(kbd "s-<tab>") . zw/exwm-switch-to-buffer)
        ;; git
        (,(kbd "s-G") . magit-status)
        ;; vterm
        (,(kbd "s-e") . vterm)
        (,(kbd "s-E") . multi-vterm)
        ;; switch buffer
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%s" (zw/translate-shift-number i))) .
                      (lambda ()
                        (interactive)
                        (zw/tab-bar-switch-to-buffer ,i))))
                  (number-sequence 1 9))
        ;; switch workspace
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
           ;;close current buffer
           ("s-q" . (lambda ()
                      (interactive)
                      (when (y-or-n-p (format "Confirm kill %s? " exwm-class-name))
                        (kill-this-buffer))))
           ;; window
           ("s-<left>" . windmove-left)
           ("s-<right>" . windmove-right)
           ("s-<up>" . zw/exwm-window-up)
           ("s-<down>" . zw/exwm-window-down)
           :map vertico-map
           ("s-<tab>" . vertico-next)
           ("s-`" . vertico-next))

;; * exwm enable
(exwm-enable)
;; clear echo area
(message nil)
