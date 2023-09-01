;; -*- lexical-binding: t -*-
;; * exwm init
(use-package exwm
  :demand t
  :straight (:host github :repo "ch11ng/exwm" :files ("*")))

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
 exwm-floating-border-width 4
 exwm-floating-border-color (face-background 'highlight))

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
    (zw/exwm-run-in-background "udiskie --no-automount -t")))

(add-hook 'exwm-init-hook #'zw/exwm-run-apps)

;; * exwm appearance
;; ** Color
(defgroup zw/exwm-ui nil
  "zw/exwm-ui."
  :group 'convenience)

(defcustom zw/exwm-ui-fg "#bbc2cf"
  "zw/exwm ui foreground."
  :group 'zw/exwm-ui
  :type 'string)

(defcustom zw/exwm-ui-bg "#21242b"
  "zw/exwm ui background."
  :group 'zw/exwm-ui
  :type 'string)

(defcustom zw/exwm-ui-fg-alt "#3f444a"
  "zw/exwm ui alternative foreground."
  :group 'zw/exwm-ui
  :type 'string)

(defcustom zw/exwm-ui-bg-alt (doom-lighten zw/exwm-ui-bg 0.1)
  "zw/exwm ui alternative background."
  :group 'zw/exwm-ui
  :type 'string)

(defcustom zw/exwm-ui-bg-hl "#42444a"
  "zw/exwm ui background current line."
  :group 'zw/exwm-ui
  :type 'string)

(defcustom zw/exwm-ui-highlight "#51afef"
  "zw/exwm ui highlight."
  :group 'zw/exwm-ui
  :type 'string)

(defun zw/exwm-ui-set-color ()
  (set-face-foreground 'tab-bar zw/exwm-ui-fg)
  (set-face-background 'tab-bar zw/exwm-ui-bg)
  (set-face-foreground 'keycast-key "#000000")
  (set-face-background 'keycast-key zw/exwm-ui-highlight)
  (set-face-foreground 'keycast-command zw/exwm-ui-highlight))
(add-hook 'exwm-init-hook 'zw/exwm-ui-set-color)

;; ** window management
;; *** X window
;; **** update title
(defun zw/exwm-update-title ()
  (if (and exwm-title
           (string= (downcase exwm-title)
                    (downcase exwm-class-name)))
      (exwm-workspace-rename-buffer exwm-class-name)
    (exwm-workspace-rename-buffer (format "%s: %s" exwm-class-name exwm-title))))

(add-hook 'exwm-update-class-hook #'zw/exwm-update-title)
(add-hook 'exwm-update-title-hook #'zw/exwm-update-title)

;; **** window config
(defun zw/exwm-float-header-line-rhs ()
  (concat (propertize (zw/exwm-modeline-toggle-window-input)
                      'face 'zw/modeline-process-active)
          " "
          (propertize (zw/exwm-modeline-toggle-window-type)
                      'face 'zw/modeline-process-active)
          " "
          (propertize (zw/exwm-modeline-float-hide)
                      'face 'zw/modeline-process-active)))

(let* ((float-width (floor (/ (frame-pixel-width) 1.1)))
       (float-height (floor (/ (frame-pixel-height) 1.1)))
       (float-x (/ (- (frame-pixel-width) float-width) 2))
       (float-y (/ (- (frame-pixel-height) float-height) 2))
       (float-header-line (list '(:eval (propertize (zw/tab-bar-tab-name)
                                                    'face 'zw/modeline-process-active))
                                '(:eval (zw/modeline-middle-space (zw/exwm-float-header-line-rhs)))
                                '(:eval (zw/exwm-float-header-line-rhs)))))
  (setq exwm-manage-configurations
        `(((string= "Emacs" exwm-class-name)
           x ,float-x
           y ,float-y
           width ,float-width
           height ,float-height
           floating t
           char-mode t
           floating-mode-line nil
           floating-header-line nil)
          ((and (zw/exwm-plot-buffer-p exwm-class-name)
                (cl-some 'identity
                         (mapcar (lambda (buffer)
                                   (with-current-buffer buffer
                                     (string= "Emacs" exwm-class-name)))
                                 (buffer-list))))
           x ,(- (+ float-x float-width)
                 (floor (* float-width 0.3)))
           y ,float-y
           width ,(floor (* float-width 0.3))
           height ,(floor (* float-width 0.3))
           floating t
           floating-mode-line nil
           floating-header-line nil)
          (t floating-header-line nil
             floating-mode-line nil))))

;; **** auto hide float
(defun zw/exwm-hide-float (window)
  (with-current-buffer (window-buffer window)
    (unless (or exwm--floating-frame
                (eq window (active-minibuffer-window)))
      (let ((exwm-id-list (mapcar 'car exwm--id-buffer-alist)))
        (dolist (exwm-id exwm-id-list)
          (with-current-buffer (exwm--id->buffer exwm-id)
            (when exwm--floating-frame
              (exwm-layout--hide exwm--id)
              (select-frame-set-input-focus exwm-workspace--current))))))))

(define-minor-mode exwm-float-auto-hide-mode
  "Auto hide exwm float windows."
  :global t
  (if exwm-float-auto-hide-mode
      (advice-add 'exwm-input--update-focus :before 'zw/exwm-hide-float)
    (advice-remove 'exwm-input--update-focus 'zw/exwm-hide-float)))

(exwm-float-auto-hide-mode 1)

;; *** emacs buffer
;; plots
(defvar zw/exwm-plot-buffers
  '("^R_x11.*$"
    "^matplotlib.*$"))

(defun zw/exwm-plot-buffer-p (buffer-or-name)
  (let ((buffer (get-buffer buffer-or-name)))
    (cl-some 'identity
             (seq-map (lambda (y)
                        (string-match y (buffer-name buffer)))
                      zw/exwm-plot-buffers))))

(dolist (buffer zw/exwm-plot-buffers)
  (add-to-list 'display-buffer-alist
               `(,buffer
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . -1)
                 (dedicated . t)
                 (window-height . 0.5)))
  (add-to-list 'zw/side-window-buffer-regex buffer))

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
(set-face-attribute 'mode-line nil :box nil)

(defun zw/exwm-modeline-float-hide ()
  (propertize "[-]"
              'help-echo "mouse-1: Hide floating window"
              'mouse-face 'mode-line-highlight
              'local-map (let ((map (make-sparse-keymap)))
                           (define-key map (vector 'mode-line 'mouse-1) 'exwm-floating-hide)
                           (define-key map (vector 'header-line 'mouse-1) 'exwm-floating-hide)
                           map)))

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
(add-hook 'exwm-manage-finish-hook 'zw/toggle-presentation)

;; ** tab bar
(require 'zw-tab-bar)
(defun zw/tab-bar-format-exwm-workspace ()
  "Produce menu that shows current exwm workspace."
  `((global menu-item ,(propertize (format " %d " exwm-workspace-current-index)
                                   'face `(:background ,zw/exwm-ui-bg-alt :weight regular))
            nil :help ,(format "Current EXWM workspace: %d" exwm-workspace-current-index))))

(defun zw/tab-bar-format-buffers ()
  (let* ((i 0)
         (buffer-name-ellipsis ".")
         (buffer-separator " | ")
         (screen-width (frame-width))
         (buffer-name-sort-func (lambda (x y) (string< (buffer-name x) (buffer-name y))))
         (buffer-list (sort (zw/exwm-switch-to-buffer-list) buffer-name-sort-func))
         (buffer-list-length (length buffer-list))
         (buffer-name-max (when (> buffer-list-length 0)
                            (- (/ screen-width buffer-list-length 2)
                               (length buffer-separator)
                               5))))
    (mapcan
     (lambda (buffer)
       (setq i (1+ i))
       (let* ((bname (truncate-string-to-width
                      (buffer-name buffer) buffer-name-max nil nil buffer-name-ellipsis))
              (bname-face (if (string= (buffer-name buffer) (buffer-name))
                              (propertize bname 'face '(:weight bold))
                            (propertize bname 'face `(:foreground ,zw/exwm-ui-fg-alt))))
              (tab-click-func (lambda () (interactive)
                                (exwm-workspace-switch-to-buffer buffer)))
              (current-tab `(current-tab menu-item ,bname-face
                                         ,tab-click-func
                                         :help ,(buffer-name buffer)))
              (tab-seperator `(,(intern (format "sep-%i" i)) menu-item ,buffer-separator ignore)))
         (if (= i buffer-list-length)
             (list current-tab)
           (list current-tab tab-seperator))))
     buffer-list)))

(defun zw/tab-bar-format-cpu-temp ()
  "Produce menu that shows cpu temperature."
  `((global menu-item ,cpu-temperature-string
            nil :help ,(format "CPU temperature: %s" cpu-temperature-string))))

(defun zw/tab-bar-format-pyim ()
  "Produce menu that shows pyim."
  (let* ((input-method (or current-input-method-title ""))
         (chinese-input-method-p (string-match-p "PYIM/C" input-method))
         (chinese-input-method (if chinese-input-method-p "中 " "")))
    `((global menu-item ,chinese-input-method
              nil :help ,(format "Current input method: %s" current-input-method-title)))))

(setq tab-bar-show t
      tab-bar-format '(zw/tab-bar-format-exwm-workspace
                       tab-bar-separator
                       zw/tab-bar-format-buffers
                       tab-bar-format-align-right
                       tab-bar-separator
                       tab-bar-separator
                       tab-bar-separator
                       zw/tab-bar-format-pyim
                       zw/tab-bar-format-cpu-temp
                       zw/tab-bar-format-time
                       zw/tab-bar-format-battery))
(tab-bar-mode 1)

;; handle touchscreen tap
(bind-keys :map tab-bar-map
           ("<touchscreen-begin>" . zw/tab-bar-touchscreen-tab-select))

(defun zw/tab-bar-touchscreen-tab-select (event)
  "Select a tab at touchscreen tap."
  (interactive "e")
  (let* ((posn (cdadr event))
         (item (tab-bar--event-to-item posn)))
    (when (eq (catch 'context-menu
                (when (touch-screen-track-tap event)
                  (call-interactively (cadr item))))
              'context-menu)
      (tab-bar-mouse-context-menu event posn))))

;; time
(setq display-time-format "%b %-e %a %H:%M:%S %p"
      display-time-interval 1
      display-time-default-load-average nil)
(display-time-mode 1)

;; battery on laptop
(require 'battery)
(when battery-status-function
  (setq have-battery-status-p
        (let ((perc-charged (assoc ?p (funcall battery-status-function))))
          (and perc-charged
               (not (zerop (string-to-number (cdr perc-charged)))))))
  (when (and have-battery-status-p
             tab-bar-show)
    (display-battery-mode 1)))

;; keycast
(use-package keycast
  :config
  (setq keycast-tab-bar-format "%k%c%R "
        keycast-tab-bar-minimal-width 0)
  (add-to-list 'keycast-substitute-alist '(pdf-view-mouse-set-region nil nil))
  (add-to-list 'keycast-substitute-alist '(pdf-util-image-map-mouse-event-proxy nil nil))
  (add-to-list 'keycast-substitute-alist '(zw/tab-bar-touchscreen-tab-select nil nil))
  (keycast-tab-bar-mode))

;; ** minibuffer
(vertico-posframe-mode 0)
(setq exwm-workspace-minibuffer-position 'bottom)
;; detached minibuffer freezes on help message
;; https://github.com/ch11ng/exwm/wiki#minor-issues-related-to-the-autohide-echo-area
(tooltip-mode 1)
(which-key-mode 0)
(setq echo-keystrokes 0)
(add-hook 'exwm-init-hook
          (lambda ()
            (set-frame-parameter exwm-workspace--minibuffer 'background-color (face-background 'mode-line))))
(with-eval-after-load "pyim"
  (add-hook 'pyim-activate-hook 'exwm-workspace-attach-minibuffer)
  (add-hook 'pyim-deactivate-hook 'exwm-workspace-detach-minibuffer))
;; if ever stuck in exwm minibuffer, use abort-recursive-edit (c-]) to exit
(defun zw/exwm-minibuffer-and-keyboard-quit ()
  (interactive)
  (if (active-minibuffer-window)
      (abort-recursive-edit)
    (keyboard-quit)))
(bind-keys :map global-map
           ("<escape>" . zw/exwm-minibuffer-and-keyboard-quit)
           ("s-<escape>" . exwm-workspace-toggle-minibuffer)
           :map exwm-mode-map
           ("s-<escape>" . exwm-workspace-toggle-minibuffer))

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
          (set-frame-parameter (selected-frame) 'alpha-background 98))
      (progn
        (setq-local cursor-type nil
                    mode-line-format nil)
        (set-frame-parameter (selected-frame) 'alpha-background 0)))))

(defun zw/exwm-desktop-window-config ()
  (pcase (buffer-name)
    (" *Minibuf-0*" nil)
    ("*scratch*" (let ((n-window (length (window-list))))
                   (if (and (= n-window 1)
                            (= (buffer-size) 0))
                       (zw/exwm-set-ui nil)
                     (zw/exwm-set-ui t))))
    (_ (zw/exwm-set-ui t))))

(defun zw/exwm-scratch-post-command ()
  (when this-command
    (zw/exwm-desktop-window-config)))

(add-hook 'window-configuration-change-hook 'zw/exwm-desktop-window-config)
(with-current-buffer "*scratch*"
  (add-hook 'post-command-hook 'zw/exwm-scratch-post-command nil t))

;; ** wallpaper
(defun zw/exwm-set-wallpaper ()
  (unless (file-exists-p "~/.cache/emacs/wallpaper.png")
    (copy-file "~/.emacs.d/exwm/wallpaper.png" "~/.cache/emacs/wallpaper.png"))
  (with-current-buffer "*scratch*"
    (display-line-numbers-mode 0))
  (call-process-shell-command "feh --bg-scale ~/.cache/emacs/wallpaper.png") nil 0)

(zw/exwm-set-wallpaper)

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
              ("." . pyim-next-page)
              ("<left>" . pyim-backward-point)
              ("<right>" . pyim-forward-point))
  :config
  (setq pyim-page-tooltip 'minibuffer
        pyim-default-scheme 'quanpin
        pyim-page-style 'minibuffer
        pyim-page-length 9
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

(defun zw/exwm-switch-to-buffer-list ()
  (seq-filter
   (lambda (x)
     (and (not (zw/hidden-buffer-p x))
          (or (buffer-file-name x)
              (with-current-buffer x
                (or (and exwm-class-name (not (zw/exwm-plot-buffer-p x)))
                    exwm--floating-frame
                    (string-match "^\\*EXWM.*$" (buffer-name x))
                    (eq major-mode 'dired-mode)
                    (eq major-mode 'org-agenda-mode))))))
   (buffer-list)))

(defun zw/exwm-switch-to-buffer ()
  (interactive)
  (let* ((buffers (seq-filter
                   (lambda (x) (not (eq (current-buffer) x)))
                   (zw/exwm-switch-to-buffer-list)))
         (buffer-names (seq-map 'buffer-name buffers))
         (completion-extra-properties '(:annotation-function zw/exwm-switch-to-buffer-annotation))
         (buffer (completing-read "EXWM switch to buffer: " buffer-names nil t)))
    (exwm-workspace-switch-to-buffer buffer)))

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
  (exwm-input-set-key (kbd "s-$") 'desktop-environment-screenshot-part)
  (advice-add 'desktop-environment-volume-set :after
              (lambda (&rest args)
                (when (executable-find "dunst")
                  (call-process-shell-command
                   (concat "dunstify -r 1 \"  " (desktop-environment-volume-get) "\"") nil 0))))
  (advice-add 'desktop-environment-brightness-set :after
              (lambda (&rest args)
                (when (executable-find "dunst")
                  (call-process-shell-command
                   (concat "dunstify -r 1 \"󰖨  " (desktop-environment-brightness-get) "\"") nil 0)))))

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

;; ** exwm edit
(use-package exwm-edit
  :init
  (setq exwm-edit-bind-default-keys nil)
  :config
  (define-key exwm-edit-mode-map (kbd "C-c C-c") nil)
  (define-key exwm-edit-mode-map (kbd "s-q") (lambda () (interactive) (message "Close with C-c C-k")))
  (define-key exwm-edit-mode-map [remap save-buffer] nil))

;; ** CPU temperature
(use-package emacs-cpu-temperature
  :demand t
  :straight (:host github :repo "zhenhua-wang/emacs-cpu-temperature")
  :hook (exwm-init . cpu-temperature-mode)
  :config
  (setq cpu-temperature-update-interval 1))

;; ** agenda
(when (file-directory-p "~/Documents/Agenda")
  (org-agenda nil "d"))

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
;; *** default
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

;; *** chat
(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (and exwm-class-name
                       (string= exwm-class-name "weixin"))
              (exwm-input-set-local-simulation-keys
               (append (remove `(,(kbd "s-w") . ,(kbd "C-w")) exwm-input-simulation-keys)
                       `((,(kbd "C-w") . ,(kbd "s-m"))
                         (,(kbd "s-w") . ,(kbd "s-m"))))))))

;; *** no simulate keys
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
        ;; window
        (,(kbd "s-D") . zw/exwm-show-desktop)
        (,(kbd "s-m") . (lambda ()
                          (interactive)
                          (if exwm--floating-frame (exwm-floating-hide) (bury-buffer))))
        (,(kbd "s-}") . enlarge-window-horizontally)
        (,(kbd "s-{") . shrink-window-horizontally)
        (,(kbd "s-^") . enlarge-window)
        (,(kbd "s-u") . winner-undo)
        (,(kbd "s-U") . winner-redo)
        (,(kbd "s-`") . zw/side-window-toggle)
        ;; update emacs
        (,(kbd "<f5>") . zw/update-emacs-tangle-dotfiles)
        ;; web search
        (,(kbd "s-/") . emacs-websearch)
        ;; Launch applications
        (,(kbd "s-<return>") . (lambda (command)
                                 (interactive (list (read-shell-command "$ ")))
                                 (async-shell-command command)))
        (,(kbd "s-SPC") . app-launcher-run-app)
        (,(kbd "s-<tab>") . zw/exwm-switch-to-buffer)
        ;; git
        (,(kbd "s-M") . magit-status)
        ;; input
        (,(kbd "C-\\") . toggle-input-method)
        ;; side bar
        (,(kbd "s-b") . zw/toggle-dired-sidebar)
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
           ;;close current buffer
           ("s-q" . (lambda ()
                      (interactive)
                      (when (yes-or-no-p (format "Confirm kill %s? " exwm-class-name))
                        (kill-this-buffer))))
           ;; window
           ("s-<left>" . windmove-left)
           ("s-<right>" . windmove-right)
           ("s-<up>" . windmove-up)
           ("s-<down>" . windmove-down)
           ;; tab bar
           ("s-1" . zw/tab-switch)
           ("s-9" . tab-new)
           ("s-0" . tab-close)
           :map vertico-map
           ("s-<tab>" . vertico-next))

;; * exwm enable
(exwm-enable)
