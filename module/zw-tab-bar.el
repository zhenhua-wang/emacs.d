;; -*- lexical-binding: t -*-

;; * Face
(defgroup zw/tab-bar nil
  "zw/tab-bar"
  :group 'convenience)

(defgroup zw/tab-bar-selected nil
  "zw/tab-bar-selected"
  :group 'zw/tab-bar)

(defface zw/tab-bar-default-selected
  `((t (:inherit tab-bar)))
  "Default face for active tab-bar"
  :group 'zw/tab-bar-selected)

(defface zw/tab-bar-menu-bar
  `((t (:inherit zw/tab-bar-default-selected)))
  "Default face for active tab-bar"
  :group 'zw/tab-bar)

(defface zw/tab-bar-tab-selected
  `((t (:inherit zw/tab-bar-default-selected)))
  "Default face for active tab-bar"
  :group 'zw/tab-bar-selected)

(defface zw/tab-bar-tab-path-selected
  `((t (:inherit zw/tab-bar-default-selected :bold t)))
  "Default face for active tab-bar"
  :group 'zw/tab-bar-selected)

(defface zw/tab-bar-tab-battery-load-default
  `((t (:inherit zw/tab-bar-default-selected :bold t)))
  "Default face for battery load on active tab-bar"
  :group 'zw/tab-bar-selected)

(defface zw/tab-bar-tab-battery-load-charging
  `((t (:inherit zw/tab-bar-default-selected :bold t)))
  "Face for charging battery load on active tab-bar"
  :group 'zw/tab-bar-selected)

(defface zw/tab-bar-tab-battery-load-low
  `((t (:inherit zw/tab-bar-default-selected :bold t)))
  "Face for low battery load on active tab-bar"
  :group 'zw/tab-bar-selected)

(defface zw/tab-bar-tab-battery-load-critical
  `((t (:inherit zw/tab-bar-default-selected :bold t)))
  "Face for critical battery load on active tab-bar"
  :group 'zw/tab-bar-selected)

(defvar zw/tab-bar-ellipsis "..."
  "Replacing string for long path name or file name")

;; * Appearance
(defun zw/tab-bar-tab-name-format (tab i)
  (let ((current-p (eq (car tab) 'current-tab)))
    (propertize
     (concat (if tab-bar-tab-hints (format "%d " i) " ")
             (alist-get 'name tab)
             (or (and tab-bar-close-button-show
                      (not (eq tab-bar-close-button-show
                               (if current-p 'non-selected 'selected)))
                      tab-bar-close-button)
                 " "))
     'face (funcall tab-bar-tab-face-function tab))))

;; * Module
;; ** begin
(defun zw/tab-bar-begin ()
  (propertize " " 'face '(:height 1.2) 'display '(raise -0.1)))

;; ** env
(defun zw/tab-bar--env-menu (event)
  (interactive "e")
  (zw/define-menu
   "Conda environment"
   (append (mapcar (lambda (x)
                     (vector x `(lambda () (interactive) (conda-env-activate ,x) t)))
                   (conda-env-candidates))
           (list "---" "---")
           (list (vector "conda deactivate"
                         `(lambda () (interactive) (conda-env-deactivate) t))))))

(defun zw/tab-bar-format-env ()
  (let ((env (ignore-errors
               (substring-no-properties (string-trim (zw/modeline-env)))))
        (icon (nerd-icons-faicon "nf-fa-desktop"
                                 :height 0.85
                                 :v-adjust 0.15))
        (separator (if (display-graphic-p) ":" " :")))
    `((env menu-item ,(if env (concat icon separator env) icon)
           zw/tab-bar--env-menu :help "Click to activate environment"))))

;; ** dired
(defun zw/tab-bar--open-dired (event)
  (interactive "e")
  (let ((sidebar-window (cl-remove-if-not
                         (lambda (window)
                           (with-selected-window window
                             (and (window-buffer window)
                                  zw-dired-sidebar-mode)))
                         (window-list))))
    (if sidebar-window
        (with-selected-window (car sidebar-window)
          (kill-buffer (current-buffer)))
      (let ((buffer (dired-noselect default-directory)))
        (zw/dired-sidebar-enable buffer)))))

(defun zw/tab-bar-format-dired ()
  `((dired-button menu-item
                  ,(propertize
                    (nerd-icons-codicon
                     "nf-cod-folder"
                     :height 0.85
                     :v-adjust 0.15)
                    'mouse-face 'highlight)
                  zw/tab-bar--open-dired :help "Open dired in current directory")))

;; ** vterm
(defun zw/tab-bar--open-vterm (event)
  (interactive "e")
  (let ((sidebar-window (cl-remove-if-not
                         (lambda (window)
                           (with-selected-window window
                             (and (window-buffer window)
                                  (featurep 'vterm)
                                  (eq major-mode 'vterm-mode))))
                         (window-list))))
    (if sidebar-window
        (with-selected-window (car sidebar-window)
          (quit-window))
      (vterm))))

(defun zw/tab-bar-format-vterm ()
  `((vterm-button menu-item
                  ,(propertize
                    (nerd-icons-codicon
                     "nf-cod-terminal_powershell"
                     :height 0.85
                     :v-adjust 0.15)
                    'mouse-face 'highlight)
                  zw/tab-bar--open-vterm :help "Open vterm")))

;; ** repl
(defun zw/tab-bar--open-repl (event)
  (interactive "e")
  (zw/right-side-window-toggle))

(defun zw/tab-bar-format-repl ()
  `((repl-button menu-item
                 ,(propertize
                   (nerd-icons-codicon
                    "nf-cod-code"
                    :height 0.9
                    :v-adjust 0.15)
                   'mouse-face 'highlight)
                 zw/tab-bar--open-repl :help "Open REPL side window")))

;; ** battery
(defun zw/tab-bar-update-battery-status ()
  "Update battery status."
  (when (bound-and-true-p display-battery-mode)
    (let* ((data (and battery-status-function
                      (functionp battery-status-function)
                      (funcall battery-status-function)))
           (status (cdr (assoc ?L data)))
           (charging? (or (string-equal "on-line" status)
                          (string-equal "AC" status)
                          (and (eq battery-status-function 'battery-upower)
                               (not (or (string-equal "discharging" (cdr (assoc ?B data)))
                                        (string-equal "pending-discharge" (cdr (assoc ?B data)))
                                        (string-equal "empty" (cdr (assoc ?B data))))))))
           (percentage (car (read-from-string (or (cdr (assq ?p data)) "ERR"))))
           (valid-percentage? (and (numberp percentage)
                                   (>= percentage 0)
                                   (<= percentage battery-mode-line-limit)))
           (face (if valid-percentage?
                     (cond (charging? 'zw/tab-bar-tab-battery-load-charging)
                           ((< percentage battery-load-critical) 'zw/tab-bar-tab-battery-load-critical)
                           ((< percentage 50) 'zw/tab-bar-tab-battery-load-low)
                           (t 'zw/tab-bar-tab-battery-load-default))
                   'error))
           (icon (if valid-percentage?
                     (cond ((> percentage 95) " ")
                           ((> percentage 75) " ")
                           ((> percentage 50) " ")
                           ((> percentage 25) " ")
                           ((> percentage battery-load-critical) " ")
                           (t " "))
                   " "))
           (text (if (>= percentage 100)
                     (format "%d%% " percentage)
                   (if charging?
                       (format "+%02d%% " percentage)
                     (format " %02d%% " percentage))))
           (help-echo (if (and battery-echo-area-format data valid-percentage?)
                          (battery-format battery-echo-area-format data)
                        "Battery status not available")))
      (setq battery-mode-line-string
            (propertize text 'face face 'help-echo help-echo)))))
(advice-add #'battery-update :override #'zw/tab-bar-update-battery-status)

(defun zw/tab-bar-format-battery ()
  "Produce battery information for the tab bar."
  `((global menu-item ,battery-mode-line-string nil
            :help (plist-get (text-properties-at 0 battery-mode-line-string)
                             'help-echo))))

;; ** time
(defun zw/tab-bar-format-time ()
  "Produce time information for the tab bar."
  `((global menu-item ,display-time-string nil
            :help (plist-get (text-properties-at 0 display-time-string)
                             'help-echo))))

;; ** cpu temperature
(defun zw/tab-bar-format-cpu-temp ()
  "Produce menu that shows cpu temperature."
  `((global menu-item ,cpu-temperature-string
            nil :help ,(format "CPU temperature: %s" cpu-temperature-string))))

;; ** exwm
(defun zw/tab-bar-format-exwm-workspace ()
  "Produce menu that shows current exwm workspace."
  (let* ((bg (face-background 'tab-bar))
         (bg-alt (pcase (frame-parameter nil 'background-mode)
                   ('light (doom-darken bg 0.1))
                   ('dark (doom-lighten bg 0.1)))))
    `((global menu-item ,(propertize (format " %d " exwm-workspace-current-index)
                                     'face `(:background ,bg-alt :weight regular))
              nil :help ,(format "Current EXWM workspace: %d" exwm-workspace-current-index)))))

;; * Config
(setq tab-bar-new-tab-choice "*scratch*"
      tab-bar-new-button-show nil
      tab-bar-close-button-show nil
      tab-bar-separator " "
      tab-bar-auto-width nil
      tab-bar-tab-name-format-function 'zw/tab-bar-tab-name-format
      tab-bar-format '(zw/tab-bar-begin
                       ;; tab-bar-format-menu-bar
                       zw/tab-bar-format-dired
                       tab-bar-separator
                       zw/tab-bar-format-env
                       tab-bar-separator
                       tab-bar-format-tabs
                       ;; zw/tab-bar-format-file-path
                       tab-bar-format-align-right))

;; * Enable
(defun zw/tab-bar-init ()
  (tab-rename "Main" 1)
  (tab-bar-mode 1)
  ;; time
  (setq display-time-format "%b %-e %a %H:%M:%S %p"
        display-time-interval 1
        display-time-default-load-average nil)
  (display-time-mode 1)
  ;; battery
  (require 'battery)
  (when battery-status-function
    (setq have-battery-status-p
          (let ((perc-charged (assoc ?p (funcall battery-status-function))))
            (and perc-charged
                 (not (zerop (string-to-number (cdr perc-charged)))))))
    (when (and have-battery-status-p
               tab-bar-show)
      (display-battery-mode 1))))
(add-hook 'after-init-hook 'zw/tab-bar-init)

;; * Keymap
(defun zw/tab-bar--item-select (item)
  (let ((func (nth 1 item)))
    (if (functionp func)
        (call-interactively func)
      (let ((tab-number (tab-bar--key-to-number (nth 0 item))))
        (unless (eq tab-number t)
          (tab-bar-select-tab tab-number))))))

(defun zw/tab-bar-touchscreen-tab-select (event)
  "Select a tab at touchscreen tap."
  (interactive "e")
  (let* ((posn (cdadr event))
         (item (tab-bar--event-to-item posn)))
    (when (touch-screen-track-tap event)
      (zw/tab-bar--item-select item))))

(defun zw/tab-bar-click-tab-select (event)
  "Select a tab at click."
  (interactive "e")
  (let ((item (tab-bar--event-to-item (event-start event))))
    (zw/tab-bar--item-select item)))

(bind-keys :map tab-bar-map
           ("<touchscreen-begin>" . zw/tab-bar-touchscreen-tab-select)
           ("<down-mouse-1>" . nil)
           ("<mouse-1>" . zw/tab-bar-click-tab-select)
           ("<down-mouse-3>" . nil))

(dolist (i (number-sequence 1 9))
  (define-key global-map
              (kbd (format "C-s-%s" i))
              (lambda ()
                (interactive)
                (let ((total-tabs (length (tab-bar-tabs))))
                  (if (> i total-tabs)
                      (tab-bar-select-tab total-tabs)
                    (tab-bar-select-tab i))))))

;; * Provide
(provide 'zw-tab-bar)
