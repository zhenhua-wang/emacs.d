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

;; * Module
;; ** begin
(defun zw/tab-bar-begin ()
  (propertize " " 'face '(:height 1.2) 'display '(raise -0.1)))

;; ** tab name
(defun zw/tab-bar-tab-name ()
  (let* ((buffer-file-p (buffer-file-name (window-buffer (minibuffer-selected-window))))
         (tab-name-max (if buffer-file-p 30 50))
         (dir-name-max 30)
         (tab-name (propertize (buffer-name (window-buffer (minibuffer-selected-window)))
                               'face 'zw/tab-bar-tab-selected))
         (tab-name-abbrev (truncate-string-to-width
                           tab-name tab-name-max nil nil
                           zw/tab-bar-ellipsis))
         (dir-name (if buffer-file-p
                       (propertize (abbreviate-file-name default-directory)
                                   'face 'zw/tab-bar-tab-path-selected)
                     ""))
         (dir-name-length (length dir-name))
         (dir-name-abbrev (if (< dir-name-length dir-name-max)
                              dir-name
                            (concat zw/tab-bar-ellipsis
                                    "/"
                                    (string-join (cdr (split-string (truncate-string-to-width
                                                                     dir-name
                                                                     dir-name-length
                                                                     (- dir-name-length dir-name-max))
                                                                    "\\/"))
                                                 "/"))))
         (dir-name-abbrev-prop (propertize dir-name-abbrev
                                           'face 'zw/tab-bar-tab-path-selected)))
    (concat dir-name-abbrev-prop tab-name-abbrev)))

;; ** function definition
(defun zw/tab-bar-beginning-of-defun ()
  "Return the line moved to by `beginning-of-defun'."
  (save-excursion
    (when (beginning-of-defun)
      (font-lock-ensure (point) (point-at-eol))
      (buffer-substring (point) (point-at-eol)))))

(defun zw/in-defun-p ()
  "check if current cursor is in a function definition"
  (save-excursion
    ;; if `end-of-defun' moves the cursor, cursor if in a definition
    (let ((current-point (point)))
      (end-of-defun)
      ;; return true if cursor moved
      (not (= (point) current-point)))))

(defun zw/tab-bar--func-def ()
  "helper function to get function definition"
  ;; if all three predicates are true, return the value of the last predicate
  (let ((tab-func-def-max 50))
    (and (derived-mode-p 'prog-mode)
         (ignore-errors (zw/in-defun-p))
         (ignore-errors (concat
                         (propertize " Def "
                                     'face 'mode-line-highlight)
                         " "
                         (truncate-string-to-width
                          (string-trim (zw/tab-bar-beginning-of-defun)) tab-func-def-max nil nil
                          zw/tab-bar-ellipsis))))))

(defun zw/tab-bar-format-function-def ()
  `((global menu-item ,(zw/tab-bar--func-def)
            :help "Function definition")))

;; ** file path
(defun zw/tab-bar-format-file-path ()
  `((current-tab menu-item ,(zw/tab-bar-tab-name)
                 :help "File path")))

;; ** env
(defun zw/tab-bar-format-env ()
  (let ((env (zw/modeline-env)))
    (when env
      (propertize env 'face 'zw/modeline-env-active))))

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
  (let ((repl-buffer (cl-remove-if-not (lambda (buffer)
                                         (get-buffer-window buffer))
                                       zw/side-window--buffer-opened)))
    (if repl-buffer
        (with-selected-window (get-buffer-window (car repl-buffer))
          (quit-window))
      (zw/side-window-toggle))))

(defun zw/tab-bar-format-repl ()
  `((repl-button menu-item
                 ,(propertize
                   (nerd-icons-codicon
                    "nf-cod-code"
                    :height 0.9
                    :v-adjust 0.15)
                   'mouse-face 'highlight)
                 zw/tab-bar--open-repl :help "Open REPL side window")))

;; ** debug
(defun zw/tab-bar-debug ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_alt") " ")
              'face 'success))

(defun zw/tab-bar-next ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_line_by_line") " ")
              'face 'warning))

(defun zw/tab-bar-continue ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_continue_small") " ")
              'face 'warning))

(defun zw/tab-bar-quit ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_disconnect") " ")
              'face 'error))

(defun zw/tab-bar-rerun ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_rerun") " ")
              'face 'error))

(defun zw/tab-bar-format-debug ()
  `((debug menu-item ,(zw/tab-bar-debug)
           zw/dape :help "Debug")
    (next menu-item ,(zw/tab-bar-next)
          dape-next :help "Next")
    (continue menu-item ,(zw/tab-bar-continue)
              dape-continue :help "Continue")
    (quit menu-item ,(zw/tab-bar-quit)
          dape-quit :help "Quit")
    (rerun menu-item ,(zw/tab-bar-rerun)
           dape-restart :help "Rerun")
    (debug-end menu-item "     "
               :help "")))

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
                           ((< percentage 25) 'zw/tab-bar-tab-battery-load-low)
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

;; ** pyim
(defun zw/tab-bar-format-pyim ()
  "Produce menu that shows pyim."
  (let* ((input-method (or current-input-method-title ""))
         (chinese-input-method-p (string-match-p "PYIM/C" input-method))
         (chinese-input-method (if chinese-input-method-p "中 " "")))
    `((global menu-item ,chinese-input-method
              nil :help ,(format "Current input method: %s" current-input-method-title)))))

;; ** exwm
;; *** workspace
(defun zw/tab-bar-format-exwm-workspace ()
  "Produce menu that shows current exwm workspace."
  (let* ((bg (face-background 'tab-bar))
         (bg-alt (pcase (frame-parameter nil 'background-mode)
                   ('light (doom-darken bg 0.1))
                   ('dark (doom-lighten bg 0.1)))))
    `((global menu-item ,(propertize (format " %d " exwm-workspace-current-index)
                                     'face `(:background ,bg-alt :weight regular))
              nil :help ,(format "Current EXWM workspace: %d" exwm-workspace-current-index)))))

;; *** buffer
(defun zw/tab-bar-switch-or-focus-buffer (buffer)
  "Switch to buffer if not visible, otherwise focus buffer."
  (let* ((buffer-window (zw/exwm-buffer-visible-p buffer))
         (buffer-float (with-current-buffer buffer exwm--floating-frame)))
    (cond ((eq (current-buffer) buffer) nil)
          ((and buffer-window buffer-float)
           (when exwm--floating-frame
             (select-frame-set-input-focus exwm-workspace--current))
           (select-frame-set-input-focus buffer-float))
          (buffer-window
           (when exwm--floating-frame
             (select-frame-set-input-focus exwm-workspace--current))
           (select-window buffer-window))
          (t
           (when exwm--floating-frame
             (select-frame-set-input-focus exwm-workspace--current))
           (exwm-workspace-switch-to-buffer buffer)))))

(defun zw/tab-bar-switch-to-buffer (i)
  "Tab bar switch to buffer."
  (let* ((buffer-list (zw/exwm-buffer-sorted-display-list))
         (buffer-list-size (length buffer-list)))
    (if (>= buffer-list-size i)
        (let* ((buffer (nth (- i 1) buffer-list)))
          (zw/tab-bar-switch-or-focus-buffer buffer))
      (zw/exwm-dunst-send-message "-r 99 -i gnome-windows" "Window" (format "\"Tab-%d does not exist\"" i)))))

(defun zw/tab-bar-format-buffers ()
  "Show buffers of current frame on tab-bar."
  (let* ((i 0)
         (buffer-name-ellipsis ".")
         (buffer-separator (propertize " | " 'face 'font-lock-comment-face))
         (screen-width (frame-width))
         (buffer-list (zw/exwm-buffer-sorted-display-list))
         (buffer-list-length (length buffer-list))
         (buffer-name-max (when (> buffer-list-length 0)
                            (- (/ screen-width buffer-list-length 2)
                               (length buffer-separator)
                               5))))
    (mapcan
     (lambda (buffer)
       (let* ((current-frame (if (frame-live-p zw/active-frame) zw/active-frame exwm-workspace--current))
              (current-buffer (window-buffer (frame-selected-window current-frame)))
              (bname (truncate-string-to-width
                      (buffer-name buffer) buffer-name-max nil nil buffer-name-ellipsis))
              (bname-face (if (eq buffer current-buffer)
                              (propertize bname 'face '(:weight bold))
                            (propertize bname 'face 'font-lock-comment-face)))
              (current-tab `(tab menu-item ,bname-face
                                 (lambda () (interactive)
                                   (zw/tab-bar-switch-or-focus-buffer ,buffer))
                                 :help ,(or (buffer-file-name buffer) (buffer-name buffer))))
              (tab-seperator `(seperator menu-item ,buffer-separator ignore)))
         (setq i (1+ i))
         (if (= i buffer-list-length)
             (list current-tab)
           (list current-tab tab-seperator))))
     buffer-list)))

;; * Keymap
;; switch to tab
(defun zw/tab-switch (index-name)
  (interactive
   (let* ((other-tabs (mapcar (lambda (tab)
                                (concat
                                 (number-to-string (1+ (tab-bar--tab-index tab)))
                                 " "
                                 (alist-get 'name tab)))
                              (tab-bar--tabs-recent))))
     (list (completing-read (format-prompt "Switch to tab by index"
                                           (car other-tabs))
                            other-tabs))))
  (tab-bar-select-tab (or (string-to-number (car (split-string index-name))) 1)))

;; register zw/tab-switch marginalia
(with-eval-after-load "marginalia"
  (add-to-list 'marginalia-prompt-categories '("\\<tab by index\\>" . tab-index))
  (add-to-list 'marginalia-annotator-registry
               '(tab-index marginalia-annotate-tab-index marginalia-annotate-face builtin none))
  (defun marginalia-annotate-tab-index (cand)
    "Annotate named tab CAND with window and buffer information."
    (when-let* ((tabs (funcall tab-bar-tabs-function))
                (index (1- (string-to-number (car (split-string cand))))))
      (let* ((tab (nth index tabs))
             (ws (alist-get 'ws tab))
             (bufs (window-state-buffers ws)))
        ;; NOTE: When the buffer key is present in the window state
        ;; it is added in front of the window buffer list and gets duplicated.
        (when (cadr (assq 'buffer ws)) (pop bufs))
        (concat
         (marginalia--fields
          ((format "win:%s" (length bufs))
           :face 'marginalia-size)
          ((format "group:%s" (or (alist-get 'group tab) 'none))
           :face 'marginalia-type
           :width 30)
          ((if (memq 'current-tab tab)
               "*current tab*"
             (string-join bufs " "))
           :face 'marginalia-documentation)))))))

;; define zw-tab-bar-keymap-mode
(define-minor-mode zw-tab-bar-keymap-mode
  "zw tab bar keymap mode."
  :global t
  (if zw-tab-bar-keymap-mode
      (bind-keys :map global-map
                 ;; tab operations
                 ("s-1" . zw/tab-switch)
                 ("s-9" . tab-new)
                 ("s-0" . tab-close))
    (bind-keys :map global-map
               ;; tab operations
               ("s-1" . nil)
               ("s-9" . nil)
               ("s-0" . nil))))

;; set tab-bar-map
(defun zw/tab-bar-touchscreen-tab-select (event)
  "Select a tab at touchscreen tap."
  (interactive "e")
  (let* ((posn (cdadr event))
         (item (tab-bar--event-to-item posn))
         (func (nth 1 item)))
    (when (and (touch-screen-track-tap event)
               (functionp func))
      (call-interactively func))))

(defun zw/tab-bar-click-tab-select (event)
  "Select a tab at click."
  (interactive "e")
  (let* ((item (tab-bar--event-to-item (event-start event)))
         (func (nth 1 item)))
    (when (functionp func)
      (call-interactively func))))

(bind-keys :map tab-bar-map
           ("<touchscreen-begin>" . zw/tab-bar-touchscreen-tab-select)
           ("<down-mouse-1>" . nil)
           ("<mouse-1>" . zw/tab-bar-click-tab-select)
           ("<down-mouse-3>" . nil))

;; * Config
;; ** main
(setq tab-bar-new-tab-choice "*scratch*"
      tab-bar-new-button-show nil
      tab-bar-close-button-show nil
      tab-bar-separator " "
      tab-bar-format '(zw/tab-bar-begin
                       ;; tab-bar-format-menu-bar
                       zw/tab-bar-format-dired
                       tab-bar-separator
                       zw/tab-bar-format-vterm
                       tab-bar-separator
                       zw/tab-bar-format-repl
                       ;; zw/tab-bar-format-file-path
                       tab-bar-format-align-right
                       tab-bar-separator
                       zw/tab-bar-format-env
                       tab-bar-separator
                       zw/tab-bar-format-debug))
(tab-bar-mode)

;; ** time
(setq display-time-format "%b %-e %a %H:%M:%S %p"
      display-time-interval 1
      display-time-default-load-average nil)
(display-time-mode 1)

;; ** battery
(require 'battery)
(when battery-status-function
  (setq have-battery-status-p
        (let ((perc-charged (assoc ?p (funcall battery-status-function))))
          (and perc-charged
               (not (zerop (string-to-number (cdr perc-charged)))))))
  (when (and have-battery-status-p
             tab-bar-show)
    (display-battery-mode 1)))

;; * Provide
(provide 'zw-tab-bar)
