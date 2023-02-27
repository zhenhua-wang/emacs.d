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

;; show menu
(defun zw/tab-bar-format-menu-bar ()
  "Produce the Menu button for the tab bar that shows the menu bar."
  `((menu-bar menu-item (propertize "☰"
                                    'face 'zw/tab-bar-menu-bar
                                    'pointer 'hand)
              tab-bar-menu-bar :help "Menu Bar")))

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

(defun zw/tab-bar-beginning-of-defun ()
  "Return the line moved to by `beginning-of-defun'."
  (save-excursion
    (when (beginning-of-defun)
      (font-lock-ensure (point) (point-at-eol))
      (buffer-substring (point) (point-at-eol)))))

(defun zw/in-defun-p ()
  "check if current cursor is in a function definition"
  (save-excursion
    ;; HACK: if `end-of-defun' moves the cursor, cursor if in a definition
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

(defun zw/tab-bar-format-file-path ()
  `((current-tab menu-item  (zw/tab-bar-tab-name)
                 :help "File path")))

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
                     (cond ((and charging? (> percentage 95)) "")
                           ((> percentage 95) "")
                           ((> percentage 90) "")
                           ((> percentage 80) "")
                           ((> percentage 70) "")
                           ((> percentage 60) "")
                           ((> percentage 50) "")
                           ((> percentage 40) "")
                           ((> percentage 30) "")
                           ((> percentage 20) "")
                           ((> percentage battery-load-critical) "")
                           (t ""))
                   ""))
           (help-echo (if (and battery-echo-area-format data valid-percentage?)
                          (battery-format battery-echo-area-format data)
                        "Battery status not available")))
      (setq battery-mode-line-string
            (propertize (concat icon " ") 'face face 'help-echo help-echo)))))
(advice-add #'battery-update :override #'zw/tab-bar-update-battery-status)

(defun zw/tab-bar-format-battery ()
  "Produce battery information for the tab bar."
  `((global menu-item ,battery-mode-line-string nil
            :help (plist-get (text-properties-at 0 battery-mode-line-string)
                             'help-echo))))

(defun zw/tab-bar-format-time ()
  "Produce time information for the tab bar."
  `((global menu-item ,display-time-string nil
            :help (plist-get (text-properties-at 0 display-time-string)
                             'help-echo))))

;; format tab-bar-mode
(setq tab-bar-new-tab-choice "*scratch*"
      tab-bar-new-button-show nil
      tab-bar-close-button-show nil
      tab-bar-separator " "
      tab-bar-format '(tab-bar-separator
                       zw/tab-bar-format-menu-bar
                       tab-bar-separator
                       zw/tab-bar-format-file-path
                       tab-bar-format-align-right))

;; enable tab-bar
(add-hook 'after-init-hook #'tab-bar-mode)

;;; misc config
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

(provide 'zw-tab-bar)
