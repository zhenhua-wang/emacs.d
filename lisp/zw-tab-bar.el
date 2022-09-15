(defgroup zw-tab-bar nil
  "zw-tab-bar"
  :group 'convenience)

(defgroup zw-tab-bar-selected nil
  "zw-tab-bar-active"
  :group 'zw-tab-bar)

(defgroup zw-tab-bar-nonselected nil
  "zw-tab-bar-inactive"
  :group 'zw-tab-bar)

(defface zw-tab-bar-menu-bar
  `((t (:foreground ,(face-foreground 'default) :background ,(face-background 'tab-bar))))
  "Default face for active tab-bar"
  :group 'zw-tab-bar)

(defface zw-tab-bar-battery
  `((t (:inherit zw-tab-bar-menu-bar)))
  "Default face for active tab-bar"
  :group 'zw-tab-bar)

(defface zw-tab-bar-default-selected
  `((t (:foreground ,(face-foreground 'default) :background ,(face-background 'tab-bar))))
  "Default face for active tab-bar"
  :group 'zw-tab-bar-selected)

(defface zw-tab-bar-default-nonselected
  `((t (:foreground
        ,(face-foreground 'font-lock-comment-face)
        :background
        ,(face-background 'tab-bar)
        :underline t)))
  "Default face for inactive tab-bar"
  :group 'zw-tab-bar-nonselected)

(defface zw-tab-bar-tab-selected
  `((t (:inherit zw-tab-bar-default-selected)))
  "Default face for active tab-bar"
  :group 'zw-tab-bar-selected)

(defface zw-tab-bar-tab-path-selected
  `((t (:inherit zw-tab-bar-default-selected :bold t :foreground ,(face-foreground 'font-lock-keyword-face))))
  "Default face for active tab-bar"
  :group 'zw-tab-bar-selected)

(defvar zw-tab-bar-path-max 30
  "Maximum length of current tab path")

(defvar zw-tab-bar-path-ellipsis "..."
  "Replacing string for long path name")

(defun zw-tab-bar-format-battery ()
  `((global menu-item ,(propertize battery-mode-line-string
                                   'face 'zw-tab-bar-battery) ignore)))

;; show menu
(defun zw-tab-bar-format-menu-bar ()
  "Produce the Menu button for the tab bar that shows the menu bar."
  `((menu-bar menu-item (propertize " ☰ "
                                    'face 'zw-tab-bar-menu-bar
                                    'pointer 'hand)
              tab-bar-menu-bar :help "Menu Bar")))

(defun zw-tab-bar-tab-name ()
  (let* ((tab-name (propertize (buffer-name (window-buffer (minibuffer-selected-window)))
                               'face 'zw-tab-bar-tab-selected))
         (dir-name (if buffer-file-name
                       (propertize (abbreviate-file-name default-directory)
                                   'face 'zw-tab-bar-tab-path-selected)
                     ""))
         (dir-name-length (length dir-name))
         (dir-name-abbrev (if (< dir-name-length zw-tab-bar-path-max)
                              dir-name
                            (concat ".../"
                                    (string-join (cdr (split-string (truncate-string-to-width
                                                                     dir-name
                                                                     dir-name-length
                                                                     (- dir-name-length zw-tab-bar-path-max))
                                                                    "\\/"))
                                                 "/"))))
         (dir-name-abbrev-prop (propertize dir-name-abbrev
                                           'face 'zw-tab-bar-tab-path-selected)))
    (concat dir-name-abbrev-prop tab-name)))

(defun zw-tab-bar-beginning-of-defun ()
  "Return the line moved to by `beginning-of-defun'."
  (save-excursion
    (when (beginning-of-defun)
      (font-lock-ensure (point) (point-at-eol))
      (buffer-substring (point) (point-at-eol)))))

(defun zw-tab-bar-format-current-tab ()
  `((current-tab menu-item (if (and (derived-mode-p 'prog-mode) (zw-tab-bar-beginning-of-defun))
                               (string-trim (zw-tab-bar-beginning-of-defun))
                             (zw-tab-bar-tab-name))
                 :help "Current tab")))

;; set default foreground
(set-face-foreground 'tab-bar (face-foreground 'default))

;; format tab-bar-mode
(setq tab-bar-new-tab-choice "*scratch*"
      tab-bar-new-button-show nil
      tab-bar-close-button-show nil
      tab-bar-separator " "
      tab-bar-format '(zw-tab-bar-format-menu-bar
                       zw-tab-bar-format-current-tab
                       tab-bar-separator
                       tab-bar-format-align-right
                       zw-tab-bar-format-battery))

;; enable tab-bar
(tab-bar-mode 1)

(provide 'zw-tab-bar)
