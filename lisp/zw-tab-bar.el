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

(defface zw-tab-bar-path-selected
  '((t (:inherit zw-tab-bar-default-selected :bold t)))
  "Default face for active tab-bar"
  :group 'zw-tab-bar-selected)

(defun zw-tab-bar-format-battery ()
  `((global menu-item ,(propertize battery-mode-line-string
                                   'face 'zw-tab-bar-battery) ignore)))

;; show menu
(defun zw-tab-bar-format-menu-bar ()
  "Produce the Menu button for the tab bar that shows the menu bar."
  `((menu-bar menu-item (propertize " ☰"
                                    'face 'zw-tab-bar-menu-bar
                                    'pointer 'hand)
              tab-bar-menu-bar :help "Menu Bar")))

(defun zw-tab-bar-tab-name ()
  (let ((tab-name (propertize (buffer-name (window-buffer (minibuffer-selected-window)))
                              'face 'zw-tab-bar-default-selected))
        (dir-name (if buffer-file-name
                      (propertize (abbreviate-file-name default-directory)
                                  'face 'zw-tab-bar-path-selected)
                    "")))
    (concat dir-name tab-name)))

;; format tab-bar-mode
(setq tab-bar-tab-name-function 'zw-tab-bar-tab-name
      tab-bar-new-tab-choice "*scratch*"
      tab-bar-new-button-show nil
      tab-bar-close-button-show nil
      tab-bar-separator " "
      tab-bar-format '(zw-tab-bar-format-menu-bar
                       tab-bar-format-tabs
                       tab-bar-separator
                       tab-bar-format-align-right
                       zw-tab-bar-format-battery))

;; enable tab-bar
(tab-bar-mode 1)

(provide 'zw-tab-bar)
