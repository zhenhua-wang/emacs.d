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
  `((menu-bar menu-item (propertize " ☰"
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

(defun zw-tab-bar-format-tab-file-path ()
  `((current-tab menu-item (if (and (derived-mode-p 'prog-mode) (zw-tab-bar-beginning-of-defun))
                               (concat
                                (propertize " Def "
                                            'face 'keycast-key)
                                " "
                                (string-trim (zw-tab-bar-beginning-of-defun)))
                             (zw-tab-bar-tab-name))
                 :help "File path or function definition")))

(defun zw-tab-bar-format-tab-func-def ()
  `((current-tab menu-item (concat
                            (propertize " Def "
                                        'face 'keycast-key)
                            (string-trim (zw-tab-bar-beginning-of-defun)))
                 :help "Function definition")))

;; set default foreground
(set-face-foreground 'tab-bar (face-foreground 'default))

;; format tab-bar-mode
(setq tab-bar-new-tab-choice "*scratch*"
      tab-bar-new-button-show nil
      tab-bar-close-button-show nil
      tab-bar-separator " "
      tab-bar-format '(zw-tab-bar-format-menu-bar
                       tab-bar-separator
                       zw-tab-bar-format-tab-file-path
                       tab-bar-separator
                       tab-bar-format-align-right
                       zw-tab-bar-format-battery))

;; enable tab-bar
(tab-bar-mode 1)

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
