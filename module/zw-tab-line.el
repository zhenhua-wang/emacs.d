;; -*- lexical-binding: t -*-

;; * Appearence
(defun zw/tab-line-init-appearence ()
  (set-face-attribute 'tab-line-tab-current nil
                      :underline (face-background 'highlight))
  (dolist (centaur-face '(tab-line-tab
                          tab-line-tab-current
			  tab-line-tab-inactive))
    (set-face-attribute centaur-face nil
                        :family (face-attribute 'default :font)
                        :height (face-attribute 'tab-bar :height))))

;; fix issue when switching theme
(advice-add 'consult-theme :after (lambda (arg)
                                    (zw/tab-line-init-appearence)))

;; * Module
;; ** tab name
(defun zw/tab-line-tab-name (buffer &optional _buffers)
  (with-current-buffer buffer
    (format " %s %s "
            (nerd-icons-icon-for-mode major-mode)
            (buffer-name buffer))))

;; ** select tab
(defun zw/tab-line-select (index)
  (interactive)
  (let* ((visible-tabs (tab-line-tabs-window-buffers))
         (n-visible-tabs (length visible-tabs))
         (selected-buffer (nth (- index 1) visible-tabs)))
    (unless (eq (current-buffer) selected-buffer)
      (if (> index n-visible-tabs)
          (message "Tab %s does not exist" index)
        (tab-line-select-tab-buffer selected-buffer)))))

;; * keymap
(dolist (key-func (mapcar (lambda (i)
                            `(,(kbd (format "s-%d" i)) .
                              (lambda ()
                                (interactive)
                                (zw/tab-line-select ,i))))
                          (number-sequence 0 9)))
  (define-key global-map (car key-func) (cdr key-func)))

;; * Config
(setq tab-line-tab-name-function #'zw/tab-line-tab-name
      tab-line-new-button-show nil
      tab-line-close-button-show t
      tab-line-close-button "× "
      tab-line-separator ""
      x-underline-at-descent-line t)
(add-hook 'tab-line-mode-hook 'zw/tab-line-init-appearence)

;; * Group
(defun zw/tab-line-group-docs ()
  (memq major-mode '(helpful-mode
                     help-mode
                     ess-r-help-mode)))

;; * enable
(add-hook 'after-init-hook 'global-tab-line-mode)
(defun zw/tab-line-hide ()
  (when (and (featurep 'tab-line)
	     tab-line-mode
             (not buffer-file-name)
             (not (zw/tab-line-group-docs)))
    (tab-line-mode -1)))
(add-hook 'after-change-major-mode-hook 'zw/tab-line-hide)
(add-hook 'buffer-list-update-hook 'zw/tab-line-hide)

;; * Provide
(provide 'zw-tab-line)
