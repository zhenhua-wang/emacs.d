;; -*- lexical-binding: t -*-

;; * Group
;;;; group hash table
(defvar zw/tab-line-group--hash-table (make-hash-table))

(defun zw/tab-line-group-add-buffer (buffer)
  (let* ((group (zw/tab-line-buffer-group buffer))
         (group-buffers (gethash group zw/tab-line-group--hash-table)))
    (add-to-list 'group-buffers buffer 'append)
    (puthash group
             ;; clear dead buffers
             (seq-filter 'buffer-live-p group-buffers)
             zw/tab-line-group--hash-table)))

(defun zw/tab-line-group-add-current-buffer ()
  (zw/tab-line-group-add-buffer (current-buffer)))

(defun zw/tab-line-group-remove-buffer (buffer)
  (let* ((group (zw/tab-line-buffer-group buffer))
         (group-buffers (gethash group zw/tab-line-group--hash-table)))
    (puthash group
             (remove buffer group-buffers)
             zw/tab-line-group--hash-table)))

(defun zw/tab-line-group-remove-current-buffer ()
  (zw/tab-line-group-remove-buffer (current-buffer)))

(add-hook 'buffer-list-update-hook 'zw/tab-line-group-add-current-buffer)
(add-hook 'kill-buffer-hook 'zw/tab-line-group-remove-current-buffer)

;;;; group buffers
(defun zw/tab-line-buffer-group (buffer)
  (with-current-buffer buffer
    (cond (buffer-file-name "File")
          ((zw/tab-line-group-docs) "Doc")
          (t "Other"))))

(defun zw/tab-line-buffer-group-buffers ()
  (let* ((buffers (funcall tab-line-tabs-buffer-list-function))
         (buffers (seq-remove (lambda (b) (with-current-buffer b
                                            (zw/tab-line-hide-buffers)))
                              buffers))
         (group (zw/tab-line-buffer-group (current-buffer))))
    (gethash group zw/tab-line-group--hash-table)))

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

;; ** group
(defun zw/tab-line-group-docs ()
  (memq major-mode '(helpful-mode
                     help-mode
                     ess-r-help-mode)))

;; ** visible tabs
(defun zw/tab-line-hide-buffers ()
  (and (not buffer-file-name)
       (not (zw/tab-line-group-docs))))

(defun zw/tab-line-tabs-window-buffers (orig-fun &rest args)
  "Advice 'tab-line-tabs-window-buffers' to filter boring buffers."
  (seq-remove (lambda (b) (with-current-buffer b
                            (zw/tab-line-hide-buffers)))
              (apply orig-fun args)))
(advice-add 'tab-line-tabs-window-buffers :around
            'zw/tab-line-tabs-window-buffers)

;; ** select tab
(defun zw/tab-line-select (index)
  (interactive)
  (let* ((visible-tabs (funcall tab-line-tabs-function))
         (n-visible-tabs (length visible-tabs))
         (selected-buffer (nth (- index 1) visible-tabs)))
    (unless (eq (current-buffer) selected-buffer)
      (if (> index n-visible-tabs)
          (message "Tab %s does not exist" index)
        (switch-to-buffer selected-buffer)))))

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
      tab-line-tabs-function #'zw/tab-line-buffer-group-buffers
      tab-line-new-button-show nil
      tab-line-close-button-show t
      tab-line-close-button "× "
      tab-line-separator ""
      x-underline-at-descent-line t)
(add-hook 'tab-line-mode-hook 'zw/tab-line-init-appearence)

;; * enable
(add-hook 'after-init-hook 'global-tab-line-mode)
(defun zw/tab-line-hide ()
  (when (and (featurep 'tab-line)
	     tab-line-mode
             (zw/tab-line-hide-buffers))
    (tab-line-mode -1)))
(add-hook 'after-change-major-mode-hook 'zw/tab-line-hide)
(add-hook 'buffer-list-update-hook 'zw/tab-line-hide)

;; * Provide
(provide 'zw-tab-line)
