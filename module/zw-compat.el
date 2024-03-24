;; -*- lexical-binding: t -*-

(unless (fboundp 'string-pixel-width)
  (defun string-pixel-width (string)
    (if (zerop (length string))
        0
      ;; Keeping a work buffer around is more efficient than creating a
      ;; new temporary buffer.
      (with-current-buffer (get-buffer-create " *string-pixel-width*")
        ;; If `display-line-numbers' is enabled in internal buffers
        ;; (e.g. globally), it breaks width calculation (bug#59311)
        (setq-local display-line-numbers nil)
        (delete-region (point-min) (point-max))
        ;; Disable line-prefix and wrap-prefix, for the same reason.
        (setq line-prefix nil
	      wrap-prefix nil)
        (insert (propertize string 'line-prefix nil 'wrap-prefix nil))
        (car (buffer-text-pixel-size nil nil t))))))

(unless (fboundp 'mode-line-window-selected-p)
  (defun mode-line-window-selected-p ()
    (let ((window (selected-window)))
      (or (eq window (old-selected-window))
	  (and (minibuffer-window-active-p (minibuffer-window))
	       (with-selected-window (minibuffer-window)
	         (eq window (minibuffer-selected-window))))))))

(provide 'zw-compat)
