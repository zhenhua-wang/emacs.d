(defun zw/toggle-dired-sidebar ()
  (interactive)
  (let* ((root (expand-file-name default-directory))
         (buffer (dired-noselect root))
         (name (concat " :" (buffer-name buffer)))
         (window (get-buffer-window buffer))
         (display-alist '((side . left)
                          (window-width . 0.2)
                          (slot . -1))))
    (if window
        (kill-buffer buffer)
      (progn
        (display-buffer-in-side-window buffer display-alist)
        (with-current-buffer buffer
          (visual-line-mode 0)
          (setq-local buffer-face-mode-face
                      (list ':background (doom-color 'base2)))
          (buffer-face-mode)
          (setq-local mode-line-format (list
                                        "%e"
                                        '(:eval (zw/modeline-buffer-name))))
          (rename-buffer name))))))
