(defun zw/toggle-dired-sidebar ()
  "Toggle dired on left side."
  (interactive)
  (let* ((dir (abbreviate-file-name
               (or (vc-root-dir)
                   (ignore-errors (file-name-directory (buffer-file-name)))
                   default-directory)))
         (buffer (dired-noselect dir)))
    (with-current-buffer buffer (zw-dired-sidebar-mode 1))
    (display-buffer-in-side-window
     buffer `((side . left) (slot . 0)
              (window-width . 0.2)
              (preserve-size . (t . nil))
              (window-parameters . ((no-delete-other-windows . t)
                                    (dedicated . t)))))
    (select-window (get-buffer-window buffer))))

(defun zw/dired-find-file ()
  (interactive)
  (dired-find-file)
  (zw-dired-sidebar-mode 1))

(defun zw/dired-up-directory ()
  (interactive)
  (dired-up-directory)
  (zw-dired-sidebar-mode 1))

(define-minor-mode zw-dired-sidebar-mode
  "Toggle zw-dired-sidebar mode."
  :lighter " Dired-Sidebar"
  :keymap
  `((,(kbd "s-q") . zw/kill-bufer-quit-window)
    (,(kbd "q") . zw/kill-bufer-quit-window)
    (,(kbd "s-b") . zw/kill-bufer-quit-window)
    (,(kbd "^") . zw/dired-up-directory)
    (,(kbd "RET") . zw/dired-find-file)
    (,(kbd "<mouse-2>") . zw/dired-find-file))
  (let* ((dir (abbreviate-file-name (dired-current-directory)))
         (buffer (dired-noselect dir))
         (name (concat " :" dir)))
    (if zw-dired-sidebar-mode
        (with-current-buffer buffer
          (dired-hide-details-mode t)
          (rename-buffer name)
          (setq-local mode-line-format
                      (list "%e" " " dir)))
      (with-current-buffer buffer
        (rename-buffer dir)))))

(provide 'zw-dired-sidebar)
