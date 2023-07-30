(defun zw/toggle-dired-sidebar ()
  "Toggle dired on left side."
  (interactive)
  ;; close all old sidebars
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when zw-dired-sidebar-mode
        (kill-buffer buffer))))
  ;; open current directory in sidebar
  (let* ((dir (abbreviate-file-name
               (or (vc-root-dir)
                   (ignore-errors (file-name-directory (buffer-file-name)))
                   default-directory)))
         (buffer (dired-noselect dir)))
    ;; bury current dired buffer when it has the same root as sidebar
    (when (eq (current-buffer) buffer)
      (bury-buffer))
    (with-current-buffer buffer (zw-dired-sidebar-mode 1))
    (display-buffer-in-side-window
     buffer `((side . left) (slot . 0)
              (window-width . 0.2)
              (preserve-size . (t . nil))
              (window-parameters . ((no-delete-other-windows . t)
                                    (dedicated . t)))))
    (select-window (get-buffer-window buffer))))

(defun zw/dired-sidebar-find-file ()
  (interactive)
  (dired-find-file)
  (zw-dired-sidebar-mode 1))

(defun zw/dired-sidebar-up-directory ()
  (interactive)
  (dired-up-directory)
  (zw-dired-sidebar-mode 1))

(defun zw/dired-sidebar-open-in-dired ()
  (interactive)
  (let* ((dir (dired-current-directory)))
    (kill-buffer (current-buffer))
    (dired dir)
    (message (format "open %s in dired" dir))))

(defun zw/dired-sidebar-modeline-major-mode ()
  "Sidebar modeline major mode."
  (propertize "Sidebar " 'face (zw/modeline-set-face 'zw/modeline-major-mode-active
                                                     'zw/modeline-default-inactive)))

(defun zw/dired-sidebar-modeline-directory (dir)
  "Sidebar modeline directory."
  (car (last (split-string dir "/") 2)))

(define-minor-mode zw-dired-sidebar-mode
  "Toggle zw-dired-sidebar mode."
  :lighter " Dired-Sidebar"
  :keymap
  `((,(kbd "s-q") . zw/kill-bufer-quit-window)
    (,(kbd "q") . zw/kill-bufer-quit-window)
    (,(kbd "s-b") . zw/kill-bufer-quit-window)
    (,(kbd "^") . zw/dired-sidebar-up-directory)
    (,(kbd "RET") . zw/dired-sidebar-find-file)
    (,(kbd "<mouse-2>") . zw/dired-sidebar-find-file)
    (,(kbd "C-x 1") . zw/dired-sidebar-open-in-dired))
  (let* ((dir (abbreviate-file-name (dired-current-directory)))
         (current-dir (zw/dired-sidebar-modeline-directory dir))
         (buffer (dired-noselect dir))
         (name (concat " :" dir)))
    (if zw-dired-sidebar-mode
        (with-current-buffer buffer
          (dired-hide-details-mode t)
          (rename-buffer name)
          (setq-local mode-line-format
                      (list "%e" " "
                            current-dir zw/modeline-separator
                            '(:eval (zw/modeline-remote))
                            '(:eval (zw/modeline-middle-space (zw/dired-sidebar-modeline-major-mode)))
                            '(:eval (zw/dired-sidebar-modeline-major-mode))))
          (set-window-dedicated-p (get-buffer-window buffer) 'dedicated))
      (with-current-buffer buffer
        (dired-hide-details-mode 0)
        (rename-buffer dir)
        (setq-local mode-line-format (default-value 'mode-line-format))
        (set-window-dedicated-p (get-buffer-window buffer) nil)))))

(provide 'zw-dired-sidebar)
