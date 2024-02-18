;; -*- lexical-binding: t -*-

(defun zw/headerline-keymap (function)
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'header-line 'mouse-1) function)
    map))

(defun zw/headerline-debug ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_alt") " ")
              'face 'success
              'mouse-face 'highlight
              'keymap (zw/headerline-keymap 'zw/dape-in-path)))

(defun zw/headerline-next ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_line_by_line") " ")
              'face 'warning
              'mouse-face 'highlight
              'keymap (zw/headerline-keymap 'dape-next)))

(defun zw/headerline-continue ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_continue_small") " ")
              'face 'warning
              'mouse-face 'highlight
              'keymap (zw/headerline-keymap 'dape-continue)))

(defun zw/headerline-quit ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_stop") " ")
              'face 'error
              'mouse-face 'highlight
              'keymap (zw/headerline-keymap 'dape-quit)))

(defun zw/headerline-rerun ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_rerun") " ")
              'face 'error
              'mouse-face 'highlight
              'keymap (zw/headerline-keymap 'dape-restart)))

(defun zw/headerline-begin ()
  (let ((color (face-background 'tab-line))
        (width 1)
        (height (floor (* (string-pixel-width " ")
                          2.5))))
    (zw/modeline--begin color width height)))

(defvar zw/headerline-supported-mode '(python-mode-hook))
(dolist (mode zw/headerline-supported-mode)
  (add-hook mode 'zw-header-line-mode))

;; define zw-header-line-mode
(define-minor-mode zw-header-line-mode
  "zw header-line mode."
  :global nil
  (if zw-header-line-mode
      (progn
        (setq-local header-line-format
                    (list '(:eval (zw/headerline-begin))
                          '(:eval (zw/headerline-debug))
                          '(:eval (zw/headerline-next))
                          '(:eval (zw/headerline-continue))
                          '(:eval (zw/headerline-quit))
                          '(:eval (zw/headerline-rerun))))
        (redraw-frame))
    (setq-local header-line-format (default-value 'header-line-format))))

(provide 'zw-header-line)
