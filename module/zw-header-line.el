;; -*- lexical-binding: t -*-

(defun zw/headerline-keymap (function)
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'header-line 'mouse-1) function)
    map))

(defun zw/headerline-debug ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_alt") " ")
              'face 'success
              'keymap (zw/headerline-keymap 'zw/dape-in-path)))

(defun zw/headerline-next ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_line_by_line") " ")
              'face 'warning
              'keymap (zw/headerline-keymap 'dape-next)))

(defun zw/headerline-continue ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_continue_small") " ")
              'face 'warning
              'keymap (zw/headerline-keymap 'dape-continue)))

(defun zw/headerline-quit ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_stop") " ")
              'face 'error
              'keymap (zw/headerline-keymap 'dape-quit)))

(defun zw/headerline-rerun ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_rerun") " ")
              'face 'error
              'keymap (zw/headerline-keymap 'dape-restart)))

(defun zw/headerline-begin ()
  (let ((color (face-background 'tab-line))
        (width 1)
        (height (floor (* (string-pixel-width " ")
                          2.5))))
    (zw/modeline--begin color width height)))

(defun zw/headerline-rhs ()
  (concat (zw/headerline-debug)
          (zw/headerline-next)
          (zw/headerline-continue)
          (zw/headerline-quit)
          (zw/headerline-rerun)))

(use-package breadcrumb
  :config
  (setq breadcrumb-imenu-crumb-separator " > "
        breadcrumb-project-crumb-separator " / "))
(defun zw/headerline-breadcrumb ()
  (let ((breadcrumb (breadcrumb-imenu-crumbs)))
    (if (and breadcrumb (not (string= "" breadcrumb)))
        breadcrumb
      (breadcrumb-project-crumbs))))

(add-hook 'prog-mode-hook 'zw-header-line-mode)
(defvar zw/headerline-disable-mode '(emacs-lisp-mode-hook
                                     polymode-init-inner-hook))
(dolist (mode zw/headerline-disable-mode)
  (add-hook mode (lambda () (zw-header-line-mode -1))))

;; define zw-header-line-mode
(define-minor-mode zw-header-line-mode
  "zw header-line mode."
  :global nil
  (if zw-header-line-mode
      (progn
        (setq-local header-line-format
                    (list '(:eval (zw/headerline-begin))
                          " "
                          '(:eval (zw/headerline-breadcrumb))
                          '(:eval (zw/modeline-middle-space (zw/headerline-rhs)))
                          '(:eval (zw/headerline-rhs))))
        (redraw-frame))
    (setq-local header-line-format (default-value 'header-line-format))
    (redraw-frame)))

(provide 'zw-header-line)
