;; -*- lexical-binding: t -*-

(use-package dashboard
  :hook ((dashboard-after-initialize . zw/dashboard-init)
         (dashboard-mode . zw/dashboard-init))
  :init
  (setq dashboard-startup-banner "~/.emacs.d/exwm/banner.png"
        dashboard-image-banner-max-width 700
        dashboard-center-content t
        dashboard-vertically-center-content t
        dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons
        dashboard-set-file-icons t
        dashboard-projects-backend 'project-el
        dashboard-projects-switch-function 'dired
        dashboard-footer-icon ""
        dashboard-footer-messages '("Happy hacking!"))
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-newline
                                    dashboard-insert-footer))
  (setq dashboard-items '((recents . 5)
                          (projects . 5)))
  (setq dashboard-navigator-buttons
        `(((,(nerd-icons-mdicon "nf-md-github" :height 1.1)
            "Homepage" "Browse homepage"
            (lambda (&rest _) (browse-url "https://github.com/zhenhua-wang/emacs.d")))
           (,(nerd-icons-mdicon "nf-md-update" :height 1.1)
            "Update" "Update Configuration"
            (lambda (&rest _) (zw/update-emacs-tangle-dotfiles))))))
  (defun zw/dashboard--modeline-name ()
    (propertize (concat " " default-directory " "
                        zw/modeline-separator)
                'face (zw/modeline-set-face 'zw/modeline-major-mode-active
                                            'zw/modeline-default-inactive)))
  (defun zw/dashboard-init ()
    (setq-local default-directory "~/"
                mode-line-format
                (list
                 "%e"
                 ;; left
                 '(:eval (zw/modeline-remote))
                 '(:eval (zw/dashboard--modeline-name))
                 '(:eval (zw/modeline-text-scale))
                 '(:eval (zw/modeline-bar))
                 ;; right
                 '(:eval (zw/modeline-middle-space (zw/modeline-rhs)))
                 '(:eval (zw/modeline-rhs))))
    (ignore-errors (dashboard-jump-to-recents)))
  (dashboard-setup-startup-hook))

(provide 'zw-dashboard)
