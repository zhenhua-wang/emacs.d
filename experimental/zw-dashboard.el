;; -*- lexical-binding: t -*-

(use-package dashboard
  :hook (dashboard-mode . zw/dashboard-init)
  :init
  (setq dashboard-startup-banner 2
        dashboard-center-content t
        dashboard-vertically-center-content t
        dashboard-set-navigator t
        dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons
        dashboard-set-file-icons t)
  (setq dashboard-items '((recents  . 5)))
  (setq dashboard-navigator-buttons
        `(((,(nerd-icons-mdicon "nf-md-github" :height 1.1)
            "Homepage"
            "Browse homepage"
            (lambda (&rest _) (browse-url "https://github.com/zhenhua-wang/emacs.d")))
           (,(nerd-icons-mdicon "nf-md-update" :height 1.1)
            "Update" "Update Configuration"
            (lambda (&rest _) (zw/update-emacs-tangle-dotfiles))))))
  (defun zw/dashboard-init ()
    (setq-local mode-line-format
                (list
                 "%e"
                 ;; left
                 '(:eval (zw/modeline-remote))
                 '(:eval (zw/modeline-buffer-name 30 "..."))
                 '(:eval (zw/modeline-text-scale))
                 '(:eval (zw/modeline-bar))
                 ;; right
                 '(:eval (zw/modeline-middle-space (zw/modeline-rhs)))
                 '(:eval (zw/modeline-rhs)))))
  (dashboard-setup-startup-hook))

(provide 'zw-dashboard)
