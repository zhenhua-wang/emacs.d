;; -*- lexical-binding: t -*-

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 2
        dashboard-center-content t
        dashboard-vertically-center-content t
        dashboard-set-navigator t
        dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons
        dashboard-set-file-icons t)
  (setq dashboard-items '((recents  . 5)))
  (setq dashboard-navigator-buttons
        `(((,(nerd-icons-codicon "nf-cod-github" :height 1.1 :v-adjust 0.0)
            "Homepage"
            "Browse homepage"
            (lambda (&rest _) (browse-url "https://github.com/zhenhua-wang/emacs.d")))))))

(provide 'zw-dashboard)
