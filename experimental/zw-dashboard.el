;; -*- lexical-binding: t -*-

(use-package dashboard
  :hook ((dashboard-after-initialize . zw/dashboard-init)
         (dashboard-mode . zw/dashboard-init))
  :custom
  (dashboard-startup-banner "~/.emacs.d/resources/images/banner.png")
  (dashboard-image-banner-max-width 700)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-file-icons t)
  (dashboard-projects-backend 'project-el)
  (dashboard-projects-switch-function 'dired)
  (dashboard-startupify-list '(dashboard-insert-banner
                               dashboard-insert-newline
                               dashboard-insert-navigator
                               dashboard-insert-newline
                               dashboard-insert-banner-title
                               dashboard-insert-items
                               dashboard-insert-newline
                               dashboard-insert-init-info))
  (dashboard-items '((agenda . 5)
                     ;; (recents . 5)
                     (projects . 5)))
  (dashboard-navigator-buttons
   `(((,(nerd-icons-mdicon "nf-md-github" :height 1.1)
       "Homepage" "Browse homepage"
       (lambda (&rest _) (browse-url "https://github.com/zhenhua-wang/emacs.d")))
      (,(nerd-icons-mdicon "nf-md-update" :height 1.1)
       "Update" "Update Configuration"
       (lambda (&rest _) (zw/update-emacs-tangle-dotfiles))))))
  :init
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
                 '(:eval (zw/modeline-bar))
                 ;; left
                 '(:eval (zw/modeline-remote))
                 '(:eval (zw/dashboard--modeline-name))
                 '(:eval (zw/modeline-text-scale))
                 ;; right
                 '(:eval (zw/modeline-middle-space (zw/modeline-rhs)))
                 '(:eval (zw/modeline-rhs))))
    (ignore-errors (dashboard-jump-to-recents)))
  (dashboard-setup-startup-hook)
  :config
  (when (not (display-graphic-p))
    (let ((banner-ascii (expand-file-name
                         "0.txt"
                         dashboard-banners-directory)))
      (when (not (file-exists-p banner-ascii))
        (make-symbolic-link (file-truename "~/.emacs.d/resources/images/banner.txt") banner-ascii)))
    (setq dashboard-startup-banner 0)))

(provide 'zw-dashboard)
