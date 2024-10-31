(require 'zw-dashboard)
(require 'zw-theme-extra)
;; (require 'zw-eaf)
;; (require 'zw-copilot)

(add-hook 'zw/after-set-theme-hook 'zw/ui-padding-mode)
(add-hook 'after-init-hook 'keycast-tab-bar-mode)

(setq zw/modeline-remote-show-local nil
      zw/tab-line-show-debug nil)

;; auto complete
;; (setq company-idle-delay 0
;;       company-frontends '(company-pseudo-tooltip-frontend
;;                           company-echo-metadata-frontend))

;; default maximize all frames
;; (push '(fullscreen . maximized) default-frame-alist)
;; (push '(undecorated . t) default-frame-alist)

;; left tool-bar
;; (push '(tool-bar-lines . 1)   initial-frame-alist)
;; (push '(tool-bar-position . left)   initial-frame-alist)
;; (setq tool-bar-style 'image)
