(require 'zw-dashboard)
(require 'zw-theme-extra)
;; (require 'zw-eaf)
;; (require 'zw-copilot)

(add-hook 'zw/after-set-theme-hook 'zw/ui-padding-mode)
(add-hook 'after-init-hook 'keycast-tab-bar-mode)

(setq
 zw/modeline-remote-show-local nil
 zw/tab-line-show-debug nil
 ;; company-idle-delay 0
 )
