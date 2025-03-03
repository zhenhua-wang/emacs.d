(require 'zw-dashboard)
(require 'zw-theme-extra)
;; (require 'zw-eaf)
;; (require 'zw-copilot)
;; (require 'zw-tool-bar)

(add-hook 'zw/after-set-theme-hook 'zw/ui-padding-mode)
(add-hook 'after-init-hook 'keycast-tab-bar-mode)

(setq zw/modeline-remote-show-local nil
      zw/tab-line-show-debug nil)

;; auto complete
(setq company-idle-delay 0
      company-selection-default nil
      company-frontends '(company-pseudo-tooltip-frontend
                          company-echo-metadata-frontend))

;; default maximize all frames
;; (push '(fullscreen . maximized) default-frame-alist)
;; (push '(undecorated . t) default-frame-alist)
