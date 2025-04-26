(require 'zw-dashboard)
(require 'zw-theme-extra)
;; (require 'zw-eaf)
;; (require 'zw-copilot)
;; (require 'zw-tool-bar)
;; (require 'zw-theme-auto)
;; (setq zw/theme-auto-enable 'gui)

(add-hook 'zw/after-set-theme-hook 'zw/ui-padding-mode)

(setq zw/modeline-remote-show-local nil
      zw/tab-line-show-debug nil)

;; auto complete
(setq company-idle-delay 0
      company-frontends '(company-pseudo-tooltip-frontend
                          company-echo-metadata-frontend))

;; disable preselection
;; (setq company-selection-default nil)

;; default maximize all frames
;; (push '(fullscreen . maximized) default-frame-alist)
;; (push '(undecorated . t) default-frame-alist)
