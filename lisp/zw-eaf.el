;; eaf
(add-to-list 'load-path "~/Workspace/emacs-application-framework/")
(require 'eaf)
(require 'eaf-browser)
(require 'eaf-pdf-viewer)
(eaf-setq eaf-pdf-default-zoom  2)
(eaf-setq eaf-pdf-dark-mode "ignore")
(eaf-setq eaf-webengine-default-zoom "2")
(eaf-setq eaf-browser-enable-adblocker t)
(eaf-setq eaf-browser-dark-mode nil)
(setq browse-url-browser-function 'eaf-open-browser)

(provide 'zw-eaf)
