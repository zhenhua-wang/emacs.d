;; eaf
(add-to-list 'load-path "~/Workspace/emacs-application-framework/")
(require 'eaf)
(require 'eaf-pdf-viewer)
(eaf-setq eaf-pdf-default-zoom  2)
(eaf-setq eaf-pdf-dark-mode "ignore")
(bind-keys :map eaf-mode-map
           ("p" . eaf-py-proxy-scroll_up_page)
           ("n" . eaf-py-proxy-scroll_down_page)
           ("+" . eaf-py-proxy-zoom_in))

(provide 'zw-eaf)
