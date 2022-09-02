;; eaf
(add-to-list 'load-path "~/Workspace/emacs-application-framework/")
(require 'eaf)
(require 'eaf-org-previewer)
(require 'eaf-browser)
(require 'eaf-image-viewer)
(require 'eaf-pdf-viewer)
(require 'eaf-jupyter)
(require 'eaf-markdown-previewer)
(eaf-setq eaf-webengine-default-zoom  2)
(eaf-setq eaf-pdf-default-zoom  2)
(eaf-setq eaf-pdf-dark-mode "ignore")

(provide 'zw-eaf)
