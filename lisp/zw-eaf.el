;; eaf
(use-package eaf
  :straight '(eaf :host github :repo "emacs-eaf/emacs-application-framework"
                  :files ("*"))
  :config
  ;; browser
  (require 'eaf-browser)
  (eaf-setq eaf-webengine-default-zoom "2")
  (eaf-setq eaf-browser-enable-adblocker t)
  (eaf-setq eaf-browser-dark-mode nil)
  (setq browse-url-browser-function 'eaf-open-browser)
  ;; pdf
  (require 'eaf-pdf-viewer)
  (eaf-setq eaf-pdf-default-zoom  2)
  (eaf-setq eaf-pdf-dark-mode "ignore")
  (bind-keys ("s-o" . eaf-open-this-buffer)))

(provide 'zw-eaf)
