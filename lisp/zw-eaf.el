;; eaf
(use-package eaf
  :straight '(eaf :host github :repo "emacs-eaf/emacs-application-framework"
                  :files ("*"))
  :demand
  :bind (("s-o" . eaf-open-this-buffer))
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
  (eaf-bind-key scroll_up_page "n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "p" eaf-pdf-viewer-keybinding))

(provide 'zw-eaf)
