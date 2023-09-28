;; -*- lexical-binding: t -*-

;; * PDF
(use-package pdf-tools
  :bind ((:map pdf-view-mode-map
               ("C-s" . isearch-forward)
               ("s-f" . isearch-forward)
               ("s-=" . pdf-view-enlarge)
               ("s-+" . pdf-view-enlarge)
               ("s--" . pdf-view-shrink)
               ("q" . kill-current-buffer)
               ("j" . pdf-view-next-line-or-next-page)
               ("k" . pdf-view-previous-line-or-previous-page)))
  :init
  (setq pdf-view-display-size 'fit-page
        pdf-view-use-imagemagick nil
        pdf-view-continuous nil
        pdf-view-use-scaling t
        pdf-annot-activate-created-annotations t)
  (pdf-loader-install))

;; * Provide
(provide 'zw-reader)
