;; -*- lexical-binding: t -*-

(use-package modus-themes)
(use-package ef-themes)
(use-package lambda-themes
  :straight (:type git :host github :repo "lambda-emacs/lambda-themes")
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch nil))

(provide 'zw-theme-extra)
