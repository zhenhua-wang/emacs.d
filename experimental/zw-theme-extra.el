;; -*- lexical-binding: t -*-

(use-package modus-themes)
(use-package ef-themes)
(use-package lambda-themes
  :straight (:type git :host github :repo "zhenhua-wang/lambda-themes")
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch nil))
(use-package adwaita-dark-theme)

(provide 'zw-theme-extra)
