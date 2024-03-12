;; -*- lexical-binding: t -*-

(use-package pyim
  :commands (pyim-cregexp-build)
  :bind ((:map pyim-mode-map
               ("," . pyim-previous-page)
               ("." . pyim-next-page)
               ("[" . pyim-previous-page)
               ("]" . pyim-next-page)
               ("<left>" . pyim-backward-point)
               ("<right>" . pyim-forward-point)
               ("C-\\" . pyim/toggle-input-method)
               ("s-\\" . pyim/toggle-input-method)))
  :init
  (setq default-input-method "pyim"
        pyim-page-tooltip 'posframe
        pyim-page-posframe-border-width 2
        pyim-default-scheme 'quanpin
        pyim-page-style 'two-line
        pyim-page-length 9
        pyim-cloudim 'google)
  ;; vertico search pinyin
  (defun pyim-orderless-regexp (orig-func component)
    (let ((result (funcall orig-func component)))
      (pyim-cregexp-build result)))
  (advice-add 'orderless-regexp :around #'pyim-orderless-regexp)
  :config
  ;; toggle input entered pinyin
  (defun pyim/toggle-input-method ()
    (interactive)
    (let ((word (pyim-entered-get)))
      (pyim-quit-clear)
      (funcall-interactively #'toggle-input-method)
      (insert word))))

(use-package pyim-basedict
  :after pyim
  :config
  (pyim-basedict-enable))

(provide 'zw-pyim)
