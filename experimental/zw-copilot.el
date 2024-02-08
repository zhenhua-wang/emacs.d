;; -*- lexical-binding: t -*-

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :hook ((prog-mode . copilot-mode))
  :bind (("C-<tab>" . copilot-complete)
         :map copilot-completion-map
         ("C-g" . 'copilot-clear-overlay)
         ("C-p" . 'copilot-previous-completion)
         ("C-n" . 'copilot-next-completion)
         ("C-<tab>" . 'copilot-accept-completion)
         ("C-<right>" . 'copilot-accept-completion-by-word)
         ("C-<down>" . 'copilot-accept-completion-by-line))
  :config
  (add-to-list 'copilot-disable-predicates #'company--active-p)
  (add-to-list 'copilot-disable-display-predicates #'company--active-p)
  (setq copilot-indent-offset-warning-disable t))

(provide 'zw-copilot)
