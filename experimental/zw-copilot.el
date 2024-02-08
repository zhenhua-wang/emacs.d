;; -*- lexical-binding: t -*-

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :hook ((prog-mode . copilot-mode))
  :bind (("M-<iso-lefttab>" . copilot-complete)
         :map copilot-completion-map
         ("C-g" . 'copilot-clear-overlay)
         ("M-p" . 'copilot-previous-completion)
         ("M-n" . 'copilot-next-completion)
         ("M-<iso-lefttab>" . 'copilot-accept-completion)
         ("M-<right>" . 'copilot-accept-completion-by-word)
         ("M-<down>" . 'copilot-accept-completion-by-line))
  :config
  (add-to-list 'copilot-disable-predicates #'company--active-p)
  (add-to-list 'copilot-disable-display-predicates #'company--active-p)
  (setq copilot-indent-offset-warning-disable t))

(provide 'zw-copilot)
