;; -*- lexical-binding: t -*-

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :hook ((prog-mode . zw/activate-copilot))
  :bind (("C-<tab>" . copilot-complete)
         :map copilot-completion-map
         ("C-g" . 'copilot-clear-overlay)
         ("C-p" . 'copilot-previous-completion)
         ("C-n" . 'copilot-next-completion)
         ("C-<tab>" . 'copilot-accept-completion)
         ("C-<right>" . 'copilot-accept-completion-by-word)
         ("C-<down>" . 'copilot-accept-completion-by-line))
  :config
  (setq copilot-indent-offset-warning-disable t)
  (add-to-list 'copilot-disable-predicates #'company--active-p)
  (add-to-list 'copilot-disable-display-predicates #'company--active-p)
  (advice-add 'company-posframe-show :before 'copilot-clear-overlay)
  (defun zw/activate-copilot ()
    (if (> (buffer-size) copilot-max-char)
        (message "Buffer size exceeds copilot max char limit. Copilot will not be activated.")
      (copilot-mode 1))))

(provide 'zw-copilot)
