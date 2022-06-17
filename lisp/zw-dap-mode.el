(use-package treemacs
  :commands treemacs
  :config
  (defalias 'treemacs 'treemacs-display-current-project-exclusively)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package treemacs-all-the-icons
  :after treemacs
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package dap-mode
  :hook
  (after-init . dap-auto-configure-mode)
  (dap-stopped . (lambda (arg) (debug-hydra)))
  (dap-terminated . (lambda (args) (debug-hydra/nil)))
  (python-mode . (lambda () (require 'dap-python)))
  ((c-mode c++-mode objc-mode swift-mode) . (lambda ()
					      (require 'dap-cpptools)
					      (dap-cpptools-setup)))
  :init
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions controls)))

;; hydra mode in debug
(defun debug-hydra ()
  "Run `debug-hydra/body'."
  (interactive)
  (debug-hydra/body))
;; hydra key def
(defhydra debug-hydra (:color pink :hint nil :foreign-keys run)
  "
^debug mode^
^^^^^^^^----------------------------------------------------------------------------------------------------------------
_n_: Next                _c_: Continue          _i_: Step in               _o_: Step out
_ee_: Eval               _er_: Eval region      _ep_: Eval at point
_b_: Toggle breakpoint   _dd_: Start debug      _de_: Edit debug template
_Q_: Quit debugging
"
  ("dd" dap-debug)
  ("de" dap-debug-edit-template)
  ("b" dap-breakpoint-toggle)
  ("ee" dap-eval)
  ("er" dap-eval-region)
  ("ep" dap-eval-thing-at-point)
  ("Q" dap-delete-all-sessions :color red)
  ("n" dap-next)
  ("i" dap-step-in)
  ("o" dap-step-out)
  ("c" dap-continue)
  ("q" nil "quit" :color blue))

(provide 'zw-dap-mode)
