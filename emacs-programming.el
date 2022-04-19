(use-package lsp-mode
  :custom
  (lsp-completion-provider :none)
  (lsp-log-io nil)
  :init
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  :commands lsp)

;; optionally
(use-package lsp-ui
  :after lsp
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (lsp-ui-doc-show)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-imenu-auto-refresh t))

(use-package lsp-ivy
  :disabled
  :commands lsp-ivy-workspace-symbol)

(use-package dap-mode
  :hook
  (dap-stopped .
   (lambda (arg) (call-interactively #'debug-hydra)))
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (require 'dap-cpptools)
  (dap-cpptools-setup))

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
_b_: Toggle breakpoint   _dd_: Start debug      _de_: Edit debug template  _Q_: Quit debugging
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

(use-package eglot)
  :hook
  ;; (python-mode . eglot-ensure)
  ;; (ess-r-mode . eglot-ensure))

(use-package ccls
  :defer t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package ess
  :defer t
  :commands R
  :custom
  (ess-ask-for-ess-directory nil)
  (ess-style 'RStudio-)
  (ess-local-process-name "R")
  (ansi-color-for-comint-mode 'filter)
  (comint-scroll-to-bottom-on-input t)
  (comint-scroll-to-bottom-on-output t)
  (comint-move-point-for-output t)
  :config
  (require 'ess-site))

(use-package markdown-mode
  ;;:ensure auctex
  :commands (markdown-mode gfm-mode)
  ;; :mode (("README\\.md\\'" . gfm-mode)
  ;;        ("\\.md\\'" . markdown-mode)
  ;;        ("\\.markdown\\'" . markdown-mode)
  ;;        ("\\.Rmd\\'" . markdown-mode))
  ;; :init (setq markdown-command "multimarkdown")
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  (markdown-enable-math t)
  :config
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'markdown-mode-hook 'adaptive-wrap-prefix-mode)
  )

(use-package polymode
  :commands polymode-mode)

(use-package adaptive-wrap)

(use-package poly-markdown
  :commands (poly-markdown-mode poly-gfm-mode)
  :mode (("\\.md$" . poly-gfm-mode)
         ("\\.rmd$" . poly-gfm-mode)
         ("\\.markdown$" . poly-markdown-mode)))

(use-package poly-noweb
  :commands poly-noweb-mode)

(use-package poly-R
  :mode (("\\.Rmd" . poly-markdown+r-mode)
         ("\\.rmd" . poly-markdown+r-mode)
         ("\\.Rnw" . poly-noweb+r-mode)
         ("\\.rnw" . poly-noweb+r-mode)
         ))

(use-package pyvenv
  :hook ((python-mode . pyvenv-mode))
  :config
  (setenv "WORKON_HOME" (concat (exec-path-from-shell-copy-env "CONDA_PREFIX") "/envs"))
  (pyvenv-mode 1))

(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;; (use-package impatient-mode
;;   :ensure simple-httpd
;;   :ensure htmlize
;;   :config
;;   (require 'impatient-mode))

(use-package csv-mode
  :mode
  ("\\.[Cc][Ss][Vv]\\'". csv-mode)
  :hook
  (csv-mode . csv-align-mode)
  :config
  (setq csv-separators '("," ";" "|" " ")))

(use-package magit
  :ensure with-editor
  :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :defer t)

(use-package format-all)

(use-package highlight-indent-guides
  :hook ((prog-mode . highlight-indent-guides-mode))
  :custom
  (highlight-indent-guides-method 'character))

(use-package ws-butler
  :hook
  (prog-mode . ws-butler-mode))

;; check code syntax
(use-package flycheck
  :hook (prog-mode . flycheck-mode))
