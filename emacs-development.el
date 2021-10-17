(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/yasnippet"))
  ;; (add-to-list 'company-backends 'company-yasnippet)
  (yas-global-mode 1))

(use-package lsp-mode
  :disabled
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-print-io t) ;; print some debug info
  :bind
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :hook (
	 ;; if you want which-key integration
	 (lsp-mode . lsp-enable-which-key-integration)
	 ;; find out where you are at your src
	 (lsp-mode . efs/lsp-mode-setup))
  :commands (lsp lsp-deferred)
  )

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-jedi
  :disabled
  ;; :after lsp-mode
  :config
  (add-to-list 'lsp-disabled-clients 'pyls)
  (add-to-list 'lsp-enabled-clients 'jedi))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  lsp-ui-doc-position 'bottom)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package eglot
  :disabled
  :hook
  ;; i don't plan to use auto-hook for now
  ;; (python-mode . eglot-ensure)
  )

(use-package ess
  :defer t
  :mode "\\.R|.r\\'"
  :config
  (if (eq system-type 'windows-nt)
      (progn
        (setq ess-directory-containing-R "C:/Program Files/")
        ;; Add to list of prefixes recognized by ESS.
        ;; Matching versions will appear after doing M-x R <TAB> <TAB>
        (setq ess-r-versions '("R-1" "R-2" "R-3" "R-current" "R-devel" "R-patched"))
        (setq inferior-R-program-name "C:/Program Files/R/R-4.1.1/bin/x64/Rterm.exe")))
  (require 'ess-site)
  ;; (require 'ess-r-mode)
  ;; ess syntax highlight
  (setq ess-default-style 'RStudio-)
  ;; Do not ask for ess startup location
  (setq ess-ask-for-ess-directory nil)
  ;; To make a new process start with just *R* for the below
  ;; shift enter
  ;; see https://github.com/emacs-ess/ESS/issues/1073
  (setq ess-gen-proc-buffer-name-function 'ess-gen-proc-buffer-name:simple)
  ;; The name of the ESS process associated with the buffer.
  (setq ess-local-process-name "R")
  (setq ansi-color-for-comint-mode 'filter)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t)
  ;; company completion
  ;; (setq ess-r-company-backends
        ;; '((company-tabnine company-R-library company-R-args company-R-objects :separate)))
  )

(use-package markdown-mode
  ;;:ensure auctex
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.Rmd\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  (markdown-enable-math t)
  :config
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'markdown-mode-hook 'adaptive-wrap-prefix-mode)
  )

(use-package poly-markdown
  :ensure poly-R
  :ensure poly-noweb
  :ensure polymode
  :ensure adaptive-wrap
  :config
  ;; R/tex polymodes
  (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
  (setq markdown-enable-math t)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  )

;; (use-package python-mode
;;   :custom
;;   (python-shell-interpreter "~/anaconda3/bin/python")
;;   :config
;;   (setq python-shell-completion-native-enable nil)        ; disable native completion  
;;   )

(use-package pyvenv
  :hook ((python-mode . pyvenv-mode))
  :config
  (setenv "WORKON_HOME" "~/anaconda3/envs")
  ;; (setenv "WORKON_HOME" (concat (getenv "CONDA_PREFIX") "/envs"))
  (pyvenv-mode 1))

(use-package company-jedi
  ;; :disabled
  ;; :hook (python-mode . jedi:setup) ; dont use this, since we want to use jedi in org-babel
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  (setq python-shell-completion-native-enable nil)
  (defun config/enable-company-jedi ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'config/enable-company-jedi)
  )

(use-package ein
  :disabled
  :init
  (require 'ein-jupyter)
  :config
  (setq ein:polymode t)
  )

(use-package lua-mode)

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

(use-package magit
  :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :defer t)

(use-package which-key
  :defer 1
  :init 
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

;; add comment to your codes
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; theme
(use-package all-the-icons-ibuffer
  :init (all-the-icons-ibuffer-mode 1))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode))

;; Automatically clean whitespace
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))
