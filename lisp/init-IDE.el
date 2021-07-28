;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ess ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ess
  :ensure t
  :init
  (load "ess-autoloads")
  (require 'ess-site)
  (require 'ess-r-mode)
  :config
  ;; ess syntax highlight
  (setq ess-R-font-lock-keywords
	'((ess-R-fl-keyword:keywords   . t)
          (ess-R-fl-keyword:constants  . t)
          (ess-R-fl-keyword:modifiers  . t)
          (ess-R-fl-keyword:fun-defs   . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:%op%       . t)
          (ess-fl-keyword:fun-calls    . t)
          (ess-fl-keyword:numbers)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters)
          (ess-fl-keyword:=)
          (ess-R-fl-keyword:F&T))))

(use-package poly-R :ensure t)
(use-package julia-mode :ensure t)
;; end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; markdown mode;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-enable-math t))

;; end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ein jupyter;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ein
  :ensure t
  :init
  (require 'ein)
  (require 'ein-jupyter)
  :config
  (setq ein:polymode t))
;;end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Python ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package python-mode
  :ensure t
  :config
  (setq python-shell-completion-native-enable nil)        ; disable native completion
  (defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
  
  (add-hook 'python-mode-hook 'my/python-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  git  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  text  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package grip-mode :ensure t)
(use-package latex-preview-pane :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; theme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package all-the-icons-ibuffer
  :ensure t
  :init (all-the-icons-ibuffer-mode 1))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode))

(provide 'init-IDE)
