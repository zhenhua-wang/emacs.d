;;;;;;;;;;;;;;;;;;;;;;;;;;;;; R auto complete;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "M-,") 'company-select-next)
(define-key company-active-map (kbd "M-k") 'company-select-previous)
(define-key company-active-map [return] nil)
(define-key company-active-map [tab] 'company-complete-common)
(define-key company-active-map (kbd "TAB") 'company-complete-common)
(define-key company-active-map (kbd "M-TAB") 'company-complete-selection)
(setq company-selection-wrap-around t
      company-tooltip-align-annotations t
      company-idle-delay 0.36
      company-minimum-prefix-length 2
      company-tooltip-limit 10)
;; end.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; markdown mode;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq markdown-enable-math t)
;; end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ein jupyter;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ein)
(require 'ein-jupyter)
(setq ein:polymode t)
(setq ein:completion-backend 'ein:use-company-backend)
;;end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org-mode;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(setq org-src-tab-acts-natively t)
(setq org-src-fontify-natively t)
;;end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; interpreters;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "--simple-prompt --pprint") ;; prevent emacs from freezing

(setq org-babel-python-command "python3.6")
;;end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  themes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq treemacs-no-png-images t)
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;;end
