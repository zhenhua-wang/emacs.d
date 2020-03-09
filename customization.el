;; start: R auto complete
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

;; start: markdown mode
(setq markdown-enable-math t)
;; end

;; start: ein jupyter
(setq ein:polymode t)
(setq ein:completion-backend '(ein:use-company-backend))
;;end

;; start: org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))
(setq org-src-tab-acts-natively t)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
;;end
