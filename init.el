;; load init
(org-babel-load-file "~/.emacs.d/emacs.org")

(setq initial-scratch-message nil)
;; print starting time to scratch
;; (defun greet-time ()
;;   (let ((current-hour (string-to-number (format-time-string "%H" (current-time)))))
;;     (cond
;;      ((or (>= current-hour 20) (< current-hour 6))
;;       "Good evening")
;;      ((and (>= current-hour 6)
;; 	   (< current-hour 12))
;;       "Good morning")
;;      (t "Good afternoon"))))

;;  scratch mode
;; (setq initial-major-mode 'org-mode)

;; Profile emacs startup
;; (let ((startup-time (format "%.2f seconds"
;; 			   (float-time
;; 			    (time-subtract after-init-time before-init-time)))))
;;      (setq initial-scratch-message (format "\n+ %s, %s! The init completed in =%s= with =%d= garbage collections.\n\n"
;; 				           (greet-time)
;; 				           user-login-name
;; 				           startup-time
;; 				           gcs-done)))


;; Silence compiler warnings for gcc-emacs
;; (if (eq system-type 'gnu/linux)
;;     (setq native-comp-async-report-warnings-errors nil))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(package-selected-packages
   '(yasnippet ws-butler wordnut which-key web-mode vterm visual-fill-column use-package super-save spacegray-theme rime request rainbow-delimiters pyvenv popper poly-R org-wild-notifier org-superstar org-roam org-present org-auto-tangle org-appear openwith nov nord-theme no-littering neotree minions magit-todos lua-mode lsp-ui lsp-treemacs lsp-ivy ivy-prescient impatient-mode highlight-indent-guides gruvbox-theme grip-mode flycheck flx fish-completion exwm exec-path-from-shell evil-nerd-commenter ess eshell-syntax-highlighting eshell-prompt-extras eshell-git-prompt esh-autosuggest doom-themes doom-modeline dired-single dired-ranger dired-rainbow dired-collapse desktop-environment counsel-projectile company-prescient company-math company-jedi company-box command-log-mode cdlatex auctex all-the-icons-ivy-rich all-the-icons-ivy all-the-icons-ibuffer all-the-icons-dired adaptive-wrap)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-block-begin-line ((t (:background "#4C566A" :foreground "#ECEFF4" :bold t :height 1.0))))
 '(org-block-end-line ((t (:background "#4C566A" :foreground "#ECEFF4" :bold t :height 1.0))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "white" :font "JetBrainsMono Nerd Font" :height 2.0 :underline t))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-latex-and-related ((t (:foreground "#EBCB8B"))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "white" :font "JetBrainsMono Nerd Font" :height 1.75 :foreground "#5E81AC"))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "white" :font "JetBrainsMono Nerd Font" :height 1.5 :foreground "#88C0D0"))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "white" :font "JetBrainsMono Nerd Font" :height 1.25 :foreground "#D08770"))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "white" :font "JetBrainsMono Nerd Font" :height 1.1 :italic t :foreground "#FF8C94" :slant italic))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "white" :font "JetBrainsMono Nerd Font"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "white" :font "JetBrainsMono Nerd Font"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "white" :font "JetBrainsMono Nerd Font"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "white" :font "JetBrainsMono Nerd Font"))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
