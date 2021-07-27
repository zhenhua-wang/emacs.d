;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ivy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a package show completion buffer
(require 'ivy-rich)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq enable-recursive-minibuffers t)
(ivy-rich-mode 1)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x b") 'counsel-ibuffer)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ess ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "ess-autoloads")
(require 'ess-site)
(require 'ess-r-mode)
;; end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; markdown mode;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq markdown-enable-math t)
;; end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ein jupyter;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ein)
(require 'ein-jupyter)
(setq ein:polymode t)
;;(setq ein:completion-backend 'ein:use-company-backend)
;;end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Python completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org-mode;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ipython . t)
   (R . t)
   (ein . t)))

(setq org-confirm-babel-evaluate nil)
;;(setq org-src-tab-acts-natively t)
;;(setq org-hide-emphasis-markers t)
(setq org-src-fontify-natively t)
(setq org-ellipsis " ▾")

;; change headline bullets
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;;; Scrolling.
;; Good speed and allow scrolling through large images (pixel-scroll).
(pixel-scroll-mode)
(setq pixel-dead-time 0) ; Never go back to the old scrolling behaviour.
(setq pixel-resolution-fine-flag t) ; Scroll by number of pixels instead of lines (t = frame-char-height pixels).
(setq mouse-wheel-scroll-amount '(1)) ; Distance in pixel-resolution to scroll each mouse wheel event.
(setq mouse-wheel-progressive-speed nil) ; Progressive speed is too fast for me.

(let* ((variable-tuple
        (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
  
  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;; keys for create a python workspace
(add-hook 'org-mode-hook '(lambda ()
			    (flyspell-mode 1)
			    
			    (defun split-python-window ()
			      (interactive)
			      (delete-other-windows)
			      (split-window-right)
			      (windmove-right)
			      (org-edit-src-code)
			      (split-window-below)
			      (windmove-down)
			      (run-python)
			      (switch-to-buffer "*Python*")
			      (windmove-left))))
			   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; interpreters;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setq python-shell-interpreter "ipython3"
;;      python-shell-interpreter-args "--simple-prompt --pprint") ;; prevent emacs from freezing
;;(setq org-babel-python-command "python3")
;;(setq ob-ipython-command "ipython3")
;;end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  themes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doom
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;;end
