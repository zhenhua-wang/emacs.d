;; zw-company.el --- Initialize company configurations.	-*- lexical-binding: t -*-

(use-package company
  :hook
  (after-init . global-company-mode)
  (company-mode . yas-minor-mode)
  (ess-r-mode . company-mode)
  :bind ((:map company-mode-map
               ("M-<tab>" . company-complete)
               ("C-<tab>" . company-yasnippet))
         (:map company-active-map
               ("<escape>" . company-abort)
               ("M->" . company-select-last)
               ("M-<" . company-select-first)
               ("<tab>" . company-complete-selection)
               ("C-<tab>" . company-yasnippet)))
  :init (setq company-idle-delay 0
              company-require-match nil
              company-selection-wrap-around t
              company-minimum-prefix-length 1
              company-abort-on-unique-match nil
              company-icon-size '(auto-scale . 20)
              company-icon-margin 2
              company-tooltip-limit 12
              company-tooltip-align-annotations t
              company-tooltip-minimum-width 40
              company-tooltip-maximum-width 80
              company-dabbrev-minimum-length 4
              company-dabbrev-ignore-invisible t
              company-dabbrev-ignore-case 'keep-prefix
              company-dabbrev-downcase 'case-replace
              company-dabbrev-other-buffers 'all
              company-dabbrev-code-other-buffers t
              company-dabbrev-char-regexp "[[:word:]_-]+"
              company-dabbrev-ignore-buffers "\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"
              company-transformers '(company-sort-prefer-same-case-prefix)
              company-global-modes '(not message-mode help-mode
                                         vterm-mode eshell-mode)
              company-backends '(company-files
                                 company-capf
                                 company-dabbrev))
  ;; remove completions that start with numbers
  (push (apply-partially #'cl-remove-if
                         (lambda (c) (string-match-p "\\`[0-9]+" c)))
        company-transformers)
  :config
  ;; set tooltip themes
  (set-face-attribute 'company-tooltip nil
                      :foreground (face-foreground 'tooltip)
                      :background (face-background 'tooltip)
                      :inherit 'fixed-pitch)
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground (face-foreground 'warning)
                      :underline (face-foreground 'warning)
                      :background 'unspecified
                      :weight 'bold))

(use-package company-prescient
  :hook
  (company-mode . company-prescient-mode)
  (company-prescient-mode . prescient-persist-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; frontend ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company-posframe
  :hook
  (company-mode . company-posframe-mode)
  :bind ((:map company-posframe-active-map
               ("s-d" . company-posframe-quickhelp-toggle)
               ("s-n" . company-posframe-quickhelp-scroll-up)
               ("s-p" . company-posframe-quickhelp-scroll-down)))
  :config
  (setq company-posframe-quickhelp-delay nil
        company-posframe-show-metadata nil
        company-posframe-show-indicator t
        company-posframe-font zw/font-fixed-pitch
        company-posframe-show-params
        (list :override-parameters
              '((tab-bar-mode . 0)
                (tab-bar-format . nil)
                (tab-line-format . nil)
                (tab-bar-lines . 0)
                (tab-bar-lines-keep-state . 0))))
  (defun company-enable-in-minibuffer ()
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (company-mode 1)))
  (add-hook 'minibuffer-setup-hook #'company-enable-in-minibuffer)
  (set-face-attribute 'company-posframe-active-backend-name nil
                      :inherit 'company-tooltip
                      :foreground (face-foreground 'font-lock-keyword-face))
  (set-face-attribute 'company-posframe-inactive-backend-name nil
                      :inherit 'company-tooltip))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; backend ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun company-R-objects--prefix ()
  (unless (ess-inside-string-or-comment-p)
    (let ((start (ess-symbol-start)))
      (when start
        (buffer-substring-no-properties start (point))))))

(defun company-R-objects--candidates (arg)
  (let ((proc (ess-get-next-available-process)))
    (when proc
      (with-current-buffer (process-buffer proc)
        (all-completions arg (ess--get-cached-completions arg))))))

(defun company-capf-with-R-objects--check-prefix (prefix)
  (or (cl-search "$" prefix)
      (cl-search ":" prefix)))

(defun company-capf-with-R-objects (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-R-objects))
    (prefix (company-R-objects--prefix))
    (candidates (if (company-capf-with-R-objects--check-prefix arg)
                    (company-R-objects--candidates arg)
                  (company-capf command arg)))
    (annotation (if (company-capf-with-R-objects--check-prefix arg)
                    "R-object"
                  (company-capf command arg)))
    (kind (if (company-capf-with-R-objects--check-prefix arg)
              'field
            (company-capf command arg)))
    (doc-buffer (company-capf command arg))))

;; backends for prog-mode
(dolist (mode '(prog-mode-hook
                minibuffer-setup-hook
                inferior-python-mode-hook))
  (add-hook mode
            (lambda ()
              (setq-local company-backends
                          '(company-capf company-files)))))
;; backends for ess-r-mode
(add-hook 'ess-r-mode-hook
          (lambda ()
            (setq-local company-backends
                        ;; '(company-R-library company-R-objects company-files)
                        '(company-capf-with-R-objects company-files))))
(add-hook 'inferior-ess-r-mode-hook
          (lambda ()
            (setq-local company-backends
                        '(company-R-library company-R-objects company-files))))
;; backends for shell
(use-package company-shell
  :commands (company-shell)
  :hook (sh-mode . (lambda ()
                     (interactive)
                     (setq-local company-backends '(company-shell)))))

(provide 'zw-company)
