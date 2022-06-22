;; zw-ivy.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Ivy Mini-buffer completion configurations.
;;

;;; Code:

(use-package counsel
  :diminish ivy-mode counsel-mode
  :bind (("M-x" . counsel-M-x)
	 ("s-p" . counsel-projectile-switch-project)
	 ("C-x b" . switch-to-buffer)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c i" . counsel-imenu)
         ("C-c l" . 'counsel-search)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)
	 :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("s-<tab>" . ivy-next-line)
         ("<backtab>" . ivy-previous-line))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :init
  (setq enable-recursive-minibuffers t
	confirm-nonexistent-file-or-buffer t)
  (setq ivy-wrap t
	ivy-height 12
        ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
        ivy-fixed-height-minibuffer t
        ivy-count-format "[%d/%d] "
        ivy-ignore-buffers '("\\` " "\\`\\*tramp/" "\\`\\*xref" "\\`\\*helpful "
                             "\\`\\*.+-posframe-buffer\\*" "\\` ?\\*company-.+\\*")
        ivy-on-del-error-function #'ignore
        ivy-initial-inputs-alist nil)
  :config
  (setq counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  (if (featurep 'xwidget-internal)
      (setq browse-url-browser-function 'xwidget-webkit-browse-url))
  (setq counsel-search-engine 'google))

(use-package all-the-icons-ivy-rich
  :hook (ivy-mode . all-the-icons-ivy-rich-mode)
  :init (setq all-the-icons-ivy-rich-color-icon t))

(use-package ivy-rich
  :hook
  (counsel-mode . ivy-rich-mode)
  (ivy-rich-mode . ivy-rich-project-root-cache-mode)
  (ivy-rich-mode . (lambda ()
                     "Use abbreviate in `ivy-rich-mode'."
                     (setq ivy-virtual-abbreviate
                           (or (and ivy-rich-mode 'abbreviate) 'name))))
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :after ivy
  :init
  (setq ivy-flx-limit 10000))

;; precscient
(use-package ivy-prescient
  :hook
  (counsel-mode . ivy-prescient-mode)
  :init
  (setq ivy-prescient-enable-filtering t))

(use-package prescient
  :hook
  (counsel-mode . prescient-persist-mode)
  :init
  (setq prescient-sort-length-enable t))

(use-package ivy-yasnippet
  :bind ("C-c y" . ivy-yasnippet))

;; search bibtex using ivy
(use-package ivy-bibtex
  :defer t
  :bind
  ;; keys for bib
  ("H-p" . ivy-bibtex))


;; Correcting words with flyspell via Ivy
(use-package flyspell-correct-ivy
  :after flyspell-correct
  :init (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package counsel-projectile
  :hook (counsel-mode . counsel-projectile-mode)
  :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point)))

(provide 'zw-ivy)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zw-ivy.el ends here
