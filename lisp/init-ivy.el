;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ivy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package counsel :ensure t)
(use-package ivy
  :ensure t
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t))

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1)
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x b") 'counsel-ibuffer)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

;; icons
(use-package all-the-icons-ivy
  :ensure t
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

;; precscient
(use-package ivy-prescient
  :ensure t
  :after counsel
  :config
  (ivy-prescient-mode 1)
  :custom
  (setq ivy-prescient-enable-filtering nil))

(use-package prescient
  :ensure t
  :after counsel
  :config
  (prescient-persist-mode 1)
  (setq prescient-sort-length-enable nil))

(provide 'init-ivy)
