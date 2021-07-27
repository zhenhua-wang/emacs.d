;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; toggle between poly-org and org
(add-hook 'org-mode-hook
  (lambda ()
   (local-set-key (kbd "C-c p") 'poly-org-mode)))
(add-hook 'org-mode-hook
  (lambda ()
   (local-set-key (kbd "C-c o") 'org-mode-restart)))
