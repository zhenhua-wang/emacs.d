;; -*- lexical-binding: t -*-

(use-package treesit-auto
  :if (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  :hook ((after-init . global-treesit-auto-mode)
         (change-major-mode . zw/treesit-auto-remap))
  :mode (("\\.yml\\'" . yaml-ts-mode))
  :init
  (setq treesit-auto-install 'prompt)
  :config
  (defun zw/treesit-auto--get-buffer-recipe ()
    (seq-find
     (lambda (r)
       (ignore-errors
         (string-match (treesit-auto-recipe-ext r) (buffer-name))))
     (treesit-auto--selected-recipes)))
  (defun zw/treesit-auto-remap ()
    (when-let* ((recipe (zw/treesit-auto--get-buffer-recipe))
                (lang (treesit-auto-recipe-lang recipe))
                (ts-mode (treesit-auto-recipe-ts-mode recipe))
                (mode (treesit-auto-recipe-remap recipe))
                (mode-hook (intern (format "%s-hook" mode)))
                (mode-map (intern (format "%s-map" mode)))
                (ts-mode-hook (intern (format "%s-hook" ts-mode)))
                (ts-mode-map (intern (format "%s-map" ts-mode))))
      (with-eval-after-load lang
        (when (and (boundp mode-hook))
          (eval `(setf ,ts-mode-hook ,mode-hook)))
        (when (and (boundp mode-map))
          (eval `(setf ,ts-mode-map ,mode-map)))))))

(provide 'zw-treesit)
