;; -*- lexical-binding: t -*-

(use-package rime
  :init
  (setq default-input-method "rime"
        rime-show-preedit t
        rime-user-data-dir "~/.cache/emacs/rime/"
        rime-show-candidate 'posframe
        rime-posframe-properties (list :internal-border-width 2))
  :config
  ;; init user config
  (let* ((dir rime-user-data-dir)
         (config (expand-file-name "default.custom.yaml" dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (unless (file-exists-p config)
      (make-symbolic-link (expand-file-name "default.custom.yaml"
                                            "~/.local/share/fcitx5/rime/")
                          config t)))
  ;; rime finalize
  (add-hook 'kill-emacs-hook (lambda () (ignore-errors (rime-lib-finalize)))))

(provide 'zw-rime)
