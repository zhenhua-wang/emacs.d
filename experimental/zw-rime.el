;; -*- lexical-binding: t -*-

(use-package rime
  :init
  (setq default-input-method "rime"
        rime-show-preedit t
        rime-user-data-dir "~/.cache/emacs/rime/"
        rime-show-candidate 'posframe
        rime-posframe-properties (list :internal-border-width 2))
  ;; init user config
  (let* ((dir rime-user-data-dir)
         (config (expand-file-name "default.custom.yaml" dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (unless (file-exists-p config)
      (make-symbolic-link (expand-file-name "default.custom.yaml"
                                            "~/.local/share/fcitx5/rime/")
                          config t))))

(provide 'zw-rime)
