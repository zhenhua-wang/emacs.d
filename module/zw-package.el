;; -*- lexical-binding: t -*-

;; straight
(with-eval-after-load "straight"
  (setq straight--native-comp-available nil
        straight-disable-native-compile t))
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; integrate with use-package
(setq straight-use-package-by-default t
      ;; disable checking at start-up
      straight-check-for-modifications '(watch-files find-when-checking))

(provide 'zw-package)
