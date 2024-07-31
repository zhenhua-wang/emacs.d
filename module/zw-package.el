;; -*- lexical-binding: t -*-

;; config
(setq straight-repository-branch "develop"
      straight-use-package-by-default t)
(when (executable-find "watchexec")
  (setq straight-check-for-modifications '(watch-files find-when-checking)))
;; bootstrap
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

(provide 'zw-package)
