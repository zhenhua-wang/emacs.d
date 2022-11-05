(use-package jupyter
  :defer t
  :init
  ;; refresh jupyter kernel
  (defun zw/jupyter-refresh-kernelspecs ()
    (interactive)
    (if (fboundp 'jupyter-available-kernelspecs)
        (jupyter-available-kernelspecs t)))
  (advice-add #'conda-env-activate :after #'zw/jupyter-refresh-kernelspecs)
  (advice-add #'conda-env-deactivate :after #'zw/jupyter-refresh-kernelspecs))

(use-package ob-jupyter
  :defer t
  :straight (:type built-in)
  :commands (org-babel-execute:jupyter-python)
  :init
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:session . "py")
                                                       (:kernel . "python3")))
  (dolist (lang '(python R))
    (cl-pushnew (cons (format "jupyter-%s" lang) lang)
                org-src-lang-modes :key #'car))
  ;; enable jupyter-org-interaction-mode
  (advice-add #'org-babel-jupyter-initiate-session :after
              (lambda (&optional SESSION PARAMS)
                (jupyter-org-interaction-mode))))

(provide 'zw-jupyter)
