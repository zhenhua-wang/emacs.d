;; -*- lexical-binding: t -*-

;; * C/C++
(use-package cc-mode
  :bind ((:map c-mode-base-map
               ("C-c C-c" . compile)))
  :hook (c-mode-common . (lambda () (remove-hook 'flymake-diagnostic-functions
                                                 'flymake-cc t)))
  :config (advice-add #'c-indent-line-or-region :after
                      (lambda (&rest args) (deactivate-mark))))

;; * Python
(defun zw/python-start-shell-before-send-string (code-string)
  (if (python-shell-get-process)
      (python-shell-send-string code-string)
    (progn
      (setq-local python-shell-setup-codes nil)
      (let* ((python-shell-setup-codes (list code-string))
             (process (or (python-shell-get-process)
                          (run-python (python-shell-parse-command)
                                      (when (project-current) 'project) nil))))
        (save-selected-window
          (switch-to-buffer-other-window
           (process-buffer process))))))
  (sit-for 0.1)
  (goto-char (region-end))
  (deactivate-mark))

(defun zw/python-region-or-block-string (forward-func backward-func)
  (if mark-active
      (let ((beg (region-beginning))
            (end (region-end)))
        (buffer-substring-no-properties beg end))
    (let* ((current-line-empty-p
            (string-match-p "\\`\\s-*$" (thing-at-point 'line)))
           (beg (if current-line-empty-p
                    (point)
                  (save-excursion (funcall backward-func) (point))))
           (end (save-excursion (funcall forward-func) (point))))
      (set-mark beg) (goto-char end)
      (buffer-substring-no-properties beg end))))

(defun zw/python-shell-send-region-or-block ()
  (interactive)
  (zw/python-start-shell-before-send-string
   (zw/python-region-or-block-string 'forward-paragraph 'backward-paragraph)))

(defun zw/python-shell-send-buffer ()
  (interactive)
  (save-excursion
    (zw/python-start-shell-before-send-string (buffer-string))))

(defun zw/python-shell-send-line ()
  (interactive)
  (let ((beg (save-excursion (beginning-of-line) (point)))
        (end (save-excursion (end-of-line) (point))))
    (zw/python-start-shell-before-send-string
     (buffer-substring-no-properties beg end))))

(use-package python
  :bind ((:map python-mode-map
               ("C-c C-c" . zw/python-shell-send-region-or-block)
               ("C-c C-b" . zw/python-shell-send-buffer)
               ("C-<return>" . zw/python-shell-send-line)))
  :config (setq python-shell-dedicated 'project))

(use-package conda
  :if (executable-find "conda")
  :config
  (or (cl-loop for dir in (list conda-anaconda-home
                                "~/.anaconda"
                                "~/.miniconda"
                                "~/.miniconda3"
                                "~/.miniforge3"
                                "~/anaconda3"
                                "~/miniconda3"
                                "~/miniforge3"
                                "~/opt/miniconda3"
                                "/usr/bin/anaconda3"
                                "/usr/local/anaconda3"
                                "/usr/local/miniconda3"
                                "/usr/local/Caskroom/miniconda/base"
                                "~/.conda")
               if (file-directory-p dir)
               return (setq conda-anaconda-home (expand-file-name dir)
                            conda-env-home-directory (expand-file-name dir)))
      (message "Cannot find Anaconda installation"))
  ;; update conda environment
  (defun zw/conda-env-update ()
    (when (executable-find "ipython")
      (setq python-shell-interpreter "ipython"
            python-shell-interpreter-args
            "-i --simple-prompt --InteractiveShell.display_page=True")
      (add-to-list 'python-shell-completion-native-disabled-interpreters
                   "ipython"))
    (if (and (featurep 'lsp-mode) lsp-mode)
        (lsp-restart-workspace)))
  (defun zw/conda-set-env-vars ()
    (zw/conda-env-update)
    ;; HACK: set LD_LIBRARY_PATH after conda activate
    (setenv "LD_LIBRARY_PATH"
            (concat ":" (getenv "CONDA_PREFIX") "/lib/")))
  (defun zw/conda-unset-env-vars ()
    (zw/conda-env-update)
    (setenv "LD_LIBRARY_PATH"))
  (add-hook 'conda-postactivate-hook 'zw/conda-set-env-vars)
  (add-hook 'conda-postdeactivate-hook 'zw/conda-unset-env-vars))

;; * R
(use-package ess
  :defer t
  :commands R
  :hook
  (ess-mode . zw/ess-setup)
  (inferior-ess-mode . zw/inferior-ess-setup)
  (ess-jags-mode . zw/ess-indent)
  :bind ((:map ess-r-mode-map
               ("TAB" . zw/smart-tab)
               ("C-c c e" . ess-complete-object-name)
               ("C-c C-c" . zw/ess-send-region-or-block)))
  :config
  (require 'ess-site)
  (defun zw/ess-setup ()
    (setq-local mode-line-process nil))
  (defun zw/inferior-ess-setup ()
    ;; "Fixes a bug when `comint-prompt-read-only' in non-nil.
    ;; See https://github.com/emacs-ess/ESS/issues/300"
    (setq-local comint-use-prompt-regexp nil)
    (setq-local inhibit-field-text-motion nil)
    ;; setup modeline
    (setq-local mode-line-process
                '(:eval (concat ":run" (nth ess--busy-count ess-busy-strings)))))
  (defun zw/ess-indent ()
    (setq-local indent-line-function #'ess-r-indent-line))
  (defun zw/ess-send-region-or-block ()
    (interactive)
    (if mark-active
        (let ((beg (region-beginning))
              (end (region-end)))
          (ess-eval-region beg end 'nowait)
          (goto-char end))
      (progn (ess-eval-paragraph 'nowait)
             (forward-paragraph))))
  ;; fix freezing in macos by creating your process using pipe
  ;; https://emacs.stackexchange.com/questions/40603/process-input-seems-buggy-in-emacs-on-os-x
  ;; (setq process-connection-type nil)
  (setq ess-ask-for-ess-directory nil
        ess-nuke-trailing-whitespace-p t
        ess-style 'RStudio-
        ess-local-process-name "R"
        ess-use-company nil
        ess-use-flymake nil
        ess-eval-visibly-p 'nowait
        ess-R-font-lock-keywords
        '((ess-R-fl-keyword:keywords   . t)
          (ess-R-fl-keyword:constants  . t)
          (ess-R-fl-keyword:modifiers  . t)
          (ess-R-fl-keyword:fun-defs   . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:%op%       . t)
          (ess-fl-keyword:fun-calls    . t)
          (ess-fl-keyword:delimiters . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:numbers . t)
          (ess-R-fl-keyword:F&T . t)
          (ess-fl-keyword:= . t))))

(use-package ess-smart-assign
  :after ess
  :straight (ess-smart-assign :host github :repo "zhenhua-wang/ess-smart-assign"))

;; * Provide
(provide 'zw-lang)
