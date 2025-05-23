;; -*- lexical-binding: t -*-

;; * Environment
(use-package conda
  :commands (conda-env-activate conda-env-deactivate conda-env-candidates)
  :bind (("s-P" . zw/conda-env-activate)
         ("s-S-p" . zw/conda-env-activate))
  :init
  (defvar zw/conda-path '("/opt/anaconda/bin"
                          "/opt/miniconda3/bin"))
  (defvar zw/conda-executable-path nil)
  (dolist (conda-path zw/conda-path)
    (let ((conda-exec (concat conda-path "/conda")))
      (when (file-exists-p conda-exec)
        ;; (setenv "PATH" (concat conda-path ":" (getenv "PATH")))
        (setq conda--executable-path conda-exec))))
  ;; quick activate
  (defun zw/conda-env-activate ()
    (interactive)
    (let* ((deactivate "Conda deactivate")
           (env (completing-read
                 "Conda switch environment:"
                 (append (conda-env-candidates) `(,deactivate)))))
      (if (string= env deactivate)
          (conda-env-deactivate)
        (conda-env-activate env))))
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args
        "-i --simple-prompt --InteractiveShell.display_page=True")
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "ipython")
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
    ;; refresh current buffer
    (when (buffer-file-name)
      (revert-buffer-quick)))
  (defun zw/conda-postactivate ()
    (zw/conda-env-update)
    ;; set LD_LIBRARY_PATH after conda activate
    ;; (setenv "LD_LIBRARY_PATH"
    ;;         (concat ":" (getenv "CONDA_PREFIX") "/lib/"))
    ;; set PROJ_LIB for using ggplot with sf in R
    (setenv "PROJ_LIB"
            (concat (getenv "CONDA_PREFIX") "/share/proj/"))
    (message "Switched to conda environment: %s\n
In case of any error, you might want to install in your env:
conda install -c conda-forge glib libxkbcommon gcc=12.1.0 ncurses"
             (conda-env-name-to-dir conda-env-current-name)))
  (defun zw/conda-postdeactivate ()
    (zw/conda-env-update)
    ;; (setenv "LD_LIBRARY_PATH")
    (setenv "PROJ_LIB"))
  (add-hook 'conda-postactivate-hook 'zw/conda-postactivate)
  (add-hook 'conda-postdeactivate-hook 'zw/conda-postdeactivate))

;; * C/C++
(use-package cc-mode
  :bind ((:map c-mode-base-map
               ("C-c C-c" . compile))))

;; * Python
(defun zw/python-start-shell-before-send-string (code-string)
  (cond
   ((python-shell-get-process)
    (python-shell-send-string code-string))
   (t
    (let* ((process (save-selected-window
                      (run-python (python-shell-parse-command)
                                  (when (project-current) 'project) 'show))))
      ;; setup REPL
      (process-send-string
       process (concat "\n" python-shell-eval-setup-code "\n"
                       "\n" python-shell-eval-file-setup-code "\n"))
      (with-current-buffer (current-buffer)
        (let ((inhibit-quit nil))
          (run-hooks 'python-shell-first-prompt-hook)))
      ;; send code-string
      (process-send-string process (concat code-string "\n"))))))

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
   (zw/python-region-or-block-string 'forward-paragraph 'backward-paragraph))
  ;; deactivate mark after idle
  (sit-for 0.1)
  (goto-char (region-end))
  (deactivate-mark))

(defun zw/python-shell-send-buffer ()
  (interactive)
  (zw/python-start-shell-before-send-string
   (buffer-substring-no-properties (point-min) (point-max))))

(defun zw/python-shell-send-line ()
  (interactive)
  (let ((beg (save-excursion (beginning-of-line) (point)))
        (end (save-excursion (end-of-line) (point))))
    (zw/python-start-shell-before-send-string
     (buffer-substring-no-properties beg end)))
  (next-line))

(defun zw/python-shell-send-above ()
  (interactive)
  (zw/python-start-shell-before-send-string
   (buffer-substring-no-properties (point-min) (point))))

(use-package python
  :bind ((:map python-mode-map
               ("C-c C-d" . nil)
               ("C-c C-c" . zw/python-shell-send-region-or-block)
               ("C-c C-b" . zw/python-shell-send-buffer)
               ("C-c C-a" . zw/python-shell-send-above)
               ("C-<return>" . zw/python-shell-send-line)
               ("C-M-<left>" . python-nav-backward-sexp-safe)
               ("C-M-<right>" . python-nav-forward-sexp-safe)))
  :hook (inferior-python-mode . zw/right-side-window-mode)
  :config (setq python-shell-dedicated 'project))

;; register run repl
(defun zw/python-run-repl ()
  (interactive)
  (require 'python)
  (zw/repl-run-in-path-macro 'python-shell-interpreter 'run-python
                             (list nil (when (project-current) 'project) 'show)))
(add-to-list 'zw/repl-run-function '(python-mode . zw/python-run-repl))
(add-to-list 'zw/repl-run-function '(python-ts-mode . zw/python-run-repl))

;; * R
(use-package ess
  :commands R
  :hook
  (ess-mode . zw/ess-setup)
  (inferior-ess-mode . zw/inferior-ess-setup)
  (inferior-ess-mode . zw/right-side-window-mode)
  (ess-rdired-mode . zw/right-side-window-mode)
  (ess-jags-mode . zw/ess-indent)
  :bind ((:map ess-r-mode-map
               ("TAB" . zw/smart-tab)
               ("C-c c e" . ess-complete-object-name)
               ("C-c C-c" . zw/ess-send-region-or-block)
               ("C-c C-a" . zw/ess-send-above)
               ("s-t" . ess-bp-set)
               ("s-," . ess-bp-set-conditional)
               ("s-." . ess-bp-set-logger)))
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
    (setq-local zw/modeline--process
                '(:eval (nth ess--busy-count ess-busy-strings))))
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
  (defun zw/ess-send-above ()
    (interactive)
    (ess-eval-region (point-min) (point) 'nowait))
  ;; fix freezing in macos by creating your process using pipe
  ;; https://emacs.stackexchange.com/questions/40603/process-input-seems-buggy-in-emacs-on-os-x
  ;; (setq process-connection-type nil)
  (setq ess-style 'RStudio-
        ess-nuke-trailing-whitespace-p t
        ess-ask-for-ess-directory nil
        ess-use-company nil
        ess-use-flymake t
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
  :vc (:url "https://github.com/zhenhua-wang/ess-smart-assign")
  :bind ((:map ess-r-mode-map
               ("=" . ess-smart-assign))
         (:map inferior-ess-r-mode-map
               ("=" . ess-smart-assign))))

;; register run repl
(defun zw/R-run-repl ()
  (interactive)
  (require 'ess)
  (zw/repl-run-in-path-macro 'inferior-ess-r-program 'run-ess-r))
(add-to-list 'zw/repl-run-function '(ess-r-mode . zw/R-run-repl))

;; * CSV
(use-package csv-mode
  :hook ((csv-mode . zw/csv-init))
  :config
  (defun zw/csv-init ()
    (csv-guess-set-separator)
    (zw/visual-line-disable)))

;; * Provide
(provide 'zw-lang)
