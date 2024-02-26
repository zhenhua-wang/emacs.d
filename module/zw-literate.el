;; -*- lexical-binding: t -*-

;; * Org babel
;; ** main
(with-eval-after-load "ob"
  ;; init org-babel
  (setq org-src-window-setup 'split-window-below
        org-src-preserve-indentation t      ; helps to indent python code in org mode
        org-edit-src-content-indentation 2
        org-confirm-babel-evaluate nil
        org-src-tab-acts-natively t)
  ;; declare babel safe expression
  (zw/merge-list-symbols
   'safe-local-eval-forms
   '((defun zw/org-babel-tangle-linux (path)
       (if (eq system-type 'gnu/linux) path "no"))
     (defun zw/org-babel-tangle-not-exist (path)
       (if (file-exists-p path) "no" path)))))


;; ** lazy load
(with-eval-after-load "ob"
  (defvar +org-babel-mode-alist
    '((cpp . C)
      (C++ . C)
      (sh . shell)
      (bash . shell)
      (ess-r . R))
    "An alist mapping languages to babel libraries. This is necessary for babel
libraries (ob-*.el) that don't match the name of the language.
For example, with (fish . shell) will cause #+BEGIN_SRC fish to load ob-shell.el
when executed.")

  (defvar +org-babel-load-functions ()
    "A list of functions executed to load the current executing src block. They
take one argument (the language specified in the src block, as a string). Stops
at the first function to return non-nil.")

  (defun +org--babel-lazy-load (lang &optional async)
    (cl-check-type lang (or symbol null))
    (unless (cdr (assq lang org-babel-load-languages))
      (when async
        ;; ob-async has its own agenda for lazy loading packages (in the child
        ;; process), so we only need to make sure it's loaded.
        (require 'ob-async nil t))
      (prog1 (or (run-hook-with-args-until-success '+org-babel-load-functions lang)
                 (require (intern (format "ob-%s" lang)) nil t)
                 (require lang nil t))
        (add-to-list 'org-babel-load-languages (cons lang t)))))

  (defun +org--src-lazy-load-library-a (lang)
    "Lazy load a babel package to ensure syntax highlighting."
    (or (cdr (assoc lang org-src-lang-modes))
        (+org--babel-lazy-load lang)))
  (defun +org--babel-lazy-load-library-a (info)
    "Load babel libraries lazily when babel blocks are executed."
    (let* ((lang (nth 0 info))
           (lang (cond ((symbolp lang) lang)
                       ((stringp lang) (intern lang))))
           (lang (or (cdr (assq lang +org-babel-mode-alist))
                     lang)))
      (+org--babel-lazy-load
       lang (and (not (assq :sync (nth 2 info)))
                 (assq :async (nth 2 info))))
      t))
  (defun +org--export-lazy-load-library-h (&optional element)
    "Lazy load a babel package when a block is executed during exporting."
    (+org--babel-lazy-load-library-a (org-babel-get-src-block-info nil element)))

  ;; lazy load advice
  (advice-add #'org-src--get-lang-mode :before #'+org--src-lazy-load-library-a)
  (advice-add #'org-babel-confirm-evaluate :after-while #'+org--babel-lazy-load-library-a)
  (advice-add #'org-babel-exp-src-block :before #'+org--export-lazy-load-library-h)
  (advice-add #'org-babel-do-load-languages :override #'ignore)

  ;; language alias
  (defalias 'org-babel-execute:ess-r 'org-babel-execute:R)
  (defalias 'org-babel-ess-r-initiate-session 'org-babel-R-initiate-session))

;; * Code cells
;; This requires the python library: jupytext.
(use-package code-cells
  :hook (python-mode . code-cells-mode-maybe)
  :bind ((:map code-cells-mode-map
               ("C-c C-e" . zw/jupyter-export-to)
               ("M-p" . code-cells-backward-cell)
               ("M-n" . code-cells-forward-cell)))
  :config
  (defun zw/jupyter-export-to (file-type)
    (interactive
     (list (completing-read "jupyter export to: "
                            '("pdf" "html") nil t)))
    (when (string= (file-name-extension buffer-file-name) "ipynb")
      (async-shell-command
       (concat "jupyter nbconvert --execute --to " file-type " "
               (shell-quote-argument buffer-file-name))))))

;; * Polymode
;; BUG: revert in inner buffer would lose font-lock
(use-package polymode
  :commands polymode-mode
  :hook ((polymode-init-host . zw/polymode-init-host)
         (polymode-init-inner . zw/polymode-init-inner))
  :bind ((:map polymode-mode-map
               ("C-c C-e" . polymode-export)
               ("C-c C-b" . polymode-eval-buffer)
               ("M-p" . polymode-previous-chunk)
               ("M-n" . polymode-next-chunk)))
  :init
  (setq poly-lock-allow-fontification t
        poly-lock-allow-background-adjustment t
        ;; disable this for now because of reverse-typing issue in poly-R
        polymode-lsp-integration nil)
  :config
  (defun zw/polymode-init-host ()
    (font-lock-update))
  (defun zw/polymode-init-inner ()
    (buffer-face-mode -1)
    (display-line-numbers-mode -1)
    (zw-outline-mode -1)
    (setq-local lsp-diagnostics-provider :none))
  ;; run kill-buffer in host buffer, which solves the font lock issue
  (pm-around-advice #'kill-buffer #'polymode-with-current-base-buffer)
  ;; lsp integration
  (pm-around-advice 'lsp--buffer-content #'polymode-lsp-buffer-content))

(use-package poly-rmarkdown
  :straight '(poly-rmarkdown :host github :repo "zhenhua-wang/poly-rmarkdown")
  :commands (poly-rmarkdown-mode))

;; * Provide
(provide 'zw-literate)
