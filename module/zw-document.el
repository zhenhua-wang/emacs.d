;; -*- lexical-binding: t -*-

;; * Visual-fill-column
(use-package visual-fill-column
  :hook
  ((org-mode markdown-mode) . visual-fill-column-mode)
  ((helpful-mode ess-r-help-mode) . visual-fill-column-mode)
  :init
  (setq-default visual-fill-column-center-text t
                visual-fill-column-width 95))

;; * Org mode
;; ** main
(use-package org
  :straight (:type built-in)
  :mode (("\\.org$" . org-mode))
  :bind ((:map org-mode-map
               ("C-c =" . nil)
               ("C-," . nil)
               ("M-p" . org-previous-block)
               ("M-n" . org-next-block)
               ("M-s c" . org-cite-insert)))
  ;; :hook
  ;; (org-mode . org-num-mode)
  ;; ((org-babel-after-execute org-mode) . org-redisplay-inline-images)
  :config
  (setq org-num-face 'default
        org-ellipsis " ⇲"                    ; ▼, ↴, ⬎, ⤷, ⋱, ⤵, ⇲
        org-image-actual-width nil
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-support-shift-select 'always)
  ;; custom functions
  (defun zw/org-fold-all-but-current ()
    (interactive)
    (org-remove-occur-highlights)
    (org-overview)
    (org-reveal)))

;; org contrib
(use-package org-contrib
  :straight (:host github :repo "emacsmirror/org-contrib")
  :after org
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

;; auto tangle
(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

;; Table of contents
(use-package toc-org
  :hook ((org-mode . toc-org-mode)
         (markdown-mode . toc-org-mode))
  :config (setq toc-org-max-depth 1))

;; ** ox latex
;; htmlize.el is needed for exporting colorful codes to html
(with-eval-after-load "ox-latex"
  (require 'ox-beamer)
  (pcase system-type
    ((or 'gnu/linux 'windows-nt 'cygwin)
     (setq org-format-latex-options (plist-put org-format-latex-options :scale 1)))
    ('darwin
     (progn
       (setq org-format-latex-options (plist-put org-format-latex-options :scale 2))
       (setq org-latex-create-formula-image-program 'dvisvgm))))
  ;; latex '(latex script entities)
  (setq org-highlight-latex-and-related '(latex entities))
  ;; org-export
  (setq org-latex-listings 't)
  ;; language alias
  (add-to-list 'org-latex-listings-langs '(ess-r "R"))
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
                  [NO-DEFAULT-PACKAGES]
                  [PACKAGES]
                  [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; ** theme
(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :init
  (setq org-modern-star 'fold
        org-modern-fold-stars '(("" . ""))
        org-modern-block-fringe nil
        org-modern-table t
        org-modern-table-vertical 3
        org-modern-table-horizontal 1
        org-modern-todo t
        org-modern-todo-faces
        '(("TODO" . (:inherit fixed-pitch :foreground "white" :background "#B25068" :weight bold))
          ("DONE" . (:inherit fixed-pitch :foreground "black" :background "#6CC4A1" :weight bold)))))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autokeywords t
        org-appear-inside-latex t))

;; ** agenda
(with-eval-after-load "org-agenda"
  (add-hook 'org-agenda-mode-hook 'visual-fill-column-mode)
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (bind-keys :map org-agenda-mode-map
                         ("s-q" . org-agenda-exit)
                         ("q" . org-agenda-exit)
                         ("<f9>" . org-agenda-exit))))
  ;; default agenda
  (setq org-log-done 'time
        org-agenda-window-setup 'only-window
        org-agenda-restore-windows-after-quit t
        org-agenda-overriding-header "🗓️ Agenda"
        org-agenda-block-separator ?-
        org-agenda-format-date "%a. %b %d %Y"
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "◀── now ─────────────────────────────────────────────────")
  ;; custom agenda
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "")
            (alltodo "" ((org-agenda-overriding-header "\n⚡ To Do")
                         (org-agenda-sorting-strategy '(priority-down))
                         (org-agenda-todo-keyword-format ""))))))))

;; agenda settings
(setq zw/org-agenda-directory "~/Documents/Agenda/"
      zw/org-agenda-files '("Work.org")
      org-agenda-files (cl-mapcar (lambda (file) (expand-file-name file zw/org-agenda-directory))
                                  zw/org-agenda-files))
;; agenda keys
(defun zw/open-agenda-dashboard ()
  (interactive)
  (when (zw/window-side (selected-window))
    (select-window (zw/window-first-non-side)))
  (when (and (not (file-directory-p zw/org-agenda-directory))
             (y-or-n-p (format "%s doesn't exist! Initialize? " zw/org-agenda-directory)))
    (make-directory zw/org-agenda-directory)
    (make-empty-file (car org-agenda-files)))
  (org-agenda nil "d"))
(defun zw/open-agenda-file ()
  (interactive)
  (let* ((vertico-preselect 'first)
         (file (read-file-name "View Agenda file: "
                               zw/org-agenda-directory
                               nil t)))
    (find-file file)))
(bind-keys :map global-map
           ("<f9>" . zw/open-agenda-dashboard)
           ("S-<f9>" . zw/open-agenda-file))

;; ** babel
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

;; ** babel lazy load
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

;; * Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode ("\\.rmd\\'" . gfm-mode)
  :init
  (setq markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-uppercase-checkbox t
        markdown-fontify-code-blocks-natively t
        markdown-code-block-braces t
        markdown-regex-header-setext nil
        markdown-header-scaling t
        markdown-asymmetric-header t)
  (defun zw/markdown-toggle-markup-hiding ()
    (interactive)
    (markdown-toggle-markup-hiding)
    (remove-from-invisibility-spec 'markdown-markup))
  :config
  (add-to-list 'markdown-code-lang-modes '("r" . ess-r-mode)))

;; * Polymode
;; BUG: revert in inner buffer would lose font-lock
(use-package polymode
  :commands polymode-mode
  :hook ((polymode-init-host . zw/polymode-host-init)
         (polymode-init-inner . zw/polymode-inner-init))
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
  (defun zw/polymode-host-init ()
    (font-lock-update))
  (defun zw/polymode-inner-init ()
    (buffer-face-mode -1)
    (display-line-numbers-mode -1)
    (zw-outline-mode -1))
  ;; run kill-buffer in host buffer, which solves the font lock issue
  (pm-around-advice #'kill-buffer #'polymode-with-current-base-buffer))

(use-package poly-rmarkdown
  :straight '(poly-rmarkdown :host github :repo "zhenhua-wang/poly-rmarkdown")
  :commands (poly-rmarkdown-mode))

;; * Code cell
;; This requires the python library: jupytext.
(use-package code-cells
  :hook (python-mode . code-cells-mode-maybe)
  :bind ((:map code-cells-mode-map
               ("C-c C-c" . zw/python-shell-send-region-or-block)
               ("C-c C-b" . zw/python-shell-send-buffer)
               ("C-<return>" . zw/python-shell-send-line)
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

;; * Latex
(use-package tex
  :straight auctex
  :commands (latex-mode LaTeX-mode)
  :init
  (setq TeX-PDF-mode t
        TeX-parse-self t
        TeX-auto-save t
        TeX-insert-braces nil
        TeX-auto-untabify t
        TeX-save-query nil
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server t)
  :config
  ;; revert the PDF-buffer after the TeX compilation has finished
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package auctex-latexmk
  :after tex
  :hook ((latex-mode LaTeX-mode) .
         (lambda ()
           ;; Set LatexMk as the default.
           (setq TeX-command-default "LatexMk")))
  :config
  ;; Pass the -pdf flag when TeX-PDF-mode is active.
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  ;; Add LatexMk as a TeX target.
  (auctex-latexmk-setup))

(use-package reftex
  :hook (LaTeX-mode . reftex-mode)
  :bind ((:map reftex-mode-map
               ("s-s" . zw/latex-rescan-on-save)
               ("C-x C-s" . zw/latex-rescan-on-save)))
  :config
  (defun zw/latex-rescan-on-save ()
    (interactive)
    (reftex-reset-mode)
    (save-buffer))
  (setq reftex-plug-into-AUCTeX t
        reftex-toc-split-windows-fraction 0.2
        reftex-toc-split-windows-horizontally t))

;; * Provide
(provide 'zw-document)
