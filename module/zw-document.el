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
               ("M-s b" . org-cite-insert)))
  :hook
  ;; (org-mode . org-num-mode)
  ((org-babel-after-execute org-mode) . org-redisplay-inline-images)
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

;; auto tangle
(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

;; Table of contents
(use-package toc-org
  :hook ((org-mode . toc-org-mode)
         (markdown-mode . toc-org-mode)))

;; ** ox latex
;; htmlize.el is needed for exporting colorful codes to html
(with-eval-after-load "ox-latex"
  (pcase system-type
    ((or 'gnu/linux 'windows-nt 'cygwin)
     (setq org-format-latex-options (plist-put org-format-latex-options :scale 3.4)))
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
  (setq org-modern-star '("◉" "●" "○" "◈" "◇" "✳")
        org-modern-hide-stars 'leading
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
                         ("q" . org-agenda-exit))))
  ;; agenda settings
  (setq org-agenda-files '("~/Documents/Agenda/Work.org"))
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
          "" "┈┈┈┈┈┈┈┈┈┈┈┈┈")
        org-agenda-current-time-string
        "ᐊ┈┈┈┈┈┈┈ Now")
  ;; custom agenda
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "")
            (alltodo "" ((org-agenda-overriding-header "\n⚡ To Do")
                         (org-agenda-sorting-strategy '(priority-down))
                         (org-agenda-todo-keyword-format ""))))))))

;; agenda keys
(defun zw/git-add-commit-push-agenda ()
  (interactive)
  (if (file-directory-p "~/Documents/Agenda")
      (progn
        (shell-command "cd ~/Documents/Agenda && git add *")
        (shell-command "cd ~/Documents/Agenda && git commit -m 'Updated all files.'")
        (shell-command "cd ~/Documents/Agenda && git push")
        (message "Agenda pushed!"))
    (message "Agenda doesn't exit!")))
(defun zw/git-pull-agenda ()
  (interactive)
  (if (file-directory-p "~/Documents/Agenda")
      (progn
        (shell-command "cd ~/Documents/Agenda && git pull")
        (message "Agenda pulled!"))
    (message "Agenda doesn't exit!")))
(defun zw/open-agenda ()
  (interactive)
  (if (file-directory-p "~/Documents/Agenda")
      (org-agenda nil "d")
    (message "Agenda doesn't exit!")))
(bind-keys :prefix-map zw/org-agenda-map
           :prefix "<f12>"
           ("<down>" . zw/git-pull-agenda)
           ("<up>" . zw/git-add-commit-push-agenda)
           ("<f12>" . zw/open-agenda))

;; * Markdown
(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
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
        TeX-source-correlate-start-server t
        TeX-view-program-selection '((output-pdf "PDF Tools")))
  :config
  ;; revert the PDF-buffer after the TeX compilation has finished
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package auctex-latexmk
  :after tex
  :init
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

;; * Spell checker
(use-package flyspell
  :straight (:type built-in)
  :diminish
  :hook (((text-mode outline-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :init (setq flyspell-issue-message-flag nil
              flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face)
              ispell-program-name "aspell"
              ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

(use-package flyspell-correct
  :after flyspell
  :bind ((:map flyspell-mode-map ("M-$" . flyspell-correct-at-point))))

;; * Reader
(use-package pdf-tools
  :bind ((:map pdf-view-mode-map
               ("C-s" . isearch-forward)
               ("s-f" . isearch-forward)
               ("s-=" . pdf-view-enlarge)
               ("s-+" . pdf-view-enlarge)
               ("s--" . pdf-view-shrink)
               ("q" . kill-current-buffer)
               ("j" . pdf-view-next-line-or-next-page)
               ("k" . pdf-view-previous-line-or-previous-page)))
  :init
  (setq pdf-view-display-size 'fit-page
        pdf-view-use-imagemagick nil
        pdf-view-continuous nil
        pdf-view-use-scaling t
        pdf-annot-activate-created-annotations t)
  (pdf-loader-install))

;; * Provide
(provide 'zw-document)
