;; start: R auto complete
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "M-,") 'company-select-next)
(define-key company-active-map (kbd "M-k") 'company-select-previous)
(define-key company-active-map [return] nil)
(define-key company-active-map [tab] 'company-complete-common)
(define-key company-active-map (kbd "TAB") 'company-complete-common)
(define-key company-active-map (kbd "M-TAB") 'company-complete-selection)
(setq company-selection-wrap-around t
      company-tooltip-align-annotations t
      company-idle-delay 0.36
      company-minimum-prefix-length 2
      company-tooltip-limit 10)
;; end.

;; start: markdown mode
(setq markdown-enable-math t)
;; end

;; start: ein jupyter
(setq ein:polymode t)
(setq ein:completion-backend '(ein:use-company-backend))
;;end

;; start: org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(setq org-src-tab-acts-natively t)
(setq org-src-fontify-natively t)

(defun org-insert-code-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("python" "R" "emacs-lisp" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s :results graphics file :exports both\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)))
;;    (org-edit-src-code)))

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
(add-hook 'org-mode-hook '(lambda ()
                            ;; keybinding for inserting code blocks
                            (local-set-key (kbd "C-S-i")'org-insert-code-block)))

;;end
