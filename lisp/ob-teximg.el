;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:teximg '())

(defun org-babel-execute:teximg (body params)
  "Execute a block of teximg code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing teximg source code block")
  (let* ((processed-params (org-babel-process-params params))
         (tmp-dir "textmp/")
         (fname (cdr (assq :fname params)))
         (save-loc (concat tmp-dir fname))
         (options (org-combine-plists
                   org-format-latex-options
                   `(:html-scale ,1.5 :html-foreground ,"Black" :html-background ,"White"))))
    ;; creat ./tmp dir
    (when (not (file-exists-p tmp-dir))
      (make-directory tmp-dir t))
    ;; generate teximg
    (message save-loc)
    (org-create-formula-image body save-loc options nil)
    (print save-loc)))

;; define teximg as alias to latex
(define-derived-mode teximg-mode latex-mode "teximg"
  "A major mode for latex.")

(provide 'ob-teximg)
;;; ob-teximg.el ends here
