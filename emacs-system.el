(defun zw/get-passwd (id)
  (interactive "MEnter your id: ")
  ;; (kill-new (shell-command-to-string (concat "pass " id)))
  (let ((proc (start-process-shell-command "pass" nil (concat "pass " id))))
    ;; async parse process' output
    (set-process-filter proc (lambda (proc line)
                               (progn
                                 (kill-new line)
                                 (message "pwd copied"))))))

;; not working now..
(defun zw/insert-passwd (id)
  (interactive "MEnter your id: ")
  (start-process-shell-command "pass" nil (concat "pass insert " id))
  ;; (call-process-shell-command "pass" nil (concat "pass -c " id))
  (message (concat id " pwd inserted!!"))
)

(provide 'emacs-system)
