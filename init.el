;; load init
(org-babel-load-file "~/.emacs.d/emacs.org")

;; print starting time to scratch
(defun greet-time ()
  (let ((current-hour (string-to-number (format-time-string "%H" (current-time)))))
    (cond
     ((or (>= current-hour 20) (< current-hour 6))
      (message "Good evening"))
     ((and (>= current-hour 6)
	   (< current-hour 12))
      (message "Good morning"))
     (t (message "Good afternoon")))))
  
;;  scratch mode
(setq initial-major-mode 'org-mode)

;; Profile emacs startup
(setq startup-time (format "%.2f seconds"
			   (float-time
			    (time-subtract after-init-time before-init-time))))

(setq initial-scratch-message (format "\n+ %s, %s! The init completed in =%s= with =%d= garbage collections.\n\n"
				      (greet-time)
				      user-login-name
				      startup-time
				      gcs-done))
;; (setq initial-buffer-choice "~/Workspace/Documents/OrgFiles/Tasks.org")
;; (add-hook 'after-init-hook 'org-agenda-list)

;; Silence compiler warnings as they can be pretty disruptive
;; (if (eq system-type 'gnu/linux)
;;     (setq native-comp-async-report-warnings-errors nil))
