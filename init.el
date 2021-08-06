;; load init
(org-babel-load-file "~/.emacs.d/emacs.org")

;;  scratch mode
(setq initial-major-mode 'org-mode)

;; print starting time to scratch
(defun greet-time ()
  (setq current-hour (string-to-number (format-time-string "%H" (current-time))))
  (if (or (>= current-hour 20)
	  (< current-hour 6))
      (message "Good evening")
    (if (and (>= current-hour 6)
	     (< current-hour 12))
	(message "Good morning")
      (message "Good afternoon"))))
(greet-time)

;; Profile emacs startup
(setq startup-time (format "%.2f seconds"
			   (float-time
			    (time-subtract after-init-time before-init-time))))

(setq initial-scratch-message (format "%s, %s! The init completed in %s with %d garbage collections.\n\n"
				      (greet-time)
				      user-login-name
				      startup-time
				      gcs-done))
;(setq initial-buffer-choice "~/Workspace/test/test.org")
