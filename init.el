;; load init
(org-babel-load-file "~/.emacs.d/emacs.org")
;; (org-babel-load-file "~/.emacs.d/emacs-plain-config.org")

(setq initial-scratch-message nil)
;; print starting time to scratch
;; (defun greet-time ()
;;   (let ((current-hour (string-to-number (format-time-string "%H" (current-time)))))
;;     (cond
;;      ((or (>= current-hour 20) (< current-hour 6))
;;       "Good evening")
;;      ((and (>= current-hour 6)
;; 	   (< current-hour 12))
;;       "Good morning")
;;      (t "Good afternoon"))))

;;  scratch mode
;; (setq initial-major-mode 'org-mode)

;; Profile emacs startup
;; (let ((startup-time (format "%.2f seconds"
;; 			   (float-time
;; 			    (time-subtract after-init-time before-init-time)))))
;;      (setq initial-scratch-message (format "\n+ %s, %s! The init completed in =%s= with =%d= garbage collections.\n\n"
;; 				           (greet-time)
;; 				           user-login-name
;; 				           startup-time
;; 				           gcs-done)))

