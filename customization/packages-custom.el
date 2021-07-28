;;;;;;;;;;;;;;;;;;;;;;;;;;;   custom functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keys for create a python workspace
;; (add-hook 'org-mode-hook '(lambda ()
;; 			    (flyspell-mode 1)
			    
;; 			    (defun split-python-window ()
;; 			      (interactive)
;; 			      (delete-other-windows)
;; 			      (split-window-right)
;; 			      (windmove-right)
;; 			      (org-edit-src-code)
;; 			      (split-window-below)
;; 			      (windmove-down)
;; 			      (run-python)
;; 			      (switch-to-buffer "*Python*")
;; 			      (windmove-left))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; interpreters;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setq python-shell-interpreter "ipython3"
;;      python-shell-interpreter-args "--simple-prompt --pprint") ;; prevent emacs from freezing
;;(setq org-babel-python-command "python3")
;;(setq ob-ipython-command "ipython3")
;;end
