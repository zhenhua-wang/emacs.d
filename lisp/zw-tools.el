(require 'map)
(require 'proced)
(require 'seq)

(defun zw/get-face-attr-recur (face attr)
  "helper functions to get face background/foreground recursively"
  (let ((face-attr (face-attribute face attr)))
    (if (and face-attr
             (not (eq face-attr 'unspecified)))
        face-attr
      (let ((parent-face (face-attribute face :inherit)))
        (if (and parent-face
                 (not (eq parent-face 'unspecified)))
            (zw/get-face-attr-recur parent-face attr)
          nil)))))

(defun zw/get-face-bg-recur (face)
  "get face background recursively"
  (zw/get-face-attr-recur face :background))

(defun zw/get-face-fg-recur (face)
  "get face foreground recursively"
  (zw/get-face-attr-recur face :foreground))

(defun zw/close-shell ()
  "close window when close shell buffer including REPLs"
  (interactive)
  (if (one-window-p)
      (kill-buffer)
    (kill-buffer-and-window)))

(defun zw/delte-window-or-bury-buffer ()
  "delete or bury"
  (interactive)
  (if (one-window-p)
      (bury-buffer)
    (delete-window)))

(defun zw/update-emacs ()
  "update zw/emacs"
  (interactive)
  (shell-command "cd ~/.emacs.d && git pull")
  (message "Emacs updated!"))

(defun zw/show-info ()
  "show buffer info"
  (interactive)
  (message (if buffer-file-name
               (concat "File: "
                       (buffer-file-name)
                       ", Encoding:"
                       (zw/modeline-encoding))
             (concat "Buffer: "
                     (buffer-name)
                     ", Encoding:"
                     (zw/modeline-encoding)))))

(defun zw/tangle-dotfiles ()
  "tangle dotfiles"
  (interactive)
  (org-babel-tangle-file "~/.emacs.d/OrgFiles/dotfiles.org"))

;; https://xenodium.com/emacs-quick-kill-process/
(defun zw/quick-kill-process ()
  "quick-kill-process"
  (interactive)
  (let* ((pid-width 5)
         (comm-width 25)
         (user-width 10)
         (processes (proced-process-attributes))
         (candidates
          (mapcar (lambda (attributes)
                    (let* ((process (cdr attributes))
                           (pid (format (format "%%%ds" pid-width) (map-elt process 'pid)))
                           (user (format (format "%%-%ds" user-width)
                                         (truncate-string-to-width
                                          (map-elt process 'user) user-width nil nil t)))
                           (comm (format (format "%%-%ds" comm-width)
                                         (truncate-string-to-width
                                          (map-elt process 'comm) comm-width nil nil t)))
                           (args-width (- (window-width) (+ pid-width user-width comm-width 3)))
                           (args (map-elt process 'args)))
                      (cons (if args
                                (format "%s %s %s %s" pid user comm (truncate-string-to-width args args-width nil nil t))
                              (format "%s %s %s" pid user comm))
                            process)))
                  processes))
         (selection (map-elt candidates
                             (completing-read "kill process: "
                                              (seq-sort
                                               (lambda (p1 p2)
                                                 (string-lessp (nth 2 (split-string (string-trim (car p1))))
                                                               (nth 2 (split-string (string-trim (car p2))))))
                                               candidates) nil t)))
         (prompt-title (format "%s %s %s"
                               (map-elt selection 'pid)
                               (map-elt selection 'user)
                               (map-elt selection 'comm))))
    (when (y-or-n-p (format "Kill? %s" prompt-title))
      (if (eq (signal-process (map-elt selection 'pid) 9) 0)
          (message "killed: %s" prompt-title)
        (message "error: could not kill %s" prompt-title)))))

(provide 'zw-tools)
