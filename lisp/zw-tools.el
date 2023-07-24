(require 'map)
(require 'proced)
(require 'seq)

(defun zw/quit-window-kill-bufer ()
  "Quit window then kill buffer."
  (interactive)
  (quit-window 'kill))

(defun zw/kill-bufer-quit-window ()
  "Kill buffer then quit window."
  (interactive)
  (if (one-window-p)
      (kill-buffer)
    (kill-buffer-and-window)))

(defun zw/maximize-window ()
  "maximize window (also works for side windows)."
  (interactive)
  (let ((current-buffer-name (buffer-name (current-buffer))))
    (if (window-parameter (get-buffer-window) 'window-side)
        (progn (select-window (window-main-window))
               (delete-other-windows)
               (switch-to-buffer current-buffer-name))
      (delete-other-windows))))

(defun zw/update-emacs-tangle-dotfiles ()
  "update zw/emacs and tangle dotfiles"
  (interactive)
  (shell-command "cd ~/.emacs.d && git pull")
  (org-babel-tangle-file "~/.emacs.d/OrgFiles/dotfiles.org")
  (message "Emacs updated & dotfiles tangled!"))

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

;; set preference to horizontal split
(defun zw/split-window-sensibly-prefer-horizontal (&optional window)
  "Based on split-window-sensibly, but designed to prefer a horizontal split,
i.e. windows tiled side-by-side."
  (interactive)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically
             (with-selected-window window
               (split-window-below)))
        (and
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
         (not (window-minibuffer-p window))
         (let ((split-width-threshold 0))
           (when (window-splittable-p window t)
             (with-selected-window window
               (split-window-right)))))))
  ;; switch to scratch buffer after creating new window
  (other-window 1 nil)
  (switch-to-buffer "*scratch*"))

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

(defvar-local zw/presentation-on nil)
(defun zw/toggle-presentation ()
  "Toggle presentation"
  (interactive)
  (if zw/presentation-on
      (progn
        (setq-local mode-line-format (default-value 'mode-line-format))
        (setq-local zw/presentation-on nil))
    (progn
      (setq-local mode-line-format nil)
      (setq-local zw/presentation-on t)))
  (when (eq major-mode 'pdf-view-mode)
    (pdf-view-fit-page-to-window))
  (force-mode-line-update)
  (redraw-display))

(defun zw/smart-tab ()
  "Tab indent or toggle hide show or toggle outline"
  (interactive)
  (cond
   ((and outline-minor-mode (outline-on-heading-p)) (outline-toggle-children))
   ((and hs-minor-mode (hs-already-hidden-p)) (zw/toggle-fold))
   (t (indent-for-tab-command))))

(defun zw/dired-on-left ()
  "Display project root in side window on left, hiding details."
  (interactive)
  (let ((buffer (dired-noselect (or (vc-root-dir)
                                    (ignore-errors (file-name-directory (buffer-file-name)))
                                    default-directory))))
    (with-current-buffer buffer (dired-hide-details-mode t))
    (display-buffer-in-side-window
     buffer `((side . left) (slot . 0)
              (window-width . 0.2)
              (preserve-size . (t . nil))
              (window-parameters . ((no-delete-other-windows . t)
                                    (dedicated . t)))))
    (select-window (get-buffer-window buffer))))

(provide 'zw-tools)
