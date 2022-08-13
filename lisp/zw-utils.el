;; -*- lexical-binding: t -*-

;;; Code:

;; TODO: helper functions to get face background/foreground recursively
(defun zw/get-face-bg-recur (face)
  (let ((bg (face-background face)))
    (if bg
        bg
      (zw/get-face-bg-recur (face-attribute face :inherit)))))

(defun zw/get-face-fg-recur (face)
  (let ((fg (face-foreground face)))
    (if fg
        fg
      (zw/get-face-fg-recur (face-attribute face :inherit)))))


(provide 'zw-utils)
;;; zw-utils.el ends here
