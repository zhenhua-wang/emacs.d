;; -*- lexical-binding: t -*-

;;; Code:

;; TODO: helper functions to get face background/foreground recursively
(defun zw/get-face-attr-recur (face attr)
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
  (zw/get-face-attr-recur face :background))

(defun zw/get-face-fg-recur (face)
  (zw/get-face-attr-recur face :foreground))

(provide 'zw-utils)
;;; zw-utils.el ends here
