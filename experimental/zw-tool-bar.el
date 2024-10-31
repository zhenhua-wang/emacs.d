;; -*- lexical-binding: t -*-

;; global
(push '(tool-bar-lines . 1)   initial-frame-alist)
(push '(tool-bar-position . left)   initial-frame-alist)
(setq tool-bar-style 'image)
(setq tool-bar-map (make-sparse-keymap))
(setq tool-bar-separator-image-expression
      (tool-bar--image-expression "separator"))

;; items
(tool-bar-add-item-from-menu 'find-file "new" nil :label "New File")
(tool-bar-add-item-from-menu 'menu-find-file-existing "open" nil :label "Open")
(tool-bar-add-item-from-menu 'save-buffer "save" nil :label "Save")
(define-key-after (default-value 'tool-bar-map) [separator-1] menu-bar-separator)
(tool-bar-add-item "back-arrow" 'undo-only 'undo-only :label "Undo")
(tool-bar-add-item "redo" 'undo-redo 'undo-redo :label "Redo")
(define-key-after (default-value 'tool-bar-map) [separator-2] menu-bar-separator)

(provide 'zw-tool-bar)
