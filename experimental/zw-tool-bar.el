;; -*- lexical-binding: t -*-

;; global
(push '(tool-bar-lines . 1)   initial-frame-alist)
(push '(tool-bar-position . left)   initial-frame-alist)
(add-to-list 'image-load-path "~/.emacs.d/resources/images/icons/")
(setq tool-bar-style 'image)
(setq tool-bar-map (make-sparse-keymap))
(setq tool-bar-separator-image-expression
      (tool-bar--image-expression "separator"))

;; functions
(defun zw/tool-bar-add-item (command image help-echo)
  "Add button (24px) on tab-bar."
  (define-key-after (default-value 'tool-bar-map) (vector command)
    `(menu-item "Open user configuration file" ,command
                :enable t
                :help ,help-echo
                :image ,image)))

(defun zw/tool-bar-home ()
  "Switch to scratch buffer."
  (interactive)
  (let ((home-buff-name "*scratch*"))
    (if (string= (buffer-name) home-buff-name)
        (switch-to-buffer nil)
      (switch-to-buffer home-buff-name))))

;; items
(tool-bar-add-item "home" 'zw/tool-bar-home 'zw/tool-bar-home :label "Emacs")
(define-key-after (default-value 'tool-bar-map) [separator-1] menu-bar-separator)
(tool-bar-add-item-from-menu 'find-file "new" nil :label "New File")
(tool-bar-add-item-from-menu 'menu-find-file-existing "open" nil :label "Open")
(tool-bar-add-item-from-menu 'save-buffer "save" nil :label "Save")
(define-key-after (default-value 'tool-bar-map) [separator-2] menu-bar-separator)
(tool-bar-add-item "back-arrow" 'undo-only 'undo-only :label "Undo")
(tool-bar-add-item "fwd-arrow" 'undo-redo 'undo-redo :label "Redo")
(define-key-after (default-value 'tool-bar-map) [separator-3] menu-bar-separator)
(zw/tool-bar-add-item 'zw/open-user-config
                      (find-image '((:type png :file "setting.png" :scale 0.5)))
                      "Open user configuration file")

(provide 'zw-tool-bar)
