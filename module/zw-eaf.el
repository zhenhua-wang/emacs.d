;; eaf
(use-package eaf
  :straight '(eaf :host github :repo "emacs-eaf/emacs-application-framework"
                  :files ("*"))
  :demand
  :bind (("s-o" . zw/eaf-open-this-buffer))
  :config
  (setq zw/enable-eaf-browser-p nil
        zw/enable-eaf-pdf-p t)
  ;; browser
  (when zw/enable-eaf-browser-p
    (require 'eaf-browser)
    (setq browse-url-browser-function 'eaf-open-browser)
    (eaf-setq eaf-webengine-default-zoom "2")
    (eaf-setq eaf-browser-enable-adblocker t)
    (eaf-setq eaf-browser-dark-mode nil)
    (eaf-bind-key scroll_to_begin "M-<" eaf-browser-keybinding)
    (eaf-bind-key scroll_to_bottom "M->" eaf-browser-keybinding)
    (eaf-bind-key insert_or_scroll_up_page "n" eaf-browser-keybinding)
    (eaf-bind-key insert_or_scroll_down_page "p" eaf-browser-keybinding)
    (eaf-bind-key kill_text "s-x" eaf-browser-keybinding)
    (eaf-bind-key copy_text "s-c" eaf-browser-keybinding)
    (eaf-bind-key yank_text "s-v" eaf-browser-keybinding)
    (eaf-bind-key undo_action "s-z" eaf-browser-keybinding)
    (eaf-bind-key redo_action "s-Z" eaf-browser-keybinding)
    (eaf-bind-key select_all_or_input_text "s-a" eaf-browser-keybinding)
    (eaf-bind-key search_text_forward "s-f" eaf-browser-keybinding))
  ;; pdf
  (when zw/enable-eaf-pdf-p
    (require 'eaf-pdf-viewer)
    (add-to-list 'eaf-find-file-ext-blacklist "pdf")
    (eaf-setq eaf-pdf-default-zoom  2)
    (eaf-setq eaf-pdf-dark-mode "ignore")
    (eaf-bind-key scroll_to_begin "M-<" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_to_end "M->" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_up_page "n" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_down_page "p" eaf-pdf-viewer-keybinding)
    (eaf-bind-key copy_select "s-c" eaf-pdf-viewer-keybinding)
    (eaf-bind-key search_text_forward "s-f" eaf-pdf-viewer-keybinding)))

(defun zw/eaf-open-this-buffer ()
  "Try to open the current buffer using EAF and close old buffer"
  (interactive)
  (if (eaf--buffer-file-p)
      (let ((current-buffer-file-name buffer-file-name))
        (kill-buffer (current-buffer))
        (eaf-open current-buffer-file-name))
    (user-error "[EAF] Current buffer is not supported by EAF!")))

(provide 'zw-eaf)