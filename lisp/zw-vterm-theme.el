;; -*- lexical-binding: t -*-

;;; code

(defun zw/set-vterm-color ()
  (set-face-attribute 'vterm-color-black nil :foreground "#3B4252" :background "#3B4252")
  (set-face-attribute 'vterm-color-red nil :foreground "#B25068" :background "#B25068")
  (set-face-attribute 'vterm-color-green nil :foreground "#2EB086" :background "#2EB086")
  (set-face-attribute 'vterm-color-yellow nil :foreground "#F4E06D" :background "#F4E06D")
  (set-face-attribute 'vterm-color-blue nil :foreground "#47B5FF" :background "#47B5FF")
  (set-face-attribute 'vterm-color-magenta nil :foreground "#FF869E" :background "#FF869E")
  (set-face-attribute 'vterm-color-cyan nil :foreground "#4CACBC" :background "#4CACBC")
  (set-face-attribute 'vterm-color-white nil :foreground "#E5E9F0" :background "#E5E9F0"))

(provide 'zw-vterm-theme)
;;; zw-vterm-theme.el ends here
