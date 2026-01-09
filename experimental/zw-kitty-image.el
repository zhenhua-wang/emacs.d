;; -*- lexical-binding: t -*-

(defvar zw/kitty-image-buffer "*kitty-image-buffer*")

(defun zw/kitty-image--write-tty (data)
  "Write DATA to /dev/tty directly."
  (let ((coding-system-for-write 'binary)
        (write-region-inhibit-fsync t))
    (write-region data nil "/dev/tty" t 0)))

(defun zw/kitty-display-image (file)
  "Test direct image display in Kitty terminal."
  (interactive
   (list (read-file-name "Image: " default-directory nil t)))
  (let* ((buffer (get-buffer-create zw/kitty-image-buffer))
         (data (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally file)
                 (buffer-string)))
         (b64 (base64-encode-string data t))
         (test1 (format "\x1b_Ga=T,f=100,z=-1,t=d;%s\x1b\\" b64)))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (read-only-mode 1)
      (zw/kitty-image--write-tty test1))
    (switch-to-buffer buffer)
    (sit-for 2)))

(defun zw/kitty-clear-image ()
  "Clear all kitty graphics from the terminal."
  (interactive)
  (zw/kitty-image--write-tty "\e_Ga=d\e\\"))

(defun zw/kill-kitty-image-advice ()
  (when (string= (buffer-name) zw/kitty-image-buffer)
    (zw/kitty-clear-image)))

(advice-add 'zw/tab-line-kill-buffer-and-switch :before
            'zw/kill-kitty-image-advice)

(provide 'zw-kitty-image)
