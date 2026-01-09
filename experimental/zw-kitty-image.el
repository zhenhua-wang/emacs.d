;; -*- lexical-binding: t -*-

(defun kitty-clear-images ()
  (interactive)
  "Clear all kitty graphics from the terminal."
  (kitty-image--write-tty "\e_Ga=d\e\\"))

(defun kitty-image--write-tty (data)
  "Write DATA to /dev/tty directly."
  (let ((coding-system-for-write 'binary)
        (write-region-inhibit-fsync t))
    (write-region data nil "/dev/tty" t 0)))

(defun kitty-image-display (file)
  "Test direct image display in Kitty terminal."
  (interactive
   (list (read-file-name "Image: " default-directory nil t)))
  (let* ((data (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally file)
                 (buffer-string)))
         (b64 (base64-encode-string data t))
         (test1 (format "\x1b_Ga=T,f=100,z=-1,t=d;%s\x1b\\" b64)))
    (message "Test 1: Direct display")
    (kitty-image--write-tty test1)
    (sit-for 2)))

(provide 'zw-kitty-image)
