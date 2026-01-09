;; -*- lexical-binding: t -*-

(defvar zw/kitty-image-buffer "*kitty-image-buffer*")

(defun zw/kitty-image--write-tty (data)
  "Write DATA to /dev/tty directly."
  (let ((coding-system-for-write 'binary)
        (write-region-inhibit-fsync t))
    (write-region data nil "/dev/tty" t 0)))

(defun zw/kitty-display-image (file)
  "Display image in Kitty, fit to terminal height while preserving aspect ratio."
  (interactive
   (list (read-file-name "Image: " default-directory nil t)))
  (let* ((buffer (get-buffer-create zw/kitty-image-buffer))
         (data (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally file)
                 (buffer-string)))
         (b64 (base64-encode-string data t))
         (rows (window-body-height))
         (cursor-up (format "\x1b[%dA" 10))
         (chunk-size 4096)
         (len (length b64))
         (pos 0)
         (first t))
    (while (< pos len)
      (let* ((end (min len (+ pos chunk-size)))
             (chunk (substring b64 pos end))
             (more (if (< end len) "1" "0"))
             (prefix (if first cursor-up "")))
        (setq first nil)
        (zw/kitty-image--write-tty
         (if (= pos 0)
             (format "%s\x1b_Ga=T,f=100,z=-1,t=d,r=%d,m=%s;%s\x1b\\"
                     prefix rows more chunk)
           (format "\x1b_Gm=%s;%s\x1b\\" more chunk)))
        (setq pos end)))
    (switch-to-buffer buffer)))

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
