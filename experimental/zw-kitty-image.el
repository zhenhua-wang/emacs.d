;; -*- lexical-binding: t -*-

(defvar zw/kitty-image-buffer "*kitty-image-buffer*")

(defun zw/kitty-image--write-tty (data)
  "Write DATA to /dev/tty directly."
  (let ((coding-system-for-write 'binary)
        (write-region-inhibit-fsync t))
    (write-region data nil "/dev/tty" t 0)))

(defun zw/png-size-px (file)
  "Return (WIDTH . HEIGHT) in pixels by reading PNG IHDR. Works in TTY."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil 0 24)
    (let ((s (buffer-string)))
      ;; full 8-byte PNG signature
      (unless (and (>= (length s) 24)
                   (= (aref s 0) #x89) (= (aref s 1) #x50)
                   (= (aref s 2) #x4E) (= (aref s 3) #x47)
                   (= (aref s 4) #x0D) (= (aref s 5) #x0A)
                   (= (aref s 6) #x1A) (= (aref s 7) #x0A))
        (error "Not a PNG file: %s" file))
      (let ((w (+ (lsh (aref s 16) 24) (lsh (aref s 17) 16)
                  (lsh (aref s 18)  8) (aref s 19)))
            (h (+ (lsh (aref s 20) 24) (lsh (aref s 21) 16)
                  (lsh (aref s 22)  8) (aref s 23))))
        (cons w h)))))

(defvar zw/kitty-image-max-height 2000)

(defun zw/kitty-display-image (file)
  "Display image in Kitty; scale down to fit max height only if too large."
  (interactive (list (read-file-name "Image (PNG): " default-directory nil t)))
  (zw/kitty-clear-image)
  (let* ((buffer (get-buffer-create zw/kitty-image-buffer))
         (data (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally file)
                 (buffer-string)))
         (b64 (base64-encode-string data t))
         (rows (max 1 (window-body-height)))
         (img-h (cdr (zw/png-size-px file)))
         (need-resize (> img-h zw/kitty-image-max-height))
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
             (if need-resize
                 (format "%s\x1b_Ga=T,f=100,z=-1,t=d,r=%d,m=%s;%s\x1b\\"
                         prefix rows more chunk)
               (format "%s\x1b_Ga=T,f=100,z=-1,t=d,m=%s;%s\x1b\\"
                       prefix more chunk))
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

(defun zw/file-image-by-extension-p (file)
  (member (downcase (file-name-extension file))
          '("png" "jpg" "jpeg" "gif" "webp" "svg")))

(defun zw/kitty-dired-find-file (&rest args)
  (let ((file (dired-get-file-for-visit)))
    (if (ignore-errors (zw/file-image-by-extension-p file))
        (zw/kitty-display-image file)
      (dired-find-file)))
  (zw/dired-sidebar-enable (current-buffer)))

(advice-add 'zw/dired-sidebar-find-file :around
            'zw/kitty-dired-find-file)

(provide 'zw-kitty-image)
