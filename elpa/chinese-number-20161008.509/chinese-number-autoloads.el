;;; chinese-number-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "chinese-number" "chinese-number.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from chinese-number.el

(autoload 'chinese-number--convert-arabic-to-chinese "chinese-number" "\
convert a number in Arabic format to Chinese.

\(fn NUMBER)" t nil)

(autoload 'chinese-number--convert-chinese-to-arabic "chinese-number" "\
convert a number in Chinese format to Arabic.

\(fn NUMBER)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "chinese-number" '("chinese-number--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; chinese-number-autoloads.el ends here
