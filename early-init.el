;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:

;; set UI
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)            ; Disable the menu bar

;; Set frame
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; no title bar on macos
(add-to-list 'default-frame-alist '(undecorated . t))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; native-comp settings
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)
    ))

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Premature redisplays can substantially affect startup times and produce
;; ugly flashes of unstyled Emacs.
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

;; Site files tend to use `load-file', which emits "Loading X..." messages in
;; the echo area, which in turn triggers a redisplay. Redisplays can have a
;; substantial effect on startup times and in this case happens so early that
;; Emacs may flash white while starting up.
(define-advice load-file (:override (file) silence)
  (load file nil 'nomessage))

;; Undo our `load-file' advice above, to limit the scope of any edge cases it
;; may introduce down the road.
(define-advice startup--load-user-init-file (:before (&rest _) init-doom)
  (advice-remove #'load-file #'load-file@silence))
