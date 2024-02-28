;; -*- lexical-binding: t -*-

(use-package eglot
  :hook
  (python-mode . eglot-ensure)
  (ess-r-mode . eglot-ensure)
  ((c++-mode c-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
              ([remap display-local-help] . nil)
              ("s-i" . consult-eglot-symbols)
              ("s-h" . display-local-help)
              ("s-d" . eldoc))
  :config
  (setq read-process-output-max (* 1024 1024)
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
  ;; patch for polymode
  (with-eval-after-load "polymode"
    (defun zw/buffer-content (START END)
      (if (and (featurep 'polymode)
               polymode-mode)
          (pm--lsp-text)
        (buffer-substring-no-properties START END)))
    (defmacro zw/eglot-patch-macro (patch-func)
      `(psearch-patch ,patch-func
         (psearch-replace '`(buffer-substring-no-properties (point-min) (point-max))
                          '`(zw/buffer-content (point-min) (point-max)))))
    (let ((vc-follow-symlinks t))
      (dolist (func '(eglot--TextDocumentItem
                      eglot--signal-textDocument/didSave
                      eglot--signal-textDocument/didChange))
        (eval `(zw/eglot-patch-macro ,func)))
      ;; HACK: kill eglot.el buffer after applying Eglot patch"
      (kill-buffer "eglot.el"))))

(use-package consult-eglot
  :commands (consult-eglot-symbols))

(provide 'zw-eglot)
