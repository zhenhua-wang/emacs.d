;; -*- lexical-binding: t -*-

(use-package eaf
  :demand t
  :if (display-graphic-p)
  :straight '(eaf :host github :repo "emacs-eaf/emacs-application-framework"
                  :files ("*"))
  :hook (eaf-mode . (lambda ()
                      (setq eaf-buffer-background-color (face-background 'default))
                      ;; HACK: force focus eaf buffer
                      (let ((buffer-name (buffer-name)))
                        (run-with-timer
                         0.1 nil (lambda () (select-window (display-buffer buffer-name)))))))
  :init
  (setq zw/eaf-bin "~/.conda/envs/eaf/bin"
        eaf-python-command (expand-file-name "python3" zw/eaf-bin))
  (defun zw/eaf-install-all ()
    (interactive)
    (let ((zw/eaf-image-viewer-path (expand-file-name
                                     "straight/build/eaf-image-viewer" user-emacs-directory)))
      (async-shell-command (format "/opt/miniconda3/bin/conda create -n eaf python=3.11 && source /opt/miniconda3/bin/activate eaf && conda install conda-forge::wmctrl conda-forge::nodejs conda-forge::qt6-multimedia && pip install packaging PyQt6-WebEngine PyQt6 PyQt6-sip setuptools sexpdata epc pymupdf && npm install %s --prefix %s" zw/eaf-image-viewer-path zw/eaf-image-viewer-path))))
  (defun zw/eaf-install-app ()
    (interactive)
    (let ((zw/eaf-image-viewer-path (expand-file-name
                                     "straight/build/eaf-image-viewer" user-emacs-directory)))
      (async-shell-command (format "source /opt/miniconda3/bin/activate eaf && npm install %s --prefix %s"
                                   zw/eaf-image-viewer-path zw/eaf-image-viewer-path))))
  (defun zw/eaf-update-env ()
    (interactive)
    (async-shell-command "source /opt/miniconda3/bin/activate eaf && conda update --all"))
  :config
  (advice-add 'eaf-install :override (lambda (&rest _)))
  (advice-add 'eaf-install-and-update :override (lambda (&rest _)))
  (add-to-list 'zw/tab-line-buffer-group-alist '((eq major-mode 'eaf-mode) . File))
  (defun zw/eaf-open-in-external (orig-fun)
    (if (eq major-mode 'eaf-mode)
        (let ((buffer-file-name (eaf-get-path-or-url)))
          (funcall orig-fun))
      (funcall orig-fun)))
  (advice-add 'zw/open-in-external :around 'zw/eaf-open-in-external)
  (defun zw/eaf-tab-line-icon (orig-fun buffer)
    (with-current-buffer buffer
      (if (eq major-mode 'eaf-mode)
          (pcase eaf--buffer-app-name
            ("pdf-viewer" (nerd-icons-icon-for-file "pdf.pdf"))
            ("image-viewer" (nerd-icons-faicon "nf-fa-image" :face 'nerd-icons-orange)))
        (funcall orig-fun buffer))))
  (advice-add 'zw/tab-line-tab-icon :around 'zw/eaf-tab-line-icon))

(use-package eaf-pdf-viewer
  :if (display-graphic-p)
  :demand t
  :straight (:type git :host github :repo "emacs-eaf/eaf-pdf-viewer"
                   :files ("*"))
  :config
  (setq eaf-pdf-default-zoom  2
        eaf-pdf-dark-mode "ignore")
  (eaf-bind-key scroll_up_page "n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key copy_select "s-c" eaf-pdf-viewer-keybinding)
  (eaf-bind-key nil "M-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key reload_document "s-r" eaf-pdf-viewer-keybinding))

(use-package eaf-image-viewer
  :if (display-graphic-p)
  :demand t
  :straight (:type git :host github :repo "emacs-eaf/eaf-image-viewer"
                   :files ("*"))
  :config
  (eaf-bind-key reload_image "s-r" eaf-image-viewer-keybinding))

(provide 'zw-eaf)
