;; -*- lexical-binding: t -*-

(use-package eaf
  :straight '(eaf :host github :repo "emacs-eaf/emacs-application-framework"
                  :files ("*"))
  :config
  (setq zw/eaf-bin "~/.conda/envs/eaf/bin"
        eaf-python-command (expand-file-name "python3" zw/eaf-bin))
  (defun eaf-install ())
  (defun eaf-install-and-update ())
  (defun zw/eaf-install ()
    (let ((zw/eaf-image-viewer-path (expand-file-name
                                     "straight/build/eaf-image-viewer" user-emacs-directory)))
      (async-shell-command (format "/opt/miniconda3/bin/conda create -n eaf python=3.11 && source /opt/miniconda3/bin/activate eaf && conda install conda-forge::wmctrl conda-forge::nodejs conda-forge::qt6-multimedia && pip install packaging PyQt6-WebEngine PyQt6 PyQt6-sip setuptools sexpdata epc pymupdf && npm install %s --prefix %s" zw/eaf-image-viewer-path zw/eaf-image-viewer-path)))))

(use-package eaf-pdf-viewer
  :straight (:type git :host github :repo "emacs-eaf/eaf-pdf-viewer"
                   :files ("*"))
  :config
  (eaf-setq eaf-pdf-default-zoom  2)
  (eaf-setq eaf-pdf-dark-mode "ignore")
  (eaf-bind-key scroll_up_page "n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key copy_select "s-c" eaf-pdf-viewer-keybinding))

(use-package eaf-image-viewer
  :straight (:type git :host github :repo "emacs-eaf/eaf-image-viewer"
                   :files ("*")))

(provide 'zw-eaf)
