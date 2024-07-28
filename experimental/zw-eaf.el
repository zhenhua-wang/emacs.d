;; -*- lexical-binding: t -*-

;; dependencies
;;
;; /opt/miniconda3/bin/conda create -n eaf python=3.11
;; source /opt/miniconda3/bin/activate eaf
;; conda install conda-forge::wmctrl conda-forge::nodejs conda-forge::qt6-multimedia
;; pip install packaging PyQt6-WebEngine PyQt6 PyQt6-sip setuptools sexpdata epc pymupdf
(use-package eaf
  :straight '(eaf :host github :repo "emacs-eaf/emacs-application-framework"
                  :files ("*"))
  :config
  (setq eaf-python-command "~/.conda/envs/eaf/bin/python3"))

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
                   :files ("*"))
  :config
  (defun zw/eaf-image-viewer-install ()
    (interactive)
    (let ((zw/eaf-image-viewer-path (expand-file-name
                                     "straight/build/eaf-image-viewer" user-emacs-directory))
          (path (getenv "PATH")))
      (setenv "PATH" (concat path ":/home/zhenhua/.conda/envs/eaf/bin"))
      (shell-command (format "/home/zhenhua/.conda/envs/eaf/bin/npm install %s --prefix %s"
                             zw/eaf-image-viewer-path zw/eaf-image-viewer-path))
      (setenv "PATH" path))))

(provide 'zw-eaf)
