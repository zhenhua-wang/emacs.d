;; -*- lexical-binding: t -*-

(use-package eaf
  :demand t
  :if (display-graphic-p)
  :straight '(eaf :host github :repo "emacs-eaf/emacs-application-framework"
                  :files ("*"))
  :hook (eaf-mode . zw/eaf-setup)
  :init
  (setq zw/eaf-bin "~/.conda/envs/eaf/bin"
        eaf-python-command (expand-file-name "python3" zw/eaf-bin))
  (defun zw/eaf-setup ()
    (setq eaf-buffer-background-color (face-background 'default))
    ;; dired-sidebar open focus eaf buffer
    (when (eq this-command 'zw/dired-sidebar-find-file)
      (let ((buffer-name (buffer-name)))
        (run-with-timer
         0.1 nil (lambda () (select-window (display-buffer buffer-name)))))))
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
  (advice-add 'zw/tab-line-tab-icon :around 'zw/eaf-tab-line-icon)
  (advice-add 'zw/modeline-init :after
              (lambda () (setq eaf-mode-line-format mode-line-format)))
  ;; solve fail reloading pdf after compiling latex
  (defun zw/eaf-restart-process-advisor (&rest _)
    (eaf-restart-process))
  (advice-add 'org-export-dispatch :after 'zw/eaf-restart-process-advisor)
  (advice-add 'TeX-command-master :after 'zw/eaf-restart-process-advisor)
  ;; grab keybord in exwm
  (with-eval-after-load "exwm"
    (if (executable-find "wmctrl")
        (setq eaf-is-member-of-focus-fix-wms t)
      (if (executable-find "dunstify")
          (call-process-shell-command
           "dunstify -u critical -i dialog-error EXWM 'wmctrl not detected. EAF requires wmctrl in EXWM'" nil 0)
        (display-warning :emergency "wmctrl not detected. EAF requires wmctrl in EXWM")))))

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
  (eaf-bind-key reload_document "s-r" eaf-pdf-viewer-keybinding)
  (with-eval-after-load "tex"
    (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
    (add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))))

(use-package eaf-image-viewer
  :if (display-graphic-p)
  :demand t
  :straight (:type git :host github :repo "emacs-eaf/eaf-image-viewer"
                   :files ("*"))
  :config
  (eaf-bind-key reload_image "s-r" eaf-image-viewer-keybinding))

;; install functions
(defvar zw/eaf-install-env-string "/opt/miniconda3/bin/conda create -n eaf -y")
(defvar zw/eaf-activate-env-string  "source /opt/miniconda3/bin/activate eaf")
(defvar zw/eaf-install-dependecies-string "conda install conda-forge::python conda-forge::nodejs -y && pip install packaging PyQt6-WebEngine PyQt6 PyQt6-sip setuptools sexpdata epc pymupdf")
(defvar zw/eaf-install-app-string
  (let* ((image-base-path (expand-file-name "straight/repos/eaf-image-viewer" user-emacs-directory))
         (image-build-path (replace-regexp-in-string "repos" "build" image-base-path))
         (image-base-modules (expand-file-name "node_modules" image-base-path))
         (image-build-modules (expand-file-name "node_modules" image-build-path)))
    (format "npm install %s --prefix %s && rm -rf %s && ln -sf %s %s"
            image-base-path
            image-base-path
            image-build-modules
            image-base-modules
            image-build-modules)))

(defun zw/eaf-install-all ()
  "Install eaf environment, dependencies and apps."
  (interactive)
  (async-shell-command (concat zw/eaf-install-env-string "&&"
                               zw/eaf-activate-env-string "&&"
                               zw/eaf-install-dependecies-string "&&"
                               zw/eaf-install-app-string)))

(defun zw/eaf-update-app ()
  "Update eaf apps."
  (interactive)
  (dolist (pkg '("eaf-pdf-viewer" "eaf-image-viewer"))
    (straight-pull-package pkg)
    (straight-check-package pkg))
  (async-shell-command (concat zw/eaf-activate-env-string "&&"
                               zw/eaf-install-app-string)))

(defun zw/eaf-update-env ()
  "Update eaf dependencies."
  (interactive)
  (async-shell-command (concat zw/eaf-activate-env-string "&&"
                               zw/eaf-install-dependecies-string)))

(provide 'zw-eaf)
