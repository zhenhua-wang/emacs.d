;; -*- lexical-binding: t -*-

(use-package eaf
  :demand t
  :if (display-graphic-p)
  :vc (:url "https://github.com/emacs-eaf/emacs-application-framework")
  :hook (eaf-mode . zw/eaf-setup)
  :init
  (setq zw/eaf-bin "~/.conda/envs/eaf/bin"
        eaf-python-command (expand-file-name "python3" zw/eaf-bin)
        eaf-find-file-ext-blacklist (append '("md" "org" "html" "htm")
                                            zw/openwith-associations-ext))
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
  (defun zw/eaf-open-in-external (orig-fun)
    (if (eq major-mode 'eaf-mode)
        (let ((buffer-file-name (eaf-get-path-or-url)))
          (funcall orig-fun))
      (funcall orig-fun)))
  (advice-add 'zw/open-in-external :around 'zw/eaf-open-in-external)
  (with-eval-after-load "zw-tab-line"
    (defun zw/eaf-tab-line-icon (orig-fun buffer)
      (with-current-buffer buffer
        (if (eq major-mode 'eaf-mode)
            (pcase eaf--buffer-app-name
              ("pdf-viewer" (nerd-icons-icon-for-file "pdf.pdf"))
              ("image-viewer" (nerd-icons-faicon "nf-fa-image" :face 'nerd-icons-orange))
              ("browser" (nerd-icons-devicon "nf-dev-chrome" :face 'nerd-icons-blue)))
          (funcall orig-fun buffer))))
    (advice-add 'zw/tab-line-tab-icon :around 'zw/eaf-tab-line-icon)
    (add-to-list 'zw/tab-line-buffer-group-alist '((eq major-mode 'eaf-mode) . File)))
  (advice-add 'zw/modeline-init :after
              (lambda () (setq eaf-mode-line-format mode-line-format)))
  ;; always open eaf buffers in the largest window
  (add-to-list 'display-buffer-alist
               `(,(lambda (buffer _action)
                    (with-current-buffer buffer
                      (derived-mode-p 'eaf-mode)))
                 (display-buffer-reuse-mode-window zw/display-buffer-in-largest-window)))
  ;; grab keybord in exwm
  (with-eval-after-load "exwm"
    (if (executable-find "wmctrl")
        (setq eaf-is-member-of-focus-fix-wms t)
      (if (executable-find "dunstify")
          (call-process-shell-command
           "dunstify -u critical -i dialog-error EXWM 'wmctrl not detected. EAF requires wmctrl in EXWM'" nil 0)
        (display-warning :emergency "wmctrl not detected. EAF requires wmctrl in EXWM"))))
  ;; advise poly-rliteral callback
  (defun zw/eaf--find-file-advisor (orig-fn file &rest args)
    (with-selected-window (get-largest-window (selected-frame) nil)
      (eaf--find-file orig-fn file nil args)))
  (advice-add 'poly-rliteral--async-callback-find-file :around #'zw/eaf--find-file-advisor))

(use-package eaf-pdf-viewer
  :if (display-graphic-p)
  :demand t
  :vc (:url "https://github.com/emacs-eaf/eaf-pdf-viewer")
  :config
  (setq eaf-pdf-default-zoom  2
        eaf-pdf-dark-mode "ignore")
  (eaf-bind-key eaf-pdf-narrow-search "s-f" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_up_page "n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key copy_select "s-c" eaf-pdf-viewer-keybinding)
  (eaf-bind-key reload_document "s-r" eaf-pdf-viewer-keybinding)
  (eaf-bind-key zoom_out "s--" eaf-pdf-viewer-keybinding)
  (eaf-bind-key zoom_in "s-=" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_to_end "M->" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_to_begin "M-<" eaf-pdf-viewer-keybinding)
  (eaf-bind-key nil "M-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key toggle_presentation_mode "s-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key quit_presentation_mode "q" eaf-pdf-viewer-keybinding)
  (eaf-bind-key action_quit "<escape>" eaf-pdf-viewer-keybinding)
  (with-eval-after-load "tex"
    (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
    (add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))))

(use-package eaf-image-viewer
  :if (display-graphic-p)
  :demand t
  :vc (:url "https://github.com/emacs-eaf/eaf-image-viewer")
  :config
  (eaf-bind-key reload_image "s-r" eaf-image-viewer-keybinding))

(use-package eaf-browser
  :if (display-graphic-p)
  :demand t
  :vc (:url "https://github.com/emacs-eaf/eaf-browser")
  :config
  ;; save closed urls
  (defvar zw/eaf-browser-n-closed-urls 5)
  (defvar zw/eaf-browser--closed-urls nil)
  (defun zw/eaf-browser--kill-hook ()
    (when (and eaf--buffer-app-name
               (string= eaf--buffer-app-name "browser"))
      (add-to-list 'zw/eaf-browser--closed-urls eaf--buffer-url)
      (when (and zw/eaf-browser-n-closed-urls
                 (length> zw/eaf-browser--closed-urls
                          zw/eaf-browser-n-closed-urls))
        (setq zw/eaf-browser--closed-urls
              (cl-subseq zw/eaf-browser--closed-urls
                         0 zw/eaf-browser-n-closed-urls)))))
  (add-hook 'kill-buffer-hook 'zw/eaf-browser--kill-hook)
  ;; restore closed urls
  (defun zw/eaf-browser-restore-url ()
    "Restore the recent closed url in EAF browser."
    (interactive)
    (if zw/eaf-browser--closed-urls
        (progn (eaf-open-browser (car zw/eaf-browser--closed-urls))
               (setq zw/eaf-browser--closed-urls (cdr zw/eaf-browser--closed-urls)))
      (message "No recently closed URLs")))
  (eaf-bind-key zw/eaf-browser-restore-url "s-T" eaf-browser-keybinding)
  (defun zw/eaf-webengine-zoom-level-string ()
    (shell-command-to-string
     "gsettings get org.gnome.desktop.interface text-scaling-factor"))
  (setq eaf-browser-continue-where-left-off t
        eaf-browser-enable-adblocker t
        browse-url-browser-function 'eaf-open-browser)
  (when-let ((zoom-level-string (zw/eaf-webengine-zoom-level-string)))
    (setq eaf-webengine-default-zoom (string-to-number zoom-level-string)))
  (eaf-create-send-key-function "home")
  (eaf-create-send-key-function "end")
  (eaf-bind-key eaf-send-home-key "C-a" eaf-browser-keybinding)
  (eaf-bind-key eaf-send-end-key "C-e" eaf-browser-keybinding)
  (eaf-bind-key refresh_page "s-r" eaf-browser-keybinding)
  (eaf-bind-key zoom_out "s--" eaf-browser-keybinding)
  (eaf-bind-key zoom_in "s-=" eaf-browser-keybinding)
  (eaf-bind-key undo_action "s-z" eaf-browser-keybinding)
  (eaf-bind-key redo_action "s-Z" eaf-browser-keybinding)
  (eaf-bind-key copy_text "s-c" eaf-browser-keybinding)
  (eaf-bind-key yank_text "s-v" eaf-browser-keybinding)
  (eaf-bind-key select_all_or_input_text "s-a" eaf-browser-keybinding)
  (eaf-bind-key search_text_forward "s-f" eaf-browser-keybinding)
  (eaf-bind-key history_forward "s-]" eaf-browser-keybinding)
  (eaf-bind-key history_backward "s-[" eaf-browser-keybinding)
  (eaf-bind-key close_buffer "s-w" eaf-browser-keybinding)
  (eaf-bind-key scroll_to_begin "M-<" eaf-browser-keybinding)
  (eaf-bind-key scroll_to_bottom "M->" eaf-browser-keybinding)
  (eaf-bind-key emacs-websearch "s-t" eaf-browser-keybinding)
  ;; disable all alphabets
  (eaf-bind-key nil "SPC" eaf-browser-keybinding)
  (cl-loop for c from ?a to ?z
           do (eaf-bind-key nil (string c) eaf-browser-keybinding)))

;; install functions
(defvar zw/eaf-apps '(eaf-pdf-viewer eaf-image-viewer eaf-browser))
(defvar zw/eaf-install-env-string "/opt/miniconda3/bin/conda create -n eaf -y")
(defvar zw/eaf-activate-env-string  "source /opt/miniconda3/bin/activate eaf")
(defvar zw/eaf-install-dependecies-string "conda install conda-forge::python conda-forge::nodejs -y && pip install packaging epc sexpdata tld lxml PyQt6 PyQt6-Qt6 PyQt6-sip PyQt6-WebEngine PyQt6-WebEngine-Qt6 setuptools pymupdf requests")

(defun zw/eaf-apps-install-command ()
  (mapconcat
   (lambda (app)
     (when-let* ((app-name (symbol-name app))
                 (image-path (file-name-directory (find-library-name app-name))))
       (format "npm install %s --prefix %s" image-path image-path)))
   ;; remove non-nodejs apps
   (cl-remove-if (lambda (app)
                   (member app '(eaf-pdf-viewer)))
                 zw/eaf-apps)
   "&&"))

(defun zw/eaf-install-all ()
  "Install eaf environment, dependencies and apps."
  (interactive)
  (async-shell-command (concat zw/eaf-install-env-string "&&"
                               zw/eaf-activate-env-string "&&"
                               zw/eaf-install-dependecies-string "&&"
                               (zw/eaf-apps-install-command))))

(defun zw/eaf-update-app ()
  "Update eaf apps."
  (interactive)
  (dolist (app zw/eaf-apps)
    (package-vc-upgrade (cadr (assq app package-alist))))
  (async-shell-command (concat zw/eaf-activate-env-string "&&"
                               (zw/eaf-apps-install-command))))

(defun zw/eaf-update-env ()
  "Update eaf dependencies."
  (interactive)
  (async-shell-command (concat zw/eaf-activate-env-string "&&"
                               zw/eaf-install-dependecies-string)))

(provide 'zw-eaf)
