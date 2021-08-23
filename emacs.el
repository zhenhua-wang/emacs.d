;; Load path for manually installed packages
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load path for customied themes
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
          (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; (require 'init-benchmarking)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-always-ensure t)
(setq use-package-verbose t)

(require 'use-package)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default (not (eq system-type 'gnu/linux)))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

(server-start)

;; (use-package spacegray-theme)
(use-package doom-themes)
;; (use-package gruvbox-theme)
;; (use-package nord-theme)

;; (load-theme 'doom-oceanic-next t)
(load-theme 'doom-wilmersdorf t)
;; (load-theme 'doom-nord t)

;; hightlight current row
(global-hl-line-mode t)

;; apply a beacon effect to the hightlighted line
(use-package beacon
  :config
  ;; (setq beacon-blink-when-window-scrolls nil)
  (beacon-mode))

(pcase system-type
  ((or 'gnu/linux 'windows-nt 'cygwin)
   (setq zw/font-size 140))
  ('darwin
   (setq zw/font-size 180)))

;; set the default face
(set-face-attribute 'default nil
                       :font "JetBrainsMono Nerd Font"
                       ;; :background "black"
                       ;; make fonts less tranparent
                       :foreground "white"
                       :weight 'medium
                       :height zw/font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrainsMono Nerd Font"
                    ;; :background "black"
                    :foreground "white"
                    :weight 'normal
                    :height zw/font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    :font "Iosevka Aile" ;"Cantarell"
                    ;; :background "black"
                    :foreground "white"
                    :weight 'light
                    :height zw/font-size)

(column-number-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)


;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :hook (after-init . doom-modeline-init)
  :custom-face
  (mode-line ((t (:height 0.7))))
  (mode-line-inactive ((t (:height 0.7))))
  :custom
  (doom-modeline-height 10)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github t)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon t)
  :config
  (doom-modeline-mode 1)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (set-face-foreground 'show-paren-match "#BF616A") ;; set matched color red
  (show-paren-mode 1))

(setq-default indent-tabs-mode nil)

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(75 . 75) '(100 . 100)))))

(use-package command-log-mode
  :commands command-log-mode)
(use-package neotree
  :commands neotree)

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Set default connection mode to SSH
(setq tramp-default-method "ssh")

(use-package openwith
  :if (eq system-type 'gnu/linux)
  :config
  (setq openwith-associations
        (list
         ;; (list (openwith-make-extension-regexp
         ;;        '("xbm" "pbm" "pgm" "ppm" "pnm"
         ;;          "png" "gif" "bmp" "tif" "jpeg" "jpg"))
         ;;       "feh"
         ;;       '(file))
         (list (openwith-make-extension-regexp
                '("pdf"))
               "zathura"
               '(file))))
  (openwith-mode 1))

(use-package dired
  :straight (:type built-in)
  :ensure nil
  :defer 1
  :commands (dired dired-jump)
  :bind (("C-c r" . ranger))
  :config
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        dired-hide-details-hide-symlink-targets nil
        delete-by-moving-to-trash t)
  (setq insert-directory-program "ls" dired-use-ls-dired t)        ; sort directories first in dired

  (autoload 'dired-omit-mode "dired-x")

  (add-hook 'dired-load-hook
            (lambda ()
              (interactive)
              (dired-collapse))))

(use-package dired-rainbow
  :defer 2
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go"
					    "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(use-package dired-single
  :defer 1)

(use-package dired-ranger
  :defer 1)

(use-package dired-collapse
  :defer 1)

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package org
  :hook
  (org-mode . org-indent-mode)
  (org-mode . variable-pitch-mode)
  (org-mode . visual-line-mode)
  :commands (org-capture org-agenda)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-startup-folded  t;;'content
        org-cycle-separator-lines 2
        org-confirm-babel-evaluate nil
        org-src-preserve-indentation t    ; helps to indent python code in org mode
        org-src-tab-acts-natively t
        org-src-strip-leading-and-trailing-blank-lines t
        ;; show edit buffer below the current window, keeping all
        org-src-window-setup 'split-window-below
        ;; use user defined image size
        org-image-actual-width nil
        ;; make latex formula larger
        org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setq org-todo-keyword-faces
	'(("TODO" . (:foreground "orange red" :weight bold))
	  ("DONE" . (:foreground "green" :weight bold))))

  ;; set org babel languages
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((python . t)
       ;; (ipython . t)
       (R . t)
       (ein . t)))

    ;; This is needed as of Org 9.2
    (require 'org-tempo)
    (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("py" . "src python :results output :session"))
    (add-to-list 'org-structure-template-alist '("r" . "src R :session")))
  )

(setq org-agenda-window-setup 'current-window)
(setq org-agenda-start-with-log-mode t)
(setq org-agenda-span 'day)
(setq org-log-into-drawer t)
;; Make done tasks show up in the agenda log
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-agenda-files
      '("~/Workspace/Documents/OrgFiles/Tasks.org"
        "~/Workspace/Documents/OrgFiles/Events.org"))

;; refiling
(setq org-refile-targets
      '(("Tasks.org" :maxlevel . 1)
        ("Events.org" :maxlevel . 1)))

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-capture-templates
      `(("t" "Tasks / Projects")
        ("tt" "Task" entry (file+olp "~/Workspace/Documents/OrgFiles/Tasks.org" "Inbox")
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
        ("tr" "Research" entry (file+olp "~/Workspace/Documents/OrgFiles/Tasks.org" "Research")
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
        ("ts" "Clocked Entry Subtask" entry (clock)
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

        ("e" "Events")
        ("em" "Meeting" entry
         (file+olp+datetree "~/Workspace/Documents/OrgFiles/Events.org", "Meeting")
         "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)))

(use-package org-wild-notifier
  :hook (after-init . org-wild-notifier-mode)
  :config
  (setq org-wild-notifier-alert-time '(15))
  (setq org-wild-notifier-notification-title "Org Agenda")
  (setq org-wild-notifier--alert-severity 'high)
  (setq org-wild-notifier--day-wide-events t))

(use-package alert
  :config
  (setq alert-default-style 'libnotify))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars nil)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))


(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;; Auto-show Markup Symbols
(use-package org-appear
  :hook (org-mode . org-appear-mode))

;; auto tangle
(use-package org-auto-tangle
  ;; :load-path "site-lisp/org-auto-tangle/"    ;; this line is necessary only if you cloned the repo in your site-lisp directory 
  :defer 1
  :hook (org-mode . org-auto-tangle-mode))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(let* (
       ;; (variable-tuple '(:font "Source Sans Pro"))
       (variable-tuple
        (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
  
  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline t))))))

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Workspace/Documents/RoamNotes")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(
     ;; default template
     ("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ;; few example templates
     ("l" "programming language" plain
      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)

     ("b" "book notes" plain
      "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)

     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)
     ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"   . completion-at-point))
  :config
  (org-roam-setup))

(defun dw/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun dw/org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-appear-mode -1)
  (org-display-inline-images)
  (dw/org-present-prepare-slide))

(defun dw/org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images)
  (org-appear-mode 1))

(defun dw/org-present-prev ()
  (interactive)
  (org-present-prev)
  (dw/org-present-prepare-slide))

(defun dw/org-present-next ()
  (interactive)
  (org-present-next)
  (dw/org-present-prepare-slide))

(use-package org-present
  :bind (:map org-present-mode-keymap
         ("C-c n" . dw/org-present-next)
         ("C-c p" . dw/org-present-prev)
         ("C-c q" . org-present-quit))
  :hook ((org-present-mode . dw/org-present-hook)
         (org-present-mode-quit . dw/org-present-quit-hook)))

(defun zw/org-fold-all-but-current ()
  (interactive)
  (org-remove-occur-highlights)
  (org-overview)
  (org-reveal))

(defun zw/toggle-image-scroll ()
  (interactive)
  (pixel-scroll-mode)
  )

;;(setq split-width-threshold 1)

(use-package company
  ;; :hook (after-init . global-company-mode)
  :hook ((prog-mode . company-mode)
         (eshell-mode . company-mode))
  ;; :if (eq system-type 'gnu/linux)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :config
  (setq completion-ignore-case t))


(use-package company-box
  :after company
  ;; :if (eq system-type 'gnu/linux)
  :hook (company-mode . company-box-mode))

(use-package company-prescient
  :after company
  ;; :if (eq system-type 'gnu/linux)
  :config
  (company-prescient-mode 1))

;; ivy
(use-package ivy
  :diminish
  :bind (("s-f" . swiper)
         ("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ;; override s-tab from creating another minibuffer and make it behave mac-like
         ("s-<tab>" . ivy-next-line) ; "C-j"
         ;; ("s-SPC" . ivy-next-line)
         ("<backtab>" . ivy-previous-line))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  (setq confirm-nonexistent-file-or-buffer t)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . switch-to-buffer)
	 ("C-x C-f" . counsel-find-file)
	 ;; ("C-c b" . counsel-switch-buffer)
	 ("C-c i" . counsel-imenu)
         ("C-c s" . 'counsel-search)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (pcase system-type
    ('darwin
     (setq browse-url-browser-function 'xwidget-webkit-browse-url))
    ('gnu/linux
     (setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser")))
  (setq counsel-search-engine 'google)
  (counsel-mode 1))

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :after ivy
  :defer 1
  :init
  (setq ivy-flx-limit 10000))

(use-package ivy-posframe
  :disabled
  :custom
  (ivy-posframe-width      115)
  (ivy-posframe-min-width  115)
  (ivy-posframe-height     10)
  (ivy-posframe-min-height 10)
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters '((parent-frame . nil)
                                  (left-fringe . 8)
                                  (right-fringe . 8)))
  (ivy-posframe-mode 1))

;; precscient
(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1)
  :custom
  (setq ivy-prescient-enable-filtering t)
  )

(use-package prescient
  :after counsel
  :config
  (prescient-persist-mode 1)
  (setq prescient-sort-length-enable t)
  ;; (setq prescient-history-length 20)
  )

(use-package all-the-icons-ivy
  :hook
  (after-init . all-the-icons-ivy-setup)
  :config
  (setq all-the-icons-ivy-file-commands
        '(counsel-find-file counsel-recentf counsel-ibuffer counsel-switch-buffer)))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1)
  :config
  (setq all-the-icons-ivy-rich-color-icon t))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :after counsel
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     ((ivy-rich-candidate (:width 40))
                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
                      (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
                     :predicate
                     (lambda (cand)
                       (if-let ((buffer (get-buffer cand)))
                           ;; Don't mess with EXWM buffers
                           (with-current-buffer buffer
                             (not (derived-mode-p 'exwm-mode)))))))))

(use-package undo-tree
  :disabled
  :init
  (global-undo-tree-mode 1))

(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; check word spelling
(use-package flyspell
  :init
  (progn
    (flyspell-mode 1))
  :config
  (progn 
    (setq ispell-program-name "aspell")
    (setq ispell-list-command "--list") ;; run flyspell with aspell, not ispell
    ))

;; check code syntax
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  )

(use-package winner
  :config
  (winner-mode))

;; set preference to horizontal split
(defun split-window-sensibly-prefer-horizontal (&optional window)
  "Based on split-window-sensibly, but designed to prefer a horizontal split,
i.e. windows tiled side-by-side."
  (interactive)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically
             (with-selected-window window
               (split-window-below)))
        (and
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
         (not (window-minibuffer-p window))
         (let ((split-width-threshold 0))
           (when (window-splittable-p window t)
             (with-selected-window window
               (split-window-right))))))))

(setq split-width-threshold  80
      split-height-threshold 30
      ;; xsplit-window-preferred-function 'split-window-sensibly-prefer-horizontal
      )

(use-package ace-window
  ;:bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  (aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))

(use-package popper
  :bind (("s-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("s-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("[Oo]utput\\*$"
          "^\\*Warnings\\*"
          "^\\*Compile-Log\\*"
          "^\\*Messages\\*"
          "^\\*Backtrace\\*"
          "^\\*ielm\\*"
          "^\\*Tex Help\\*"
          "^\\*Shell Command Output\\*"
          "^\\*Async Shell Command\\*"
          "^\\*WordNut\\*"
          "^\\*straight-process\\*"
          help-mode
          eshell-mode
          inferior-ess-r-mode
          inferior-python-mode
          message-mode
          compilation-mode))
  ;; only show the popper in the same project
  ;; (setq popper-group-function #'popper-group-by-project)
  ;; (popper-mode -1)
  (popper-mode +1))

(setq display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

(setq display-buffer-alist
      '(;; top side window
        ("\\*\\(Flymake\\|Package-Lint\\|vc-git :\\).*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 0))
        ("\\*Messages.*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 1))
        ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 2))
        ;; left side window
        ("\\*Help.*"            ; See the hooks for `visual-line-mode'
         (display-buffer-in-side-window)
         (window-width . 0.5)
         (side . right)
         (slot . -1))
        ;; bottom buffer (NOT side window)
        ("\\*.*\\(e?shell\\|v?term\\).*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . 0.35))
        ;; below current window
        ("\\*Calendar.*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . shrink-window-if-larger-than-buffer))))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

;; preview markdown
(use-package grip-mode)

;; latex
(use-package tex
  :ensure auctex
  :straight (:type built-in)
  :config
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-save-query nil))

;; epub
(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; pdf
(use-package pdf-tools
  :pin manual ;; don't reinstall when package updates
  :magic ("%PDF" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (pdf-tools-install :no-query)
  (require 'pdf-occur)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick t)
  ;; revert the PDF-buffer after the TeX compilation has finished
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  )

(use-package wordnut)

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Use completion-at-point to provide completions in eshell
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
  ;; (define-key eshell-mode-map [remap eshell-pcomplete] 'completion-at-point)
  ;; (define-key eshell-mode-map (kbd "<tab>") 'company-complete)

  (setenv "PAGER" "cat")

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))
  )

;; fish completion
(use-package fish-completion
  :hook (eshell-mode . fish-completion-mode))

;; show complete history
(use-package esh-autosuggest
  ;; :disabled
  :hook (eshell-mode . esh-autosuggest-mode)
  :config
  (setq esh-autosuggest-delay 0.5)
  (set-face-foreground 'company-preview-common "#4b5668")
  (set-face-background 'company-preview nil)
  )

;; command highlight
(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-info-banner
  :disabled
  :straight (eshell-info-banner :type git
                                :host github
                                :repo "phundrak/eshell-info-banner.el")
  :hook (eshell-banner-load . eshell-info-banner-update-banner)
  )

;; themes
(use-package eshell-prompt-extras
  :after esh-mode
  :config
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt t     ; damn! this means ineditable prompt!
          eshell-prompt-function 'epe-theme-lambda))
  )

(defun zw/show-eshell()
  (interactive)
  ;; (select-window (split-window-vertically -15))
  (eshell)
  ;; (text-scale-set 0.7)
  )

(when (eq system-type 'gnu/linux)
  (org-babel-load-file "~/.emacs.d/emacs-desktop.org"))

(when (eq system-type 'gnu/linux)
  (org-babel-load-file "~/.emacs.d/emacs-development.org"))

(when (eq system-type 'gnu/linux)
  (org-babel-load-file "~/.emacs.d/emacs-system.org"))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; undo
(global-set-key (kbd "s-z") 'undo)
;; cut copy paste
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
;; window
(global-set-key (kbd "s-w") 'delete-window)
(global-set-key (kbd "s-t") 'split-window-sensibly-prefer-horizontal)

;; check dict
(global-set-key (kbd "C-c w") 'wordnut-search)
(global-set-key (kbd "C-c W") 'wordnut-lookup-current-word)

;; toggle transparency
(global-set-key (kbd "C-c t") 'toggle-transparency)
