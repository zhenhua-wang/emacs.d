;; -*- lexical-binding: t -*-

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(require 'use-package-ensure)
(require 'package)
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-vc-prefer-newest t
      package-native-compile t
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-activate-all)
(add-hook 'package-menu-mode-hook 'zw/visual-line-disable)

(defun zw/package-upgrade-all ()
  "Upgrade all installed packages."
  (interactive)
  (package-upgrade-all)
  (package-vc-upgrade-all))

(provide 'zw-package)
