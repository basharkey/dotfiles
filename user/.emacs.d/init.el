(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'"
	 "\\.yaml\\'"))

(use-package sqlite3
  :ensure t)

(use-package magit
  :ensure t
  :after sqlite3)

(use-package forge
  :ensure t
  :after magit)

(use-package counsel
  :ensure t)

;; backup/autosave
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "autosave") t)))
(setq lock-file-name-transforms
      `((".*" ,(concat user-emacs-directory "lock") t)))

(set-face-attribute 'default nil :height 110)

;; add function to remove leftover whitespace

;; use / to bypass ivy autocomplete for renaming directories
(setq ivy-magic-slash-non-match-action nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wombat))
 '(ivy-mode t)
 '(package-selected-packages '(yaml-mode use-package counsel))
 '(scroll-bar-mode nil))
