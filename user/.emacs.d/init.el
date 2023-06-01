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

(use-package magit
  :ensure t)

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

(set-face-attribute 'default nil :height 110)

(use-package dired-subtree :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

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
 '(package-selected-packages '(f yaml-mode use-package counsel))
 '(scroll-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Custom functions

;; http://emacs-fu.blogspot.com/2009/11/copying-lines-without-selecting-them.html
(defun save-line()
  (interactive)
  (message "Copied line")
  (copy-region-as-kill (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-x w") 'save-line)

(defun save-line-no-whitespace()
  (interactive)
  (message "Copied line (without whitespace)")
  (back-to-indentation)
  (copy-region-as-kill (point) (line-end-position)))

(global-set-key (kbd "C-x W") 'save-line-no-whitespace)
