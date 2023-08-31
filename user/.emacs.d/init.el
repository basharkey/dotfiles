(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

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

(use-package csv-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package forge
  :ensure t
  :after magit)

(use-package counsel
  :ensure t
  :config
  (ivy-mode 1)
  (counsel-mode 1))

;; (use-package helm
;;   :ensure t
;;   :config (helm-mode 1))

;; backup/autosave
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "autosave") t)))

(set-face-attribute 'default nil :height 130)

(use-package dired-subtree :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

;; add function to remove leftover whitespace

;; use / to bypass ivy autocomplete for renaming directories
(setq ivy-magic-slash-non-match-action nil)

;; Enable C-<number> to select tabs by tab number
(setq tab-bar-select-tab-modifiers '(control))
;; Show tab numbers in tab bar
(setq tab-bar-tab-hints t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wombat)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Custom functions

;; http://emacs-fu.blogspot.com/2009/11/copying-lines-without-selecting-them.html
(defun copy-line()
  (interactive)
  (message "Copied line")
  (copy-region-as-kill (line-beginning-position) (line-end-position)))

(defun copy-line-no-whitespace()
  (interactive)
  (message "Copied line (without whitespace)")
  (setq x (point))
  (back-to-indentation)
  (copy-region-as-kill (point) (line-end-position))
  (goto-char x))

(defun quick-ansi-term()
  (interactive)
  (ansi-term shell-file-name (concat "ansi-term" " " default-directory)))

(defun copy-to-char (arg char)
  "`king-ring-save' up to and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
See also `copy-up-to-char'."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     (read-char-from-minibuffer "Copy to char: "
						nil 'read-char-history)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
	(setq char (or (aref translation-table-for-input char) char))))
  (copy-region-as-kill (point) (progn
			 (search-forward (char-to-string char) nil nil arg)
			 (point))))

(defun copy-up-to-char (arg char)
  "`kill-ring-save' up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     (read-char-from-minibuffer "Copy up to char: "
						nil 'read-char-history)))
  (let ((direction (if (>= arg 0) 1 -1)))
    (copy-region-as-kill (point)
		 (progn
		   (forward-char direction)
		   (unwind-protect
		       (search-forward (char-to-string char) nil nil arg)
		     (backward-char direction))
		   (point)))))

;;
;; Custom keybindings
;;

;; Quick ansi-term
(global-set-key (kbd "C-x a") 'quick-ansi-term)

;; Copy line
(global-set-key (kbd "C-x w") 'copy-line)
(global-set-key (kbd "C-x W") 'copy-line-no-whitespace)

(global-set-key (kbd "C-x G") 'magit-clone)

;; Remove C-<tab> keybinding from magit so it doesn't conflict with tab-bar-switch-to-next-tab
(add-hook 'magit-mode-hook
	  (lambda()
	    (local-unset-key (kbd "C-<tab>"))))

;; Easier keybinds for term mode
(add-hook 'term-mode-hook
  (lambda ()
    (define-key term-raw-map (kbd "M-j") 'term-line-mode)
    (define-key term-mode-map (kbd "M-k") 'term-char-mode)))

;; Rebind zap to char
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; Copy to char
(global-set-key (kbd "C-z") 'copy-to-char)
(global-set-key (kbd "C-Z") 'copy-up-to-char)

;; Ideas
;; In dired pressing 1, 2, or 3 expands dirs using dired-subtree
;; 1 expand dirs depth 1, 2 expand dirs depth2
;; kinda like magit 1, 2, or 3
