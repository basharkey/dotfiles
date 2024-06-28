;; Bootstrap straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Prevent package.el from loading
(setq package-enable-at-startup nil)

;; Enable use-package integration
(straight-use-package 'use-package)

;;
;; Packages
;;

(use-package yaml-mode
  :straight t
  :mode ("\\.yml\\'"
	 "\\.yaml\\'"
	 "\\.yml.j2\\'"
	 "\\.yaml.j2\\'"))

(use-package terraform-mode
  :straight t)

(use-package go-mode
  :straight t
  :hook (go-mode . (lambda () (setq tab-width 4))))

(use-package csv-mode
  :straight t)

(use-package lilypond
  :straight t)

(use-package sqlite3
  :straight t)

(use-package magit
  :straight t)

(use-package forge
  :straight t
  :after magit)

(use-package counsel
  :straight t
  :config
  (counsel-mode 1))

(use-package ivy
  :straight t
  :config
  (ivy-mode 1)
  ;; Use a single <tab> for completion
  :bind (:map ivy-minibuffer-map
	      ("<tab>" . ivy-alt-done)))

(use-package ivy-prescient
  :straight t
  :after counsel
  :config
  (ivy-prescient-mode 1))

(use-package dired-subtree
  :straight t
  :after dired
  :bind (:map dired-mode-map
	      ("<tab>" . dired-subtree-toggle)
	      ("<backtab>" . dired-subtree-cycle)))

(use-package avy
  :straight t
  :config
  (setq avy-background t)
  ;; Optimized jump keys for halmak keyboard layout
  (setq avy-keys
	(nconc '(?t ?n ?h ?s ?a ?e ?o ?i))) ;; Home row only
  :bind (("M-o" . avy-goto-word-1)
	 ("M-p" . avy-goto-char)))

(use-package restclient
  :straight t
  :mode ("\\.rstc\\'" . restclient-mode)
  :hook (restclient-mode . (lambda () (setq tab-width 2))))

(use-package modus-themes
  :straight t
  :config
  (load-theme 'modus-vivendi t))

(use-package unbound
  :straight t)

(use-package evil
  :straight t
  :init (setq evil-want-keybinding nil)
  :bind (:map evil-normal-state-map
	      ("M-y" . 'bs/evil-paste-pop))
  :config
  (delete 'completion-list-mode evil-emacs-state-modes)
  (evil-mode 1))

;; Evil bindings for everything else including term-mode
(use-package evil-collection
  :straight t
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config
  (setq evil-collection-term-sync-state-and-mode-p t)
  (evil-collection-define-key 'normal 'dired-mode-map
    "n" 'evil-search-next
    "N" 'evil-search-previous
    "G" 'evil-goto-line
    "gg" 'evil-goto-first-line)
  (evil-collection-define-key 'normal 'completion-list-mode-map
    (kbd "RET") 'choose-completion)
  (evil-collection-init))

;; https://www.youtube.com/watch?v=MZPR_SC9LzE
;; (use-package evil-textobj-tree-sitter
;;   :straight t)

;; (use-package terraform-ts-mode
;;   :straight (terraform-ts-mode :type git :host github :repo "kgrotel/terraform-ts-mode"))
