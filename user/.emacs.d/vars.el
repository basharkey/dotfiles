;; Turn off bars
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Font size
(cond ((string= "darwin" system-type) (set-face-attribute 'default nil :height 160))
      (t (set-face-attribute 'default nil :height 150)))

;; Stop Emacs from littering with backups and autosave
(make-directory (concat user-emacs-directory "backups") t)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(make-directory (concat user-emacs-directory "autosave") t)
(setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "autosave") t)))

;; Treat sentences as ending with a single space
(setq sentence-end-double-space nil)

;; Use / to bypass ivy autocomplete for renaming directories
(setq ivy-magic-slash-non-match-action nil)

;; Enable C-<number> to select tabs by tab number
(setq tab-bar-select-tab-modifiers '(control))

;; Show tab numbers in tab bar
(setq tab-bar-tab-hints t)

;; Save existing clipboard value into kill ring before replacing it
(setq save-interprogram-paste-before-kill t)

;; Disable scratch message
(setq initial-scratch-message nil)

;; Auto pair for quotes and brackets
(electric-pair-mode 1)

;; Enables usage of minibuffers in minibuffers, such as calling counsel-yank-pop while performing a query-replace
(setq enable-recursive-minibuffers t)

;; Don't prompt when killing buffer with running process
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; Enable upcase-region command
(put 'upcase-region 'disabled nil)

;; Show match numbers in the search prompt
(setq isearch-lazy-count t)

;; -A List all but do not list implied . and ..
;; -h Human readable sizes
;; -t Sort by time, newest first
(cond ((string= "darwin" system-type) (setq dired-listing-switches "-lAht"))
      (t (string= "darwin" system-type) (setq dired-listing-switches "-lAht --group-directories-first")))

;; Make line numbers relative to current line and enable for all buffers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Timeout TRAMP/SSH connections earlier
(setq tramp-connection-timeout 10)

;; eshell Bash style prompt
(setq eshell-prompt-regexp "^[^#$\n]*[#$] "
      eshell-prompt-function
      (lambda nil
        (concat
	 (user-login-name) "@" (system-name) ":"
	 (if (string= (eshell/pwd) (getenv "HOME"))
	     "~" (eshell/basename (eshell/pwd)))
	 (if (= (user-uid) 0) "# " "$ "))))
