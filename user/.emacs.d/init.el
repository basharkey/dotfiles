(load (concat user-emacs-directory "packages.el"))

;;
;; Variables
;;

;; Turn off bars
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Font size
(set-face-attribute 'default nil :height 150)

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
(setq dired-listing-switches "-lAht --group-directories-first")

;;
;; Custom functions
;;

(defun bs/generate-new-term-buffer-name (base-name)
  " Check if buffer with name BASE-NAME + NUM exists starting at 1.
If so increment NUM by 1 to generate a new unique buffer name."
  (let ((num 1))
    (while (get-buffer (concat "*" base-name (number-to-string num) "*"))
      (setq num (+ num 1)))
    (concat base-name (number-to-string num))))

(defun bs/ansi-term ()
  (interactive)
  ;; (ansi-term shell-file-name (concat "ansi-term" " " default-directory)))
  (ansi-term shell-file-name (bs/generate-new-term-buffer-name "at")))

(defun load-user-init-file()
  "Evaluate `user-init-file'"
  (interactive)
  (load-file user-init-file))

;; http://emacs-fu.blogspot.com/2009/11/copying-lines-without-selecting-them.html
;; (defun copy-line()
;;   (interactive)
;;   (message "Copied line")
;;   (copy-region-as-kill (line-beginning-position) (line-end-position)))

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
    Ease of use features:
    - Move to start of next line.
    - Appends the copy on sequential calls.
    - Use newline as last char even on the last line of the buffer.
    - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
	(end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
	  (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
	(setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
	(kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun copy-line-no-whitespace()
  (interactive)
  (message "Copied line (without whitespace)")
  (setq x (point))
  (back-to-indentation)
  (copy-region-as-kill (point) (line-end-position))
  (goto-char x))

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

;; Create new eshell buffer
(defun new-eshell ()
  (interactive)
  (setq current-prefix-arg '(4)) ; C-u
  (call-interactively 'eshell))

;; Load bash aliases into eshell
(defun eshell-load-bash-aliases ()
  "Read Bash aliases and add them to the list of eshell aliases."
  ;; Bash needs to be run - temporarily - interactively
  ;; in order to get the list of aliases.
  (with-temp-buffer
    (call-process "bash" nil '(t nil) nil "-ci" "alias")
    (goto-char (point-min))
    (while (re-search-forward "alias \\(.+\\)='\\(.+\\)'$" nil t)
      (eshell/alias (match-string 1) (match-string 2)))))
;; We only want Bash aliases to be loaded when Eshell loads its own aliases,
;; rather than every time `eshell-mode' is enabled.
(add-hook 'eshell-alias-load-hook 'eshell-load-bash-aliases)

;; Toggle between line and char mode in term
(defun term-toggle-mode ()
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

(defun bs/yank ()
  "Call yank, then auto-indent the yanked region"
  (interactive)
  (let ((point-before (point)))
    (yank)
    (indent-region point-before (point))))

;; Source: https://www.emacswiki.org/emacs/misc-cmds.el
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

;;
;; Custom keybindings
;;

;; Load init file
(global-set-key (kbd "C-c r") 'load-user-init-file)

(global-set-key (kbd "C-c l") 'revert-buffer-no-confirm)

;; bs/ansi-term
(global-set-key (kbd "C-x a") 'bs/ansi-term)

;; New eshell
(global-set-key (kbd "C-c e") 'new-eshell)

;; Copy line
(global-set-key (kbd "C-x w") 'copy-line)
(global-set-key (kbd "C-x W") 'copy-line-no-whitespace)

(global-set-key (kbd "C-x G") 'magit-clone)

;; Remove C-<tab> keybinding from magit so it doesn't conflict with tab-bar-switch-to-next-tab
(add-hook 'magit-mode-hook
	  (lambda ()
	    (local-unset-key (kbd "C-<tab>"))))

;; Open magit diffs in other window
(define-key magit-hunk-section-map (kbd "RET") 'magit-diff-visit-file-other-window)
(define-key magit-file-section-map (kbd "RET") 'magit-diff-visit-file-other-window)

;; Easier keybinds for term mode
(add-hook 'term-mode-hook
	  (lambda ()
	    (define-key term-raw-map (kbd "M-j") 'term-line-mode)
	    (define-key term-mode-map (kbd "M-k") 'term-char-mode)))

;; Toggle between line and char mode in term
(add-hook 'term-mode-hook
	  (lambda ()
	    (define-key term-raw-map (kbd "M-n") 'term-toggle-mode)
	    (define-key term-mode-map (kbd "M-n") 'term-toggle-mode)))

;; Rebind zap to char
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-S-z") 'zap-to-char)

;; Copy to char
(global-set-key (kbd "C-z") 'copy-up-to-char)
(global-set-key (kbd "C-S-z") 'copy-to-char)

;; rgrep
(global-set-key (kbd "C-c f") 'rgrep)

;; Better ansi-term keybinding
(global-set-key (kbd "C-c y") 'term-paste)

;; Custom yank
(global-set-key (kbd "C-S-y") 'bs/yank)

;; windmove keymap
(defvar windmove-leader-map (make-sparse-keymap)
  "Prefix binding for windmove functions")
(global-set-key (kbd "C-c 1") windmove-leader-map)

(define-key windmove-leader-map (kbd "f") 'windmove-right)
(define-key windmove-leader-map (kbd "b") 'windmove-left)
(define-key windmove-leader-map (kbd "p") 'windmove-up)
(define-key windmove-leader-map (kbd "n") 'windmove-down)

(define-key windmove-leader-map (kbd "F") 'windmove-swap-states-right)
(define-key windmove-leader-map (kbd "B") 'windmove-swap-states-left)
(define-key windmove-leader-map (kbd "P") 'windmove-swap-states-up)
(define-key windmove-leader-map (kbd "N") 'windmove-swap-states-down)

;; Ideas
;; In dired pressing 1, 2, or 3 expands dirs using dired-subtree
;; 1 expand dirs depth 1, 2 expand dirs depth2
;; kinda like magit 1, 2, or 3
