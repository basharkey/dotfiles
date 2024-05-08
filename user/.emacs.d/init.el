(load (concat user-emacs-directory "packages.el"))
(load (concat user-emacs-directory "vars.el"))

(defun scroll-down-center ()
  (interactive)
  (scroll-down)
  (move-to-window-line nil))
    
(defun scroll-up-center ()
  (interactive)
  (scroll-up)
  (move-to-window-line nil))
    
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

(defun bs/eshell-list-history ()
  (interactive)
  (eshell-list-history)
  (other-window 1))

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

;; Source: https://www.emacswiki.org/emacs/misc-cmds.el
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

(defun bs/evil-paste-pop (count)
  "Replace the just-yanked stretch of killed text with a different stretch.
If this command is not run immediatly after a `yank',
`evil-paste-before', `evil-paste-after' or `evil-paste-pop',
then invoke `counsel-yank-pop' instead.
This command uses the same paste command as before, i.e., when
used after `evil-paste-after' the new text is also yanked using
`evil-paste-after', used with the same paste-count argument.

The COUNT argument inserts the COUNTth previous kill.  If COUNT
is negative this is a more recent kill."
  (interactive "p")
  (unless (memq last-command
                '(evil-paste-after
                  evil-paste-before
                  evil-visual-paste))
    (counsel-yank-pop))
  (unless evil-last-paste
    (user-error "Previous paste command used a register"))
  (evil-undo-pop)
  (goto-char (nth 2 evil-last-paste))
  (setq this-command (nth 0 evil-last-paste))
  ;; use temporary kill-ring, so the paste cannot modify it
  (let ((kill-ring (list (current-kill
                          (if (and (> count 0) (nth 5 evil-last-paste))
                              ;; if was visual paste then skip the
                              ;; text that has been replaced
                              (1+ count)
                            count))))
        (kill-ring-yank-pointer kill-ring))
    (when (eq last-command 'evil-visual-paste)
      (let ((evil-no-display t))
        (evil-visual-restore)))
    (funcall (nth 0 evil-last-paste) (nth 1 evil-last-paste))
    ;; if this was a visual paste, then mark the last paste as NOT
    ;; being the first visual paste
    (when (eq last-command 'evil-visual-paste)
      (setcdr (nthcdr 4 evil-last-paste) nil))))

;;
;; Custom keybindings
;;

(global-set-key (kbd "M-y") 'bs/evil-paste-pop)

(global-set-key (kbd "C-c r") 'load-user-init-file)
(global-set-key (kbd "C-c l") 'revert-buffer-no-confirm)
(global-set-key (kbd "C-c e") 'new-eshell)
(global-set-key (kbd "C-c f") 'rgrep)
(global-set-key (kbd "C-c y") 'term-paste)

(global-set-key (kbd "C-x a") 'bs/ansi-term)
(global-set-key (kbd "C-x w") 'copy-line)
(global-set-key (kbd "C-x G") 'magit-clone)

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-S-z") 'zap-to-char)

(global-set-key (kbd "C-z") 'copy-up-to-char)
(global-set-key (kbd "C-S-z") 'copy-to-char)

(global-set-key (kbd "<prior>") 'scroll-down-center)
(global-set-key (kbd "<next>") 'scroll-up-center)

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

;; Open magit diffs in other window
(define-key magit-hunk-section-map (kbd "RET") 'magit-diff-visit-file-other-window)
(define-key magit-file-section-map (kbd "RET") 'magit-diff-visit-file-other-window)

;; Toggle between line and char mode in term
(add-hook 'term-mode-hook
	  (lambda ()
	    (define-key term-raw-map (kbd "M-k") 'term-toggle-mode)
	    (define-key term-mode-map (kbd "M-k") 'term-toggle-mode)))

(add-hook 'eshell-mode-hook
	  (lambda ()
	    (define-key eshell-hist-mode-map (kbd "C-c C-l") 'bs/eshell-list-history)))

;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
;; (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
;; (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

;; Ideas
;; In dired pressing 1, 2, or 3 expands dirs using dired-subtree
;; 1 expand dirs depth 1, 2 expand dirs depth2
;; kinda like magit 1, 2, or 3
