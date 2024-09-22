(load (concat user-emacs-directory "packages.el"))
(load (concat user-emacs-directory "vars.el"))

(load (concat user-emacs-directory "ansi-term.el"))

(defun load-user-init-file()
  "Evaluate `user-init-file'"
  (interactive)
  (load-file user-init-file))

;; Create new eshell buffer
(defun bs/quick-eshell ()
  "Always create a new Eshell buffer"
  (interactive)
  (let ((eshell-buffer-name (bs/generate-new-buffer-name "*eshell*")))
    (setq current-prefix-arg '(4)) ; C-u
    (call-interactively 'eshell)))

(defun bs/eshell-list-history ()
  (interactive)
  (eshell-list-history)
  (other-window 1))

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

(defun url-encode-region (beg end)
  "URL encode the region between `BEG' and `END'"
  (interactive (progn
		 (let ((beg (mark))
		       (end (point)))
                   (unless (and beg end)
                     (user-error "The mark is not set, so there is no region"))
		   (list beg end))))
  (kill-region beg end)
  (insert (url-hexify-string (current-kill 0))))

;;
;; Custom keybindings
;;

;; Set custom universal-argument keybinding as C-u is used by evil-scroll-up
(global-set-key (kbd "C-c u") 'universal-argument)
(define-key universal-argument-map (kbd "C-c u") 'universal-argument-more)
(define-key universal-argument-map (kbd "C-u") nil)

(global-set-key (kbd "M-y") 'bs/evil-paste-pop)

(global-set-key (kbd "C-c r") 'load-user-init-file)
(global-set-key (kbd "C-c l") 'revert-buffer-no-confirm)
(global-set-key (kbd "C-c e") 'bs/quick-eshell)
(global-set-key (kbd "C-c f") 'rgrep)

(global-set-key (kbd "C-x a") 'bs/quick-ansi-term)
(global-set-key (kbd "C-x 4 a") 'bs/ansi-term-other-window)
(add-hook 'term-mode-hook
	  (lambda ()
	    ;; yank-pop for term
	    (define-key term-raw-map (kbd "M-y") 'bs/term-yank-from-kill-ring)
	    (define-key term-raw-map (kbd "C-c y") 'term-paste)
	    ;; Toggle between line and char mode in term
	    (define-key term-raw-map (kbd "M-k") 'bs/term-mode-toggle)
	    (define-key term-mode-map (kbd "M-k") 'bs/term-mode-toggle)))

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
