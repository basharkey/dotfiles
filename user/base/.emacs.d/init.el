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

(defun tramp-cleanup-all ()
  (interactive)
  (tramp-cleanup-all-buffers)
  (tramp-cleanup-all-connections))

(defun bs/evil-paste-after-from-kill-ring (string)
  (interactive (list (read-from-kill-ring "Yank from kill-ring: ")))
  (evil-set-register ?\" string)
  (undo-boundary)
  (evil-paste-after 1 ?\"))

(defun bs/evil-paste-before-from-kill-ring (string)
  (interactive (list (read-from-kill-ring "Yank from kill-ring: ")))
  (evil-set-register ?\" string)
  (undo-boundary)
  (evil-paste-before 1 ?\"))

;;
;; Custom keybindings
;;

;; Set custom universal-argument keybinding as C-u is used by evil-scroll-up
(global-set-key (kbd "C-c u") 'universal-argument)
(define-key universal-argument-map (kbd "C-c u") 'universal-argument-more)
(define-key universal-argument-map (kbd "C-u") nil)

(global-set-key (kbd "C-c r") 'load-user-init-file)
(global-set-key (kbd "C-c e") 'bs/quick-eshell)
(global-set-key (kbd "C-c f") 'rgrep)

(global-set-key (kbd "C-c o") 'window-swap-states)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c j") 'windmove-down)


(global-set-key (kbd "C-x w") 'copy-line)
(global-set-key (kbd "C-x G") 'magit-clone)

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-S-z") 'zap-to-char)

(global-set-key (kbd "C-z") 'copy-up-to-char)
(global-set-key (kbd "C-S-z") 'copy-to-char)

(global-set-key (kbd "<prior>") 'scroll-down-center)
(global-set-key (kbd "<next>") 'scroll-up-center)

(add-hook 'eshell-mode-hook
	  (lambda ()
	    (define-key eshell-hist-mode-map (kbd "C-c C-l") 'bs/eshell-list-history)))

;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
;; (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
;; (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
