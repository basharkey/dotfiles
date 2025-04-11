;; Force load term library to initialize term.el vars
(require 'term)
(setq shell-file-name "bash")

(defun bs/generate-new-buffer-name (name)
  "Return a string that is the name of no existing buffer
based on ‘NAME<NUMBER>’ (with NUMBER starting at 1).
If there is no live buffer named ‘NAME<1>’, then return ‘NAME<1>’.
Otherwise modify ‘NAME<NUMBER>’ by incrementing NUMBER
until an unused name is found, and then return that name."
  (let ((num 1))
    (while (get-buffer (concat "*" name (number-to-string num) "*"))
      (setq num (+ num 1)))
    (concat "*" name (number-to-string num) "*")))

(defun bs/ansi-term (program &optional new-buffer-name)
  "Start a terminal-emulator in a new buffer.
This is almost the same as `term' apart from always creating a new buffer,
and `C-x' being marked as a `term-escape-char'."
  (interactive (list (read-from-minibuffer "Run program: "
					   (or explicit-shell-file-name
					       (getenv "ESHELL")
					       shell-file-name))))

  ;; Pick the name of the new buffer.
  (setq term-ansi-buffer-name
	(if new-buffer-name
	    new-buffer-name
	  (if term-ansi-buffer-base-name
	      (if (eq term-ansi-buffer-base-name t)
		  (file-name-nondirectory program)
		term-ansi-buffer-base-name)
	    "at")))

  (setq term-ansi-buffer-name (bs/generate-new-buffer-name term-ansi-buffer-name))
  (setq term-ansi-buffer-name (term-ansi-make-term term-ansi-buffer-name program))

  (set-buffer term-ansi-buffer-name)
  (term-mode)
  (term-char-mode)

  ;; Historical baggage.  A call to term-set-escape-char used to not
  ;; undo any previous call to t-s-e-c.  Because of this, ansi-term
  ;; ended up with both C-x and C-c as escape chars.  Who knows what
  ;; the original intention was, but people could have become used to
  ;; either.   (Bug#12842)
  (let (term-escape-char)
    ;; I wanna have find-file on C-x C-f -mm
    ;; your mileage may definitely vary, maybe it's better to put this in your
    ;; .emacs ...
    (term-set-escape-char ?\C-x))

  (switch-to-buffer term-ansi-buffer-name))

(defun bs/ansi-term-no-select (program &optional new-buffer-name)
  "Like `bs/ansi-term' but return the term buffer as value, do not select it."
  (interactive (list (read-from-minibuffer "Run program: "
					   (or explicit-shell-file-name
					       (getenv "ESHELL")
					       shell-file-name))))

  ;; Pick the name of the new buffer.
  (setq term-ansi-buffer-name
	(if new-buffer-name
	    new-buffer-name
	  (if term-ansi-buffer-base-name
	      (if (eq term-ansi-buffer-base-name t)
		  (file-name-nondirectory program)
		term-ansi-buffer-base-name)
	    "at")))

  (setq term-ansi-buffer-name (bs/generate-new-buffer-name term-ansi-buffer-name))
  (setq term-ansi-buffer-name (term-ansi-make-term term-ansi-buffer-name program))

  (set-buffer term-ansi-buffer-name)
  (term-mode)
  (term-char-mode)

  ;; Historical baggage.  A call to term-set-escape-char used to not
  ;; undo any previous call to t-s-e-c.  Because of this, ansi-term
  ;; ended up with both C-x and C-c as escape chars.  Who knows what
  ;; the original intention was, but people could have become used to
  ;; either.   (Bug#12842)
  (let (term-escape-char)
    ;; I wanna have find-file on C-x C-f -mm
    ;; your mileage may definitely vary, maybe it's better to put this in your
    ;; .emacs ...
    (term-set-escape-char ?\C-x))

  term-ansi-buffer-name)

(defun bs/term-mode-toggle ()
  "Toggle term between line and char mode"
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

(defun bs/quick-ansi-term ()
  "Create a new ansi-term buffer without prompting,
using the current value of `shell-file-name' as run program."
  (interactive)
  (bs/ansi-term shell-file-name))

(defun bs/ansi-term-other-window ()
  (interactive)
  (switch-to-buffer-other-window (bs/ansi-term-no-select shell-file-name)))

(defun bs/term-yank-from-kill-ring (string)
  (interactive (list (read-from-kill-ring "Yank from kill-ring: ")))
  (term-send-raw-string string))

(defun bs/kill-all-ansi-term-buffers ()
  (interactive)
  (kill-matching-buffers "\\*at[0-9]+\\*" nil t))


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
