(package-initialize)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))

(use-package yaml-mode
  :ensure t)
(require 'yaml-mode)
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.yml\\'" . yaml-mode)
                ("\\.yaml\\'" . yaml-mode))))

(global-set-key (kbd "C-}") 'forward-paragraph)
(global-set-key (kbd "C-{") 'backward-paragraph)

(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "C->") 'end-of-buffer)
