;; ~/.emacs

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; (add-to-list 'load-path "~/.emacs.d/lib")
;; (load-library "cweb")

(setq ring-bell-function 'ignore)

(setq inhibit-splash-screen t)

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(setq org-agenda-files (list "~/org"))

(setq dired-dwim-target t)

(when (version<= "26.0.50" emacs-version)
  (setq display-line-numbers-type 'relative)
  (global-display-line-numbers-mode))

(setq ido-enable-flex-matching t)
(setq ido-everywhere 1)
(ido-mode 1)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package gruber-darker-theme)
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(use-package tex
  :ensure auctex
  :init
  (setq-default TeX-engine 'luatex))

(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c M-x" . execute-extended-command)))

(use-package projectile
  :config (projectile-mode +1)
  :bind (("C-c p" . projectile-command-map)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this-word)
	 ("C-<" . mc/mark-previous-like-this-word)
	 ("C-M->" . mc/mark-next-like-this-symbol)
	 ("C-M-<" . mc/mark-previous-like-this-symbol)
	 ("C-c C-<" . mc/mark-all-like-this)))

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
	 ([remap mark-sexp] . easy-mark)))

(use-package easy-kill-extras
  :bind (("M-@" . easy-mark-word)
	 ("C-M-@" . easy-mark-sexp)
	 ([remap zap-to-char] . 'easy-mark-to-char)))

(use-package move-text
  :bind (("M-p" . move-text-up)
	 ("M-n" . move-text-down)))

(use-package move-dup
  :bind (("C-M-p" . move-dup-duplicate-up)
	 ("C-M-n" . move-dup-duplicate-down)))

(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  :bind (("C-c f" . drag-stuff-right)
	 ("C-c b" . drag-stuff-left)))

(use-package key-chord
  :config
  (key-chord-mode 1))

(use-package avy
  :bind
  (("C-;" . avy-goto-char)
   ("C-'" . avy-goto-char-2)
   ("M-C-;" . avy-goto-word-0)
   ("M-C-'" . avy-goto-word-1)))

(use-package company
  :config
  (global-company-mode 1)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package magit)

(use-package expand-region
  :config
  (pending-delete-mode t)
  :bind (("C-=" . er/expand-region)))

(use-package yasnippet
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets)

(use-package forth-mode)
(use-package markdown-mode)

(use-package ace-jump-mode
  :bind (("C-c SPC" . ace-jump-mode)))

(defun my-c-mode-hook ()
  (setq c-basic-offset 4))
(add-hook 'c-mode-hook 'my-c-mode-hook)

(add-to-list 'default-frame-alist '(font . "JetBrains Mono-16" ))
(set-face-attribute 'default t :font "JetBrains Mono-16" )
(set-frame-font "JetBrains Mono-16" nil t)

(setq-default asm-comment-char ?\#)

(global-set-key (kbd "C-c c") 'compile)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" "bddf21b7face8adffc42c32a8223c3cc83b5c1bbd4ce49a5743ce528ca4da2b6" default))
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(auctex gruber-darker-theme yasnippet-snippets avy company iy-go-to-char key-chord ace-jump-mode easy-kill-extras markdown-mode forth-mode projectile expand-region magit smex drag-stuff move-dup move-text easy-kill multiple-cursors zenburn-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)
(put 'magit-clean 'disabled nil)
