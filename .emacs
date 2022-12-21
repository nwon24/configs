;; ~/.emacs

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq ring-bell-function 'ignore)

(setq inhibit-splash-screen t)

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

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

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

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

(use-package magit)

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package forth-mode)
(use-package markdown-mode)

(defun my-c-mode-hook ()
  (setq c-basic-offset 4))
(add-hook 'c-mode-hook 'my-c-mode-hook)

(add-to-list 'default-frame-alist '(font . "Iosevka-14" ))
(set-face-attribute 'default t :font "Iosevka-14" )

(setq-default asm-comment-char ?\#)

(global-set-key (kbd "C-c c") 'compile)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(markdown-mode forth-mode projectile expand-region magit smex drag-stuff move-dup move-text easy-kill multiple-cursors zenburn-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)
