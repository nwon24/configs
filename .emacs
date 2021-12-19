;; ~/.emacs.d/init.el

;; Basics
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Show trailing whitespace and set a keybinding to delete it
(setq-default show-trailing-whitespace t)
(global-set-key (kbd "C-c d t") 'delete-trailing-whitespace)

;; Remove all alarms/bells
(setq ring-bell-funciton 'ignore)

;; Relative line numbers if supported
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode)
  (setq display-line-numbers 'relative))

;; Start server if not already started
(require 'server)
(when (not server-name)
  (server-start))

;; A few useful packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package zenburn-theme
  :ensure t)
(load-theme 'zenburn t)
(use-package magit
  :ensure t)

(setq user-init-file "~/.emacs.d/init.el")
(defun my/set-init-file (name)
  (interactive)
  (setq user-init-file name))
(defun my/reload-init-file ()
  (interactive)
  (load-file user-init-file))
(global-set-key (kbd "C-c r") 'my/reload-init-file)

;; Stop Emacs littering directories with autosave files
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Uncomment desired font
(add-to-list 'default-frame-alist '(font . "Iosevka Fixed SS01-14"))
(set-face-attribute 'default nil :font "Iosevka Fixed SS01-14")

;;(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-14"))
;;(set-face-attribute 'default nil :font "Ubuntu Mono-14")

;;(add-to-list 'default-frame-alist '(font . "Inconsolata-14"))
;;(set-face-attribute 'default nil :font "Inconsolata-14")

;; Programming langauge stuff
(defun my-c-mode-hook ()
  (setq c-basic-offset 8))
(add-hook 'c-mode-hook 'my-c-mode-hook)
(defun my-asm-mode-hook ()
  (if (string-equal (file-name-extension (buffer-file-name)) "S")
      (progn
	(setq asm-comment-char ?#)
	(local-unset-key ?\;)
	(local-set-key ?\; 'asm-comment))
    (setq asm-comment-char ?\;)))
(add-hook 'asm-mode-hook 'my-asm-mode-hook)

(setq debug-on-error t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(magit zenburn-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
