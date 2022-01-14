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
(use-package gruber-darker-theme
  :ensure t)
(load-theme 'zenburn t)
;;(load-theme 'gruber-darker t)
(use-package magit
  :ensure t)
(use-package nasm-mode
  :ensure t)
(use-package multiple-cursors
  :ensure t)
(use-package projectile
  :ensure t)
(use-package ivy
  :ensure t)
(ivy-mode)
(use-package neotree
  :ensure t)
(global-set-key (kbd "C-c t") 'neotree-toggle)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'ivy)
(projectile-mode +1)

(defun my/set-init-file (name)
  "Set name of Emacs init file"
  (interactive)
  (setq user-init-file name))
(defun my/reload-init-file ()
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-c r") 'my/reload-init-file)

(global-set-key (kbd "C-c c") 'compile)

;; Stop Emacs littering directories with autosave files
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Uncomment desired font
(add-to-list 'default-frame-alist '(font . "Iosevka Term SS01-14"))
(set-face-attribute 'default nil :font "Iosevka Term SS01-14")

;;(add-to-list 'default-frame-alist '(font . "Liberation Mono-14"))
;;(set-face-attribute 'default nil :font "Liberation Mono-14")

;;(add-to-list 'default-frame-alist '(font . "Fira Code-12"))
;;(set-face-attribute 'default nil :font "Fira Code-12")

;;(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-14"))
;;(set-face-attribute 'default nil :font "Ubuntu Mono-14")

;;(add-to-list 'default-frame-alist '(font . "Inconsolata-14"))
;;(set-face-attribute 'default nil :font "Inconsolata-14")

;; Programming langauge stuff
(defun my-c-mode-hook ()
  (setq c-basic-offset 8))
(add-hook 'c-mode-hook 'my-c-mode-hook)

(setq debug-on-error t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("03e26cd42c3225e6376d7808c946f7bed6382d795618a82c8f3838cd2097a9cc" "3b8284e207ff93dfc5e5ada8b7b00a3305351a3fb222782d8033a400a48eca48" default))
 '(frame-brackground-mode 'dark)
 '(package-selected-packages
   '(neotree emacs-neotree projectile multiple-cursors fasm-mode magit zenburn-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
