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
  (setq display-line-numbers-type 'relative))

;; Start server if not already started
(require 'server)
(when (not server-name)
  (server-start))

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; A few useful packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Some nice themes
(use-package zenburn-theme
  :ensure t)
(use-package gruber-darker-theme
  :ensure t)
(use-package doom-themes
  :ensure t)
(load-theme 'zenburn t)
;;(load-theme 'gruber-darker t)
;;(load-theme 'doom-one t)

;; Other useful packages
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

(defun my/reload-init-file ()
  (interactive)
  (load-file user-init-file))

(defun my/move-line-down (n)
  (interactive "p")
  (while (> n 0)
    (next-line)
    (transpose-lines 1)
    (previous-line)
    (setq n (1- n))))

(defun my/move-line-up (n)
  (interactive "p")
  (while (> n 0)
    (transpose-lines 1)
    (previous-line 2)
    (setq n (1- n))))

(defun my/dup-line (n)
  (interactive "p")
  (while (> n 0)
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (newline)
    (yank)
    (setq n (1- n))))

(global-set-key (kbd "C-c C-n") 'my/move-line-down)
(global-set-key (kbd "C-c C-p") 'my/move-line-up)
(global-set-key (kbd "C-'") 'my/dup-line)
(global-set-key (kbd "C-;") 'kill-whole-line)
(global-set-key (kbd "C-c r") 'my/reload-init-file)
(global-set-key (kbd "C-c c") 'compile)

;; Stop Emacs littering directories with autosave files
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Uncomment desired font
(add-to-list 'default-frame-alist '(font . "Iosevka Term-14"))
(set-face-attribute 'default nil :font "Iosevka Term-14")

;;(add-to-list 'default-frame-alist '(font . "Liberation Mono-14"))
;;(set-face-attribute 'default nil :font "Liberation Mono-14")

;;(add-to-list 'default-frame-alist '(font . "Fira Code-12"))
;;(set-face-attribute 'default nil :font "Fira Code-12")

;;(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-14"))
;;(set-face-attribute 'default nil :font "Ubuntu Mono-14")

;;(add-to-list 'default-frame-alist '(font . "Inconsolata-14"))
;;(set-face-attribute 'default nil :font "Inconsolata-14")

;;(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))
;;(set-face-attribute 'default nil :font "Source Code Pro-14")

;; Programming langauge stuff
(defun my-c-mode-hook ()
  (local-unset-key (kbd "C-c C-p"))
  (local-unset-key (kbd "C-c C-n"))
  (setq c-basic-offset 8))
(add-hook 'c-mode-hook 'my-c-mode-hook)

(setq debug-on-error t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "03e26cd42c3225e6376d7808c946f7bed6382d795618a82c8f3838cd2097a9cc" "3b8284e207ff93dfc5e5ada8b7b00a3305351a3fb222782d8033a400a48eca48" default))
 '(exwm-floating-border-color "#191b20")
 '(frame-brackground-mode 'dark)
 '(highlight-tail-colors
   ((("#333a38" "#99bb66" "green")
     . 0)
    (("#2b3d48" "#46D9FF" "brightcyan")
     . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(objed-cursor-color "#ff6c6b")
 '(package-selected-packages
   '(neotree emacs-neotree projectile multiple-cursors fasm-mode magit zenburn-theme use-package))
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
