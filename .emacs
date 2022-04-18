;; ~/.emacs.d/init.el
;; The stuff here changes often because my opinions change quite often.
;; The stuff that is commented out is the stuff I toos around.

;; Basics
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq inhibit-startup-message t)

;; Show trailing whitespace and set a keybinding to delete it
(setq-default show-trailing-whitespace t)
(global-set-key (kbd "C-c d t") 'delete-trailing-whitespace)

;; Remove all alarms/bells
(setq ring-bell-function 'ignore)

;; Global highlighting mode
;;(global-hl-line-mode 1)

;; Relative line numbers if supported
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode)
  (setq display-line-numbers-type 'relative))

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
(setq use-package-always-ensure t)

;; Evil mode is evil
;;(use-package evil
  ;;:init
  ;;(setq evil-want-keybinding nil)
  ;;(evil-mode 1))
;;(use-package evil-collection
  ;;:after evil
  ;;:config
  ;;(evil-collection-init))
;;(use-package evil-escape
  ;;:config
  ;;(setq-default evil-escape-key-sequence "jk")
  ;;(evil-escape-mode 1))

;; Some nice themes
(use-package zenburn-theme)
(use-package gruber-darker-theme)
(use-package doom-themes)
(use-package solarized-theme)
(load-theme 'zenburn t)
;;(load-theme 'gruber-darker t)
;;(load-theme 'doom-one t)

(defun load-solarized (type)
  "Load solarized theme: light or dark"
  (interactive)
  (setq solarized-istinct-fringe-background t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  (if (= type 1)
      (load-theme 'solarized-dark t)
    (load-theme 'solarized-light t)))

;;(load-solarized 1)

;; Other useful packages
(use-package magit)
(use-package nasm-mode)
(use-package multiple-cursors)
(use-package projectile)
(use-package ivy
  :config
  (ivy-mode))
(use-package neotree)
(global-set-key (kbd "C-c t") 'neotree-toggle)

(use-package easy-kill)
(global-set-key [remap kill-ring-save] 'easy-kill)

(use-package haskell-mode)
;; A useful package for expanding selected region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;(use-package helm
;;  :init
;;  (helm-mode))
(use-package company
  :init
  (company-mode 1))

;; Multiple cursors for use with evil mode
;;(use-package evil-multiedit
;;  :ensure t)

;; The original multiple cursors doesn't really work with evil-mode
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

;;(require 'evil-multiedit)
;;(evil-multiedit-default-keybinds)

;; Put this after ivy so that we get ido-switch-buffer instead of ivy-switch-buffer
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

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

(global-set-key (kbd "M-n") 'my/move-line-down)
(global-set-key (kbd "M-p") 'my/move-line-up)
(global-set-key (kbd "C-'") 'my/dup-line)
(global-set-key (kbd "C-;") 'kill-whole-line)
(global-set-key (kbd "C-c r") 'my/reload-init-file)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c v") 'recompile)

;; Stop Emacs littering directories with autosave files
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Uncomment desired font
(add-to-list 'default-frame-alist '(font . "Iosevka-14"))
(set-face-attribute 'default nil :font "Iosevka-14")

;;(add-to-list 'default-frame-alist '(font . "Source Code Pro Medium-12"))
;;(set-face-attribute 'default nil :font "Source Code Pro Medium-12")

;; Programming langauge stuff
(defun my-c-mode-hook ()
  (setq c-basic-offset 8))
(add-hook 'c-mode-hook 'my-c-mode-hook)

;; Org mode stuff
;; When export to LaTeX untabify so that source code blocks are indented properly.
(indent-tabs-mode -1)
(setq org-src-preserve-indentation t)

(setq debug-on-error t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   '("fc48cc3bb3c90f7761adf65858921ba3aedba1b223755b5924398c666e78af8b" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "b77a00d5be78f21e46c80ce450e5821bdc4368abf4ffe2b77c5a66de1b648f10" "3d2e532b010eeb2f5e09c79f0b3a277bfc268ca91a59cdda7ffd056b868a03bc" "835868dcd17131ba8b9619-12c67c127aa18b90a82438c8613586331129dda63" "03e26cd42c3225e6376d7808c946f7bed6382d795618a82c8f3838cd2097a9cc" "3b8284e207ff93dfc5e5ada8b7b00a3305351a3fb222782d8033a400a48eca48" default))
 '(exwm-floating-border-color "#191b20")
 '(fci-rule-color "#383838")
 '(frame-brackground-mode 'dark)
 '(helm-minibuffer-history-key "M-p")
 '(highlight-tail-colors
   ((("#333a38" "#99bb66" "green")
     . 0)
    (("#2b3d48" "#46D9FF" "brightcyan")
     . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(objed-cursor-color "#ff6c6b")
 '(org-agenda-files '("~/org/agenda.org"))
 '(package-selected-packages
   '(expand-region evil-multiedit evil-escape evil-collection evil solarized-theme company helm haskell-mode neotree emacs-neotree projectile multiple-cursors fasm-mode magit zenburn-theme use-package))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
