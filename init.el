;;; init.el --- Emacs Init File -*- lexical-binding: t; -*-

;;;; Startup
;; ----------------------------------------------------------------
(setq inhibit-startup-message t)

;;;; UI Settings
;; ----------------------------------------------------------------
(menu-bar-mode -1) ; Removes menu bar.
(tool-bar-mode -1) ; Removes tool bar.
(tooltip-mode -1)  ; Shows tool tips in echo area only.
(scroll-bar-mode -1) ; Removes scroll bar from buffers.
(set-fringe-mode 10) ; Sets fringe widths.
(setq visible-bell t) ; Replaces audio feedback for "warning signals" with visual feedback.

;;;; Editing
;; ----------------------------------------------------------------
;; Configure `fill-column' and display the fill column indicator in
;; file editing major modes.
(setq fill-column 80)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;;;; Styling
;; ----------------------------------------------------------------
;; Sets default font.
(set-face-attribute 'default t :font "Source Code Pro" :height 100)
;; Sets default theme.
(load-theme 'tango-dark t)

;;;; Package Management
;; ----------------------------------------------------------------
;; Initializes the package sources.
;; TODO: Check if package.el is already loaded by default with Emacs version.
(require 'package)
(dolist (archive '(("melpa" . "https://melpa.org/packages/")
		   ("melpa-stable" . "https://stable.melpa.org/packages")))
  (add-to-list 'package-archives archive t))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install and setup `use-package'.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
