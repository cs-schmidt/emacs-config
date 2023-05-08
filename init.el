;;; init.el --- Emacs Init File -*- lexical-binding: t; -*-

;;;; Package Management
;; ----------------------------------------------------------------
;; TODO: Research auto generation of package lock files and VC tracking thereof 
;; Initializes straight.el for package management.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Makes `use-package' forms use straight.el to install packages by
;; default.
(setq straight-use-package-by-default t)
 
;; Install and setup `use-package'.
(straight-use-package 'use-package)

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
;(setq visible-bell t) ; Replaces audio feedback for "warning signals" with visual feedback.
(column-number-mode)
(global-display-line-numbers-mode t)
;; TODO: disable line numbers in terminal modes.
(dolist (mode '(term-mode-hook shell-mode-hook eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-mode-line-height 15)))

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

;;;; Styling
;; ----------------------------------------------------------------
;; Sets default font.
(set-face-attribute 'default nil :font "Source Code Pro" :height 100)

;; Sets default theme.
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-acario-dark t)
  (doom-themes-visual-bell-config))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;;;; Completions
;; ---------------------------------------------------------------
(use-package vertico
  :straight t
  :bind (:map vertico-map
         ("C-j" . #'vertico-next)
         ("C-k" . #'vertico-previous)
	 ("C-f" . #'vertico-exit)
	 :map minibuffer-local-map
	 ("M-h" . #'backward-kill-word))
  :custom (vertico-cycle t)
  :init (vertico-mode))

(use-package savehist
  :init (savehist-mode))

(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :after vertico
  :straight t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
			   marginalia-annotators-light
			   nil))
  :init (marginalia-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;;;; Emacs
;; ----------------------------------------------------------------
;; TODO: Finish configuring helpful.
(use-package helpful
  :straight t
  :bind
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-function))

;;;; Editing
;; ----------------------------------------------------------------
;; Configure `fill-column' and display the fill column indicator in
;; file editing major modes.
(setq fill-column 80)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
