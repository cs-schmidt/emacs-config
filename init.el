;;; init.el --- Emacs Init File -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;; This file represents the entry point to my configuration.

;;; Code:

;;;; Startup
;; ----------------------------------------------------------------
(setq inhibit-startup-message t)

;;;; Package Management
;; ----------------------------------------------------------------
;; TODO: Research auto generation of package lock files and VC
;;       tracking thereof.

;; Initialize straight.el for package management.
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

;; Install and activate use-package. For this config use-package will be the
;; primary interface for configuring packages.
(straight-use-package 'use-package)
;; Auto installs packages in use-package forms.
(setq straight-use-package-by-default t)

;;;; Emacs
;; ----------------------------------------------------------------
;; Leverage general to manage keybindings.
(use-package general
  :config
  ;; Define stardard leader key.
  (general-create-definer zzz/leader-keys
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC")
  ;; Setup main bindings.
  (zzz/leader-keys
    "w" '(:keymap evil-window-map :package evil :which-key "window prefix")))

;; TODO: Finish configuring helpful.
(use-package helpful
  :bind
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-function))

;;;; Completions
;; ---------------------------------------------------------------
;; TODO: Finish setting up completion system.

(use-package vertico
  :bind (:map vertico-map
         ("C-j" . #'vertico-next)
         ("C-k" . #'vertico-previous)
	 ("C-f" . #'vertico-exit)
	 :map minibuffer-local-map
	 ("M-h" . #'backward-kill-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

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
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
			   marginalia-annotators-light
			   nil))
  :init
  (marginalia-mode))

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

;;;; Editing
;; ----------------------------------------------------------------
;; TODO: Add tab support with centaur-tabs.
;; TODO: Add ace-window for additional window management options.
;; TODO: Add hydra later.

;; Setup vim emulation in Emacs.
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-shift-width 2)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package projectile
  :diminish projectile-mode
  :general
  (zzz/leader-keys
    "p" '(:keymap projectile-command-map :which-key "projectile prefix"))
  :config
  (projectile-mode)
  :init
  (setq projectile-project-search-path '(("~/Work/" . 2)
                                         "~/Study/personal-knowledge-management/"))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package treemacs)
(use-package treemacs-evil
  :after (treemacs evil))

(use-package hl-todo
  :init
  (global-hl-todo-mode 1)
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces '(("TODO" warning bold)
                           ("FIXME" error bold)
                           ("BUG" error bold)
                           ("REVIEW" font-lock-keyword-face bold)
                           ("HACK" font-lock-constant-face bold)
                           ("DEPRECATED" font-lock-doc-face bold)
                           ("NOTE" success bold)
                           ("XXX" font-lock-constant-face bold))))

;; Configure `fill-column' and show the fill column indicator in file editing
;; major modes.
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Set spaces to be used exclusively for indentation.
(setq-default indent-tabs-mode nil)

;;;; Lang
;; ----------------------------------------------------------------
(use-package flycheck
  :init (global-flycheck-mode))

;;;; Org Mode
;; ----------------------------------------------------------------
;; NOTE: Encountered an issue where `org--inhibit-version-check' was
;;       not being found. I think it happens whenever a package that
;;       depends on org is loaded before org.
;; NOTE: See evil-org package for evil-like bindings in org mode.

(use-package org
  :custom
  (org-startup-folded t)
  (org-startup-with-latex-preview t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-fontify-quote-and-verse-blocks t))

(use-package org-appear
  :after org
  :config
  (add-hook 'org-mode-hook #'org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-inside-latex t))

(use-package org-modern
  :init
  (global-org-modern-mode)
  :custom
  (org-modern-star '("​" "​" "​" "◈")) ; sets headline stars to empty space
  (org-modern-list '((?- . "•")
                     (?+ . "➤")))
  (org-modern-tag nil)
  (org-modern-table nil)
  (org-modern-horizontal-rule t))

;;;; Org Roam
;; ----------------------------------------------------------------
;; TODO: Setup keymap and bindings for useful org-roam commands.
;; TODO: Lock-in usage of org-roam v2.
(use-package org-roam
  :after org
  :config
  (org-roam-db-autosync-mode)
  :custom
  (org-roam-directory (file-truename "~/Study/personal-knowledge-management/"))
  ;; Sets display formatting for org-roam nodes in minibuffer.
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:60}" 'face 'org-tag)))
  ;; Sets templates for new org-roam node entries.
  (org-roam-capture-templates '(("d" "default template" plain "%?" :target (file+head "${slug}_%<%Y%m%d%H%M%S>.org" "#+title: ${title}\n#+filetags: <subject> 📝 🌰\n") :unnarrowed t))))

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-drill
  :after org-roam)

;; Org-drill review function.
(defun zzz/org-drill ()
  "Initiates org-drill review sessions on my org-roam database."
  ;; Interactive.
  ;; Selects from available org mode tags in database.
  (interactive)
  (if (featurep 'org-roam)
      (with-eval-after-load 'org-roam
        (let* ((topics (flatten-tree (org-roam-db-query [:select :distinct tag
                                                         :from tags])))
               (drill (completing-read "Select drill topic: " topics))
               (file-paths (flatten-tree (org-roam-db-query [:select file
                                                             :from nodes
                                                             :inner-join tags
                                                             :on (= nodes:id tags:node-id)
                                                             :where (= tags:tag $s1)]
                                                            drill))))
          (org-drill file-paths)))
    (message "org-roam is not loaded")))

;;;; UI Settings
;; ----------------------------------------------------------------
;; General UI decluttering options.
(menu-bar-mode -1)   ; Removes menu bar.
(tool-bar-mode -1)   ; Removes tool bar.
(tooltip-mode -1)    ; Shows tool tips in echo area only.
(scroll-bar-mode -1) ; Removes scroll bar from buffers.
(set-fringe-mode 10) ; Sets fringe widths.

;; Setup line numbering in buffers.
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

;; Setup mode-line.
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-mode-line-height 24))

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

;;;; Styling
;; ----------------------------------------------------------------
;; Set default font.
(set-face-attribute 'default nil :font "Source Code Pro" :height 100)

;; Set default theme.
(use-package doom-themes
  :config
  (load-theme 'doom-acario-dark t)
  ; Replaces audio feedback for "warning signals" with mode-line flash.
  (doom-themes-visual-bell-config))

(use-package rainbow-delimiters
  :hook '(prog-mode . rainbow-delimiters-mode))

;; The `default-frame-alist' variable is a list of frame parameters
;; which control the default appearance and behaviour of all frames in
;; Emacs. By changing the value of the `alpha' parameter we can give
;; each frame a transparency effect.
(add-to-list 'default-frame-alist '(alpha 95 95))

;;; init.el ends here
