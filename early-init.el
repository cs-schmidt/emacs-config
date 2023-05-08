;;; early-init.el --- Early Init File -*- lexical-binding: t -*-
;;
;; With Emacs version 27 or higher this file will be loaded before processing
;; the init.el file.

;; Prevents package.el from being initialized (straight.el is being used as the
;; alternative in this case).
(setq package-enable-at-startup nil)
