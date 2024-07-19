;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

;; (let ((minver "27.1"))
;;   (when (version< emacs-version minver)
;;     (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
;; (when (version< emacs-version "28.1")
;;   (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
;; (require 'init-benchmarking) ;; Measure startup time

;; (defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
;; (defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection threshold for early startup (see use of gcmh below)
;; (setq gc-cons-threshold (* 128 1024 1024))

;; Process performance tuning
;; (setq read-process-output-max (* 4 1024 1024))
;; (setq process-adaptive-read-buffering nil)

(require 'package)
(require 'cl-lib)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-enable-at-startup nil)
(setq package-native-complie t)
(package-initialize)

;; Bootstrap 'use-package'
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

(use-package diminish
  :ensure t)
(require 'diminish)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

;; customized



(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(global-auto-revert-mode 1)
;; (global-tab-line-mode t)

;; (load-theme 'modus-vivendi)
(set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 140)
;; (set-face-attribute 'region nil :background "#fca62d")

;; to insert a line like below(^L, page break) C-q C-l



(require 'editing-conf)
(require 'theme-conf)
(require 'search-conf)
(require 'eshell-conf)
(require 'minibuffer-conf)
(require 'modeline-conf)
(require 'dired-conf)
(require 'window-conf)
(require 'lsp-conf)
(require 'org-conf)
(require 'reading-conf)
(require 'git-conf)
(require 'project-conf)
(require 'util-conf)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
