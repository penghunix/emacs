;;; search-conf.el --- Settings for grep and grep-like tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(use-package wgrep
  :ensure t
  :config
  (define-key grep-mode-map (kbd "C-c C-q") 'wgrep-change-to-wgrep-mode)
  (define-key grep-mode-map (kbd "w") 'wgrep-change-to-wgrep-mode))

(use-package recentf
  :ensure t
  :config
  (setq recentf-max-saved-items 200)
  (setq recentf-filename-handlers
		(append '(abbreviate-file-name) recentf-filename-handlers))
  (recentf-mode))
;;; search-conf.el ends here
