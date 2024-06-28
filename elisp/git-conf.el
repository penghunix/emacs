;;; git-conf.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:

(use-package magit
  :ensure t
  :config
  (setq-default magit-diff-refine-hunk 'all)
  ;; Hint: customize `magit-repository-directories' so that you can use C-u M-F12 to
  ;; quickly open magit on any one of your projects.
  (global-set-key [(meta f12)] 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch)
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up))


;;; git-conf.el ends here
