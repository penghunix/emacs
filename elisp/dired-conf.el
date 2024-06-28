;;; dired-conf.el --- dired configs -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(setq-default dired-dwim-target t)
(setq dired-recursive-deletes 'top)

(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook (lambda()
	    (dired-hide-details-mode t)
	    (all-the-icons-dired-mode t))))

(provide 'dired-conf)

;;; dired-conf.el ends here
