;;; dired-conf.el --- dired configs -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(setq-default dired-dwim-target t)
(setq dired-recursive-deletes 'top
      dired-listing-switches "-agho --group-directories-first"
      dired-omit-files "^\\.[^.].*"
      dired-omit-verbose nil
      dired-dwim-target 'dired-dwim-target-text
      dired-hide-details-hide-symlink-targets nil
      dired-kill-when-opening-new-dired-buffer t
      delete-by-moving-to-trash t)

(use-package dired-preview
  :ensure t)
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :config)

(add-hook 'dired-mode-hook (lambda()
	                           (dired-hide-details-mode t)
	                           (all-the-icons-dired-mode t)
                             (dired-preview-mode t)))

;; (use-package ranger
;;   :ensure t)

;; (use-package dired-ranger
;;   :ensure t)


(define-key dired-mode-map (kbd "b") 'dired-up-directory)
(define-key dired-mode-map (kbd "H") 'dired-hide-details-mode)

(provide 'dired-conf)

;;; dired-conf.el ends here
