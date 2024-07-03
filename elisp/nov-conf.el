;;; nov-conf.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package nov
  :ensure t
  :config
  (setq nov-text-width t)
  (setq visual-fill-column-center-text t)
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(provide 'nov-conf)

;;; nov-conf.el ends here
