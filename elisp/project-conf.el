;;; project-conf.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; doc link: https://docs.projectile.mx/projectile/
;;; Code:

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package treemacs
  :ensure t)

(use-package treemacs-projectile
  :ensure t)

(provide 'project-conf)

;;; project-conf.el ends here
