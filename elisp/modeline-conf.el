;;; modeline-conf.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package nerd-icons
  :ensure nil) ;; M-x nerd-icons-install-fonts
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-height 45
        doom-modeline-bar-width 4)
  (doom-modeline-mode 1))

(provide 'modeline-conf)

;;; modeline-conf.el ends here
