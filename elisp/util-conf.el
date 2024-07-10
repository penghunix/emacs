;;; util-conf.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package keycast
  :ensure t
  :config
  (keycast-tab-bar-mode 1))

(use-package command-log-mode
  :ensure t
  :commands global-command-log-mode
  :config
  (setq command-log-mode-auto-show t))

(provide 'util-conf)

;;; util-conf.el ends here
