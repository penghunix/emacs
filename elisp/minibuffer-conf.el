;;; minibuffer-conf.el --- Config for minibuffer completion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode t))

(use-package marginalia
  :ensure t
  :diminish
  :config
  (marginalia-mode t))

(use-package consult
  :ensure t
  :config
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
  (global-set-key [remap goto-line] 'consult-goto-line))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'minibuffer-conf)

;;; minibuffer-conf.el ends here
