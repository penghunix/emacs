;;; reading-conf.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'justify-kp)

(defun my-nov-window-configuration-change-hook ()
  (my-nov-post-html-render-hook)
  (remove-hook 'window-configuration-change-hook
               'my-nov-window-configuration-change-hook
               t))

(defun my-nov-post-html-render-hook ()
  (if (get-buffer-window)
      (let ((max-width (pj-line-width))
            buffer-read-only)
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (not (looking-at "^[[:space:]]*$"))
              (goto-char (line-end-position))
              (when (> (shr-pixel-column) max-width)
                (goto-char (line-beginning-position))
                (pj-justify)))
            (forward-line 1))))
    (add-hook 'window-configuration-change-hook
              'my-nov-window-configuration-change-hook
              nil t)))

(defun nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "JetbrainsMono Nerd Font"
                                           :height 1.0))

(use-package nov
  :ensure t
  :config
  (setq nov-text-width t)
  (setq visual-fill-column-center-text t)
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)
  (add-hook 'nov-mode-hook 'nov-font-setup)
  ;; (add-hook 'nov-post-html-render-hook 'my-nov-post-html-render-hook)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(provide 'reading-conf)

;;; reading-conf.el ends here
