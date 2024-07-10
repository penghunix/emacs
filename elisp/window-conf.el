;;; window-conf.el --- Working with windows within frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package winner
  :ensure t
  :config
  (winner-mode t))

(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'sanityinc/toggle-delete-other-windows)

(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'split-window-vertically-instead)

(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'sanityinc/split-window)



(use-package tab-bar
  :ensure nil
  :bind (("C-<f4>" . tab-bar-close-tab))
  ;; :bind (("s-[" . tab-bar-switch-to-prev-tab)
  ;;        ("s-]" . tab-bar-switch-to-next-tab)
  ;;        ("s-{" . (lambda ()
  ;;                   (interactive)
  ;;                   (tab-move -1)))
  ;;        ("s-}" . (lambda ()
  ;;                   (interactive)
  ;;                   (tab-move 1))))
  :custom
  (tab-bar-show t)
  (tab-bar-close-button-show nil)
  (tab-bar-auto-width nil)
  (tab-bar-format '(tab-bar-format-menu-bar
                    ;;dw/exwm-workspace-icon
                    tab-bar-format-tabs-groups
                    tab-bar-separator
                    ;;dw/tmr-mode-line
                    tab-bar-separator
                    tab-bar-format-align-right
                    tab-bar-format-global))
  ;; Like winner-mode for tabs
  (tab-bar-history-mode 1)
  (tab-bar-mode 1))


(provide 'window-conf)

;;; window-conf.el ends here
