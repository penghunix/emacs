;;; editing-conf.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default locale-coding-system 'utf-8-unix
              buffer-file-encoding-system 'utf-8-unix
              coding-system-for-read 'utf-8-unix
              coding-system-for-write 'utf-8-unix
              default-terminal-coding-system 'utf-8-unix
              default-process-coding-system nil
              default-file-name-coding-system 'utf-8-unix
              default-keyboard-coding-system 'utf-8-unix
              default-sendmail-coding-system 'utf-8-unix
              inhibit-startup-message t
              visible-bell nil
              use-file-dialog nil
              use-dialog-box nil
              display-line-numbers-width 7
              version-control t
              delete-old-versions t
              kept-old-versions 5
              kept-new-versions 5
              tab-width 4
              truncate-lines t
              line-move-visual t
              sentence-end-double-space nil
              bookmark-set-fringe-mark nil
              scroll-step 1
              scroll-margin 8
              scroll-conservatively 101
              delete-by-moving-to-trash t
              trash-directory (concat (getenv "HOME") "/.Trash")
              blink-cursor-interval 0.4
              bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
              buffers-menu-max-size 30
              case-fold-search t
              column-number-mode t
              indicate-buffer-boundaries 'left
              display-fill-column-indicator-character ?┊
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              indent-tabs-mode nil
              create-lockfiles nil
              auto-save-default nil
              make-backup-files nil
              backup-directory-alist (list (cons "." (concat user-emacs-directory "backup/")))
              backup-by-copying nil
              mouse-yank-at-point t
              save-interprogram-paste-before-kill t
              scroll-preserve-screen-position 'always
              set-mark-command-repeat-pop t
              tooltip-delay 0.5
              truncate-lines t
              truncate-partial-width-windows nil
              display-line-numbers-width 7)

(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(add-hook 'after-init-hook 'electric-pair-mode)
(add-hook 'after-init-hook 'electric-indent-mode)
(add-hook 'after-init-hook 'global-auto-revert-mode)
(add-hook 'after-init-hook 'delete-selection-mode)
(add-hook 'after-init-hook 'transient-mark-mode)
(add-hook 'after-init-hook 'subword-mode)

(add-hook 'prog-mode-hook (lambda ()
                            (display-line-numbers-mode t)
                            ;; (hl-line-mode nil)
                            (display-fill-column-indicator-mode t)))

(use-package rainbow-mode
  :ensure t
  :diminish 'rainbow-mode
  :config
  (rainbow-mode t))

(use-package rainbow-delimiters
  :ensure t
  :diminish
  :config
  (rainbow-delimiters-mode t))

(use-package move-dup
  :ensure t
  :config
  (global-set-key (kbd "C-c d") 'move-dup-duplicate-down)
  (global-set-key (kbd "C-c u") 'move-dup-duplicate-up))

(use-package which-key
  :ensure t
  :diminish
  :config
  (setq-default which-key-idle-delay 1.0)
  (which-key-mode t))

(use-package browse-kill-ring
  :ensure t
  :config
  (setq browse-kill-ring-separator "\f")
  (global-set-key (kbd "M-Y") 'browse-kill-ring)
  (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous))

(use-package page-break-lines
  :ensure t
  :diminish
  :config
  (add-to-list 'page-break-lines-modes 'browse-kill-ring-mode)
  (global-page-break-lines-mode t))

(use-package repeat
  :ensure t
  :diminish
  :config
  (repeat-mode t))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-+") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package hungry-delete
  :ensure t
  :diminish
  :config
  (global-hungry-delete-mode t))

(use-package expand-region
  :ensure t
  :bind
  ("M-h" . er/expand-region)
  ("M-H" . er/contract-region))

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

(defun kill-curr-buffer ()
	(interactive)
	(kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-curr-buffer)

(defun kill-all-buffers ()
	(interactive)
	(mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-x a k") 'kill-all-buffers)

(defun next-open-line ()
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "S-<return>") 'next-open-line)


(global-unset-key (kbd "S-<SPC>"))
(setq default-input-method "korean-hangul")
;; (global-set-key (kbd "S-<SPC>") 'toggle-input-method)
;; use C-\ instead

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'editing-conf)

;;; editing-conf.el ends here
