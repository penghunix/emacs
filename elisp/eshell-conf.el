;;; eshell-conf.el --- eshell configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(eval-after-load 'eshell
  '(require 'eshell-autojump nil t))

(setq eshell-last-dir-ring-size 500)

;; (use-package eshell-git-prompt
;;   :after eshell)

;; (eshell-git-prompt-use-theme 'simple)

(defun eshell-new ()
  "Open a new insctance of eshell."
  (interactive)
  (eshell 'N))

(setenv "SHELL" "/usr/bin/zsh")
(setq explicit-shell-file-name "/usr/bin/zsh")

(setq my/eshell-aliases
      '((d  . dired)
	(ff . find-file)
	(ffo . find-file-other-window)
	(l  . (lambda () (eshell/ls '-hl)))
	(ll  . (lambda () (eshell/ls '-ahl)))
	(eshell/clear . eshell/clear-scrollback)))

(mapc (lambda (alias)
	(defalias (car alias) (cdr alias)))
      my/eshell-aliases)

(setq eshell-prompt-regexp "^[^#$\n]*[#$] "
      eshell-prompt-function
      (lambda nil
	(concat
	 "[" (user-login-name) "@" (system-name) " "
	 (if (string-equal-ignore-case (eshell/pwd) (getenv "HOME"))
	     "~" (eshell/basename (eshell/pwd)))
	 "]"
	 (if (= (user-uid) 0) "# " "$ "))))

(add-hook 'eshell-mode-hook (lambda ()
                              (display-line-numbers-mode -1)
                              (hl-line-mode -1)))

(provide 'eshell-conf)

;;; eshell-conf.el ends here
