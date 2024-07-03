;;; lsp-conf.el --- Settings for grep and grep-like tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :ensure t)



;; elisp
(setq-default initial-scratch-message
              (concat ";; Happy hacking, " user-login-name "!!\n\n"))

(defun penghunix/headerise-elisp ()
  "Add minimal header and footer to an elisp buffer in order to placate flycheck."
  (interactive)
  (let ((fname (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (error "This buffer is not visiting a file"))))
    (save-excursion
      (goto-char (point-min))
      (insert ";;; " fname " --- Insert description here -*- lexical-binding: t -*-\n"
              ";;; Commentary:\n"
              ";;; Code:\n\n")
      (goto-char (point-max))
      (insert ";;; " fname " ends here\n"))))

(defun penghunix/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(global-set-key [remap eval-expression] 'pp-eval-expression)

(defun penghunix/load-this-file ()
  "Load the current file or buffer.
The current directory is temporarily added to `load-path'.  When
there is no current file, eval the current buffer."
  (interactive)
  (let ((load-path (cons default-directory load-path))
        (file (buffer-file-name)))
    (if file
        (progn
          (save-some-buffers nil (apply-partially 'derived-mode-p 'emacs-lisp-mode))
          (load-file (buffer-file-name))
          (message "Loaded %s" file))
      (eval-buffer)
      (message "Evaluated %s" (current-buffer)))))

;; (with-eval-after-load 'lisp-mode
;;   (define-key emacs-lisp-mode-map (kbd "C-c C-l") 'penghunix/load-this-file)
;;   (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'penghunix/eval-last-sexp-or-region)
;;   (define-key emacs-lisp-mode-map (kbd "C-<return>") 'penghunix/eval-last-sexp-or-region))

(define-key emacs-lisp-mode-map (kbd "C-c C-l") 'penghunix/load-this-file)
(define-key emacs-lisp-mode-map (kbd "C-x C-e") 'penghunix/eval-last-sexp-or-region)
(define-key emacs-lisp-mode-map (kbd "C-<return>") 'penghunix/eval-last-sexp-or-region)
(define-key lisp-interaction-mode-map (kbd "C-c C-l") 'penghunix/load-this-file)
(define-key lisp-interaction-mode-map (kbd "C-x C-e") 'penghunix/eval-last-sexp-or-region)
(define-key lisp-interaction-mode-map (kbd "C-<return>") 'penghunix/eval-last-sexp-or-region)



;; html
(use-package tagedit
  :ensure t
  :config
  (tagedit-add-paredit-like-keybindings)
  (define-key tagedit-mode-map (kbd "M-?") nil)
  (define-key tagedit-mode-map (kbd "M-s") nil)
  (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1))))



(provide 'lsp-conf)

;;; lsp-conf.el ends here
