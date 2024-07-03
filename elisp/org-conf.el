;;; org-conf.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq org-log-done t
      org-edit-timestamp-down-means-later t
      org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)

;; Lots of stuff from http://doc.norang.ca/org-mode.html

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   (seq-filter
    (lambda (pair)
      (locate-library (concat "ob-" (symbol-name (car pair)))))
    '((R . t)
      (dot . t)
      (gnuplot . t)
      (latex . t)
      (python . t)
      (shell . t)
      (sql . t)
      (sqlite . t)))))

(provide 'org-conf)

;;; org-conf.el ends here
