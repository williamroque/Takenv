;;; org-config.el --- for org-mode config
;;; Commentary:
;; Where lies the org-mode config
;;; Code:

;; prevent confirmation prompt when executing code blocks
(defvar org-confirm-babel-evaluate nil)

;; prevent section indentation
(defvar org-adapt-indentation nil)

;; prevent section numbering in exports
(defvar org-export-with-section-numbers nil)

;; latex formatting
(defvar org-format-latex-options
      '(:foreground default :background default :scale 1.0 :html-foreground "Black" :html-background "Transparent" :html-scale 0.4 :matchers
                    ("begin" "$1" "$" "$$" "\\(" "\\[")))

;; prevent html export from having postamble
(defvar org-html-postamble nil)

;; convenient way to export to HTML in org-mode
(defun export-and-open-html ()
  "Export \"org-mode\" file to HTML and open all HTML files in the directory."
  (interactive)
  (org-html-export-to-html)
  (shell-command "open *.html"))

;; evil-leader bindings
(use-package evil-leader
  :config
  (evil-leader/set-key "h" 'export-and-open-html)

  ;; convenient way to toggle latex preview in org-mode
  (evil-leader/set-key "l" 'org-latex-preview)

  ;; convenient way to execute code block
  (evil-leader/set-key "'" 'org-babel-execute-src-block))

(use-package yasnippet
  :ensure t
  :config

  ;; make sure latex snippets work in org-mode
  (add-hook 'org-mode-hook (lambda ()
                             (yas-minor-mode)
                             (yas-activate-extra-mode 'latex-mode))))

;; remove table of contents from export
(defvar org-export-with-toc nil)

;; load python in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;; change MathJax formatting
(setf org-html-mathjax-options
      '((path "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
        (scale "90") 
        (align "center") 
        (indent "2em")
        (mathml nil)))

;; create org-mode links across files
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)

;; use RET to follow links
(add-to-list 'org-link-frame-setup '(file . find-file))

;; org-mode custom HTML export style
(defun my-org-inline-css-hook (exporter)
  "Insert custom inline css."
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.css"))
           (homestyle (or (null dir) (null (file-exists-p path))))
           (final (if homestyle "~/.org_style.css" path)))
      (setq org-html-head-include-default-style nil)
      (setq org-html-head (concat
                           "<style type=\"text/css\">\n"
                           "<!--/*--><![CDATA[/*><!--*/\n"
                           (with-temp-buffer
                             (insert-file-contents final)
                             (buffer-string))
                           "/*]]>*/-->\n"
                           "</style>\n")))))

(add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)

;; Replace \vert with pipe for tables with latex
(require 'ox)
(defun my-code-filter-replace-pipes (text backend info)
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "\\\\vert" "|" text)))
(add-to-list 'org-export-filter-code-functions 'my-code-filter-replace-pipes)

;; org-download
(use-package org-download
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))

;; toggle truncate lines in org-mode
(define-key org-mode-map "\M-q" 'toggle-truncate-lines)
(toggle-truncate-lines)

(provide 'org-config)
;;; org-config.el ends here
