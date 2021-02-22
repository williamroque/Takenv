;;; init.el --- the entry point for our lovely config
;;; Commentary:
;; For optimizations and loading packages.
;;; Code:

;; optimize startup time
(let ((file-name-handler-alist nil))
  ;; optimize startup time
  (setq gc-cons-threshold 100000000)
  (add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

  ;; add MELPA index
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/")
               '("melpa-stable" . "https://stable.melpa.org/packages/"))
  (package-initialize)

  ;; fix executable path
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

  ;; install use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)

  ;; load use-package
  (eval-when-compile
    (require 'use-package))

  (use-package packages
    :load-path "lisp/")
  (use-package evil-config
    :load-path "lisp/")
  (use-package edit
    :load-path "lisp/")
  (use-package ui
    :load-path "lisp/")
  (use-package misc
    :load-path "lisp/")
  (use-package org-config
    :load-path "lisp/")
  (use-package rust-config
    :load-path "lisp/")
  (use-package python-config
    :load-path "lisp/")
  (use-package helm-config
    :load-path "lisp/")
  (use-package vterm-config
    :load-path "lisp/"))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-python-flake8-executable "python")
 '(flycheck-python-pycompile-executable "python")
 '(flycheck-python-pylint-executable "python")
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(pdf-tools yasnippet-snippets vterm use-package undo-tree typescript-mode smooth-scrolling ranger rainbow-mode racer paredit org-download magit lsp-mode json-mode helm-projectile helm-ag gruvbox-theme git-gutter flycheck exec-path-from-shell evil-surround evil-numbers evil-leader evil-collection elpy doom-themes define-word cmake-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#121112" :foreground "#fdf4c1" :weight medium :height 140 :width normal :family "Victor Mono"))))
 '(flycheck-error ((t (:underline "#cc6666"))))
 '(flycheck-info ((t (:underline "#b5bd68"))))
 '(flycheck-warning ((t (:underline "#f0c674"))))
 '(flymake-error ((t (:underline "#cc6666"))))
 '(flymake-note ((t (:underline "#b5bd68"))))
 '(flymake-warning ((t (:underline "#de935f"))))
 '(flyspell-duplicate ((t (:underline "DarkOrange"))))
 '(flyspell-incorrect ((t (:underline "#cc6666"))))
 '(font-lock-preprocessor-face ((t (:inherit bold :foreground "#886950"))))
 '(line-number ((t (:background "#121112"))))
 '(line-number-current-line ((t (:background "#121112" :foreground "#fe8019"))))
 '(markdown-code-face ((t (:background "#121112"))))
 '(markdown-pre-face ((t nil)))
 '(org-block ((t (:background "#121112"))))
 '(org-level-2 ((t (:box (:line-width 1 :color "unemphasizedSelectedContentBackgroundColor") :weight thin))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "#799")))))
