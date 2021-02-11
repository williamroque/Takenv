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
