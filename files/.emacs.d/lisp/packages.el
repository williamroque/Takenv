(use-package livedown
             :load-path "lisp/")

;; enable flycheck mode
(add-hook 'after-init-hook #'global-flycheck-mode)

;; enable YASnippet
(yas-global-mode)

(provide 'packages)
