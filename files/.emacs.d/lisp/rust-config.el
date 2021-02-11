;;; rust-config.el --- for rust-related config
;;; Commentary:
;; All sorts of rust-related things.
;;; Code:

;; Rust LSP
(defvar lsp-rust-server 'rust-analyzer)

;; enable racer mode in rust mode
(add-hook 'rust-mode-hook #'racer-mode)

;; enable company mode in racer
(add-hook 'rust-mode-hook #'company-mode)

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(defvar company-tooltip-align-annotations t)

(defvar racer-rust-src-path
      (concat (string-trim
               (shell-command-to-string "rustc --print sysroot"))
              "/lib/rustlib/src/rust/library"))

(provide 'rust-config)
;;; rust-config.el ends here
