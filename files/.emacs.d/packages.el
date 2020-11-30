;; add MELPA index
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

;; fix executable path
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; package-install
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3c3836" "#fb4933" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(evil-undo-system 'undo-tree)
 '(global-undo-tree-mode t)
 '(helm-completion-style 'emacs)
 '(package-selected-packages
   '(lsp-mode json-mode doom-themes rust-mode evil-surround smooth-scrolling helm-ag helm-projectile helm evil-leader undo-tree gruvbox-theme evil))
 '(pdf-view-midnight-colors '("#fdf4c1" . "#1d2021")))

;; markdown live preview
(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
(require 'livedown)

;; enable flycheck mode
(add-hook 'after-init-hook #'global-flycheck-mode)
