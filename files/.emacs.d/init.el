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
   '(typescript-mode ranger lsp-mode json-mode doom-themes rust-mode evil-surround smooth-scrolling helm-ag helm-projectile helm evil-leader undo-tree gruvbox-theme evil))
 '(pdf-view-midnight-colors '("#fdf4c1" . "#1d2021")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#121112" :foreground "#fdf4c1" :weight medium :height 140 :width normal :family "Victor Mono"))))
 '(line-number ((t (:background "#121112"))))
 '(line-number-current-line ((t (:background "#121112" :foreground "#fe8019"))))
 '(markdown-code-face ((t (:background "#121112"))))
 '(markdown-pre-face ((t nil))))

;; makes modularization easier
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "packages.el")
(load-user-file "evil.el")
(load-user-file "edit.el")
(load-user-file "ui.el")
(load-user-file "misc.el")
