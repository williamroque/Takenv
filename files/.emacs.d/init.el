;; load before evil-leader
(setq evil-want-keybinding nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3c3836" "#fb4933" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(evil-search-module 'evil-search)
 '(evil-undo-system 'undo-tree)
 '(flycheck-python-flake8-executable "python")
 '(flycheck-python-pycompile-executable "python")
 '(flycheck-python-pylint-executable "python")
 '(global-undo-tree-mode t)
 '(helm-completion-style 'emacs)
 '(helm-minibuffer-history-key "M-p")
 '(org-adapt-indentation nil)
 '(org-export-with-section-numbers nil)
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.0 :html-foreground "Black" :html-background "Transparent" :html-scale 0.4 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-html-postamble nil)
 '(package-selected-packages
   '(rainbow-mode lsp-mode json-mode doom-themes rust-mode evil-surround smooth-scrolling helm-ag helm-projectile helm evil-leader undo-tree gruvbox-theme evil))
 '(pdf-view-midnight-colors '("#fdf4c1" . "#1d2021")))
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
