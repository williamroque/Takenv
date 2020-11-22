;; configure helm
(helm-mode 1)
(evil-leader/set-key "SPC" 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)

(require 'helm-projectile)
(setq helm-mini-default-sources '(helm-source-projectile-recentf-list
                                  helm-source-projectile-buffers-list
                                  helm-source-projectile-projects
                                  helm-source-projectile-files-list
                                  helm-source-buffers-list
                                  helm-source-recentf))


;; put all backups in one place
(setq backup-directory-alist `(("." . "~/.saves")))

;; configure .curve for json
(add-to-list 'auto-mode-alist '("\\.curve\\'" . json-mode))

;; convenient next split
(global-set-key (kbd "M-l") 'evil-window-right)
(global-set-key (kbd "M-h") 'evil-window-left)

;; LSP
(setq lsp-rust-server 'rust-analyzer)

;; set default directory to home
(setq default-directory (concat (getenv "HOME") "/"))

;; add brazilian holidays to calendar
(load-user-file "brazil-holidays.el")
(setq calendar-holidays holiday-brazil-holidays)

;; convenient way to open lisp eval
(global-set-key (kbd "C-;") 'eval-expression)

;; convenient way to start markdown preview
(evil-leader/set-key "m" 'livedown-preview)
