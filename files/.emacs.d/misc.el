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
(global-set-key (kbd "M-j") 'evil-window-down)
(global-set-key (kbd "M-k") 'evil-window-up)

;; Rust LSP
(setq lsp-rust-server 'rust-analyzer)

;; Python environment
(elpy-enable)

;; set default directory to home
(setq default-directory (concat (getenv "HOME") "/"))

;; add brazilian holidays to calendar
(load-user-file "brazil-holidays.el")
(setq calendar-holidays holiday-brazil-holidays)

;; convenient way to open lisp eval
(global-set-key (kbd "C-;") 'eval-expression)

;; delete char the fast way
(define-key key-translation-map (kbd "C-h") [127])

;; convenient way to start markdown preview
(evil-leader/set-key "m" 'livedown-preview)

;; use backspace to switch buffers
(define-key evil-normal-state-map (kbd "<backspace>") 'next-buffer)
(define-key evil-normal-state-map (kbd "<S-backspace>") 'previous-buffer)
