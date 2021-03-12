;;; helm-config.el --- for armor-shining
;;; Commentary:
;; For helm-related customizations.
;;; Code:

(use-package helm
  :ensure t
  :config
  (helm-mode 1)

  ;; set M-x to the helm version
  (global-set-key (kbd "M-x") 'helm-M-x)

  (setq helm-completion-style 'emacs)
  (setq helm-minibuffer-history-key "M-p"))

(use-package helm-ag
  :ensure t)

(use-package helm-projectile
  :after helm
  :ensure t
  :config
  (setq helm-mini-default-sources '(helm-source-projectile-recentf-list
                                    helm-source-projectile-buffers-list
                                    helm-source-projectile-projects
                                    helm-source-projectile-files-list
                                    helm-source-buffers-list
                                    helm-source-recentf)))

(use-package evil-leader
  :after helm
  :config

  ;; open the minibuffer with SPC+SPC (leader+SPC)
  (evil-leader/set-key "SPC" 'helm-mini)

  ;; more convenient way to open minibuffer
  (evil-leader/set-key ";" 'helm-M-x)

  ;; for helm-ag
  (evil-leader/set-key "g" 'helm-ag))

(provide 'helm-config)
;;; helm-config.el ends here
