;;; vterm-config.el --- For enhanced vtermming
;;; Commentary:
;; For vterm-related config.
;;; Code:

;; clear vterm scrollback
(use-package vterm
  :ensure t
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (define-key vterm-mode-map (kbd "M-h") 'evil-window-left)
              (define-key vterm-mode-map (kbd "M-j") 'evil-window-down)
              (define-key vterm-mode-map (kbd "M-k") 'evil-window-up)
              (define-key vterm-mode-map (kbd "M-l") 'evil-window-right)
              (setq-local evil-insert-state-cursor '("#aaa" box))))

  (define-key vterm-mode-map "\M-u" 'vterm-clear)

  ;; use C-return to send escape to vterm
  (add-hook 'vterm-mode-hook
            (lambda () (local-set-key (kbd "<C-return>") (lambda ()
                                                           (interactive)
                                                           (vterm-send-key "<escape>"))))))


(provide 'vterm-config)
;;; vterm-config.el ends here
