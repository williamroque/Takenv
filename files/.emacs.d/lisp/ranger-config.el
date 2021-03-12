;;; ranger-config.el --- For super-special file browsing
;;; Commentary:
;; For ranger-related config.
;;; Code:

(use-package ranger
  :ensure t
  :config
  (evil-leader/set-key "k" 'ranger))

(provide 'ranger-config)
;;; ranger-config.el ends here
