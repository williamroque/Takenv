;; tab width
(setq-default tab-width 4)
(setq c-basic-offset 4)
(setq-default indent-tabs-mode nil)

;; stop blinking, cursor. seriously.
(blink-cursor-mode -1)

;; show matching parenthesis
(show-paren-mode t)

;; break text when the time is right
(add-hook 'text-mode-hook (lambda ()
                            (turn-on-auto-fill)
                            (set-fill-column 82)))
(add-hook 'markdown-mode-hook (lambda ()
                            (turn-on-auto-fill)
                            (set-fill-column 82)))

;; set spellcheck program
(setq ispell-program-name "/usr/local/bin/aspell")

;; convenient enable spellcheck
(global-set-key (kbd "<f6>") 'flyspell-prog-mode)

;; convenient language changing
(let ((langs '("american" "brasileiro")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

(global-set-key (kbd "C-<f6>") 'cycle-ispell-languages)

;; only spellcheck comments
(setq flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face))

;; copy filepath to clipboard
(defun copy-filepath ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

;; enable paredit for lisp
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
