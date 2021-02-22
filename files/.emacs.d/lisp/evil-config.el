;;; evil.el --- for all nefarious or vi-related things
;;; Commentary:
;; All kinds of evil-mode configurations.
;;; Code:

(use-package evil
             :ensure t
             :init
             (setq evil-want-keybinding nil)

             (setq evil-search-module 'evil-search)
             (setq evil-undo-system 'undo-tree)

             ;; move cursor to the right when splitting
             (setq evil-vsplit-window-right t)
             (setq evil-split-window-below t)

             ;; add digraphs
             (setq evil-digraphs-table-user '(((?r ?a) . ?\x2192)
                                              ((?a ?m) . ?\x0026)
                                              ((?s ?l) . ?\x005c)))

             ;; change cursor colors based on mode (state)
             (setq evil-emacs-state-cursor '("white" box))
             (setq evil-normal-state-cursor '("#aaa" box))
             (setq evil-visual-state-cursor '("orange" box))
             (setq evil-insert-state-cursor '("#dacfa6" bar))
             (setq evil-replace-state-cursor '("orange" hollow))
             (setq evil-operator-state-cursor '("orange" hollow))

             :config
             (evil-mode 1)

             ;; surround mode
             (global-evil-surround-mode 1)

             ;; set convenient line-opening
             (define-key evil-normal-state-map (kbd "zj")
                         (lambda () (interactive) (call-interactively 'move-end-of-line) (newline)))
             (define-key evil-normal-state-map (kbd "zk")
                         (lambda () (interactive) (call-interactively 'move-beginning-of-line) (newline) (previous-line)))

             ;; set convenient scrolling
             (define-key evil-normal-state-map (kbd "C-j")
                         (lambda () (interactive) (scroll-up 3)))
             (define-key evil-normal-state-map (kbd "C-k")
                         (lambda () (interactive) (scroll-down 3)))

             ;; (muscle memory)
             (define-key evil-normal-state-map (kbd "J")
                         (lambda () (interactive) (scroll-up 3)))
             (define-key evil-normal-state-map (kbd "K")
                         (lambda () (interactive) (scroll-down 3)))

             ;; convenient macro execution
             (evil-define-key 'normal 'global (kbd "SPC RET") "@q")

             ;; quick exit insert mode
             (defun my-jk ()
               (interactive)
               (let* ((initial-key ?j)
                      (final-key ?k)
                      (timeout 0.5)
                      (event (read-event nil nil timeout)))
                 (if event
                   ;; timeout met
                   (if (and (characterp event) (= event final-key))
                     (evil-normal-state)
                     (insert initial-key)
                     (push event unread-command-events))
                   ;; timeout exceeded
                   (insert initial-key))))

             ;; bind jk
             (add-hook 'prog-mode-hook '(lambda() (interactive) (define-key evil-insert-state-local-map (kbd "j") 'my-jk)))
             (add-hook 'text-mode-hook '(lambda() (interactive) (define-key evil-insert-state-local-map (kbd "j") 'my-jk)))
             (add-hook 'conf-unix-mode-hook '(lambda() (interactive) (define-key evil-insert-state-local-map (kbd "j") 'my-jk)))
             (add-hook 'conf-toml-mode-hook '(lambda() (interactive) (define-key evil-insert-state-local-map (kbd "j") 'my-jk)))

             ;; prevent jk in vterm mode
             (add-hook 'vterm-mode-hook '(lambda() (interactive) (define-key evil-insert-state-local-map (kbd "j") nil)))


             ;; convenient remap for ex state
             (evil-define-key 'normal 'global (kbd ";") 'evil-ex)
             (evil-define-key 'normal 'dired-mode-map (kbd ";") 'evil-ex)

             ;; repeat find char remap
             (evil-define-key 'normal 'global (kbd "-") 'evil-repeat-find-char)

             ;; better line motion
             (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
             (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line))

(use-package evil-collection
             :after evil
             :ensure t
             :config
             (evil-collection-init))

(use-package evil-leader
             :after evil
             :ensure t
             :init
             (setq evil-leader/in-all-states 1)

             :config
             (global-evil-leader-mode)

             ;; set leader
             (evil-leader/set-leader "SPC")

             ;; convenient saving
             (evil-leader/set-key "f" 'save-buffer)

             ;; convenient exiting
             (evil-leader/set-key "e" 'kill-this-buffer)
             (evil-leader/set-key "x" '(lambda () (interactive) (save-buffer) (kill-this-buffer)))

             ;; select recently pasted
             (evil-leader/set-key "V" 'exchange-point-and-mark)

             ;; convenient way to evaluate buffer
             (evil-leader/set-key "v" 'eval-buffer)

             ;; convenient terminal opening
             (evil-leader/set-key "t" 'vterm)

             ;; increment/decrement numbers
             (evil-leader/set-key "]" 'evil-numbers/inc-at-pt)
             (evil-leader/set-key "[" 'evil-numbers/dec-at-pt)

             ;; show line numbers when repeating command
             (defvar began-line-toggle nil
               "Whether line numbers have currently been toggled.")

             (defun cautious-line-toggle ()
               "Toggle line numbers temporarily if began-line-toggle is nil"
               (interactive)
               (if (and (not began-line-toggle) (not (derived-mode-p 'prog-mode)))
                 (progn
                   (toggle-line-numbers)
                   (setq began-line-toggle t))))

             (add-hook 'prefix-command-preserve-state-hook 'cautious-line-toggle)

             (add-hook 'pre-command-hook '(lambda ()
                                            (interactive)
                                            (if began-line-toggle
                                              (progn
                                                (toggle-line-numbers)
                                                (setq began-line-toggle nil)))))

             ;; temporarily show line numbers
             (evil-leader/set-key "n" 'cautious-line-toggle))

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
  then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
    (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(provide 'evil-config)
;;; evil-config.el ends here
