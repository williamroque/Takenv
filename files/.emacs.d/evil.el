;; set evil mode
(require 'evil)
(evil-mode 1)

;; set leader
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")

;; surround mode
(global-evil-surround-mode 1)

;; convenient saving
(evil-leader/set-key "f" 'save-buffer)

;; convenient exiting
(evil-leader/set-key "e" 'kill-this-buffer)
(evil-leader/set-key "x" '(lambda () (interactive) (save-buffer) (kill-this-buffer)))

;; disable evil for vterm
;; (add-hook 'vterm-mode-hook 'evil-emacs-state)

;; use evil-collection
(evil-collection-init)

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
;; (define-key evil-normal-state-map (kbd "RET") 'evil-execute-macro)

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

;; I'm not proud of it, I'm changing it if I can, but it works
(add-hook 'prog-mode-hook '(lambda() (interactive) (define-key evil-insert-state-local-map (kbd "j") 'my-jk)))
(add-hook 'text-mode-hook '(lambda() (interactive) (define-key evil-insert-state-local-map (kbd "j") 'my-jk)))

;; prevent jk in vterm mode
(add-hook 'vterm-mode-hook '(lambda() (interactive) (define-key evil-insert-state-local-map (kbd "j") nil)))

;; add digraphs
(setq evil-digraphs-table-user '(((?r ?a) . ?\x2192)
                                 ((?a ?m) . ?\x0026)
                                 ((?s ?l) . ?\x005c)))

;; convenient remap for ex state
(evil-define-key 'normal 'global (kbd ";") 'evil-ex)
(evil-define-key 'normal 'dired-mode-map (kbd ";") 'evil-ex)

;; repeat find char remap
(evil-define-key 'normal 'global (kbd "-") 'evil-repeat-find-char)

;; change cursor colors based on mode (state)
(setq evil-emacs-state-cursor '("white" box))
(setq evil-normal-state-cursor '("#aaa" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("#dacfa6" bar))
(setq evil-replace-state-cursor '("orange" hollow))
(setq evil-operator-state-cursor '("orange" hollow))

;; select recently pasted
(evil-leader/set-key "V" 'exchange-point-and-mark)

;; convenient way to evaluate buffer
(evil-leader/set-key "v" 'eval-buffer)

;; convenient terminal opening
(evil-leader/set-key "t" 'vterm)

;; disable evil for calendar mode
(evil-set-initial-state 'calendar-mode 'emacs)

;; better line motion
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

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

;; increment/decrement numbers
(evil-leader/set-key "]" 'evil-numbers/inc-at-pt)
(evil-leader/set-key "[" 'evil-numbers/dec-at-pt)

;; move cursor to the right when splitting
(setq evil-vsplit-window-right t)
(setq evil-split-window-below t)

;; show line numbers when repeating command
(setq began-line-toggle nil)

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
(evil-leader/set-key "n" 'cautious-line-toggle)
