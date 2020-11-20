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

;; disable evil for term
(evil-set-initial-state 'term-mode 'emacs)

;; set convenient line-opening
(define-key evil-normal-state-map (kbd "zj")
  (lambda () (interactive) (call-interactively 'move-end-of-line) (newline)))
(define-key evil-normal-state-map (kbd "zk")
  (lambda () (interactive) (previous-line) (call-interactively 'move-end-of-line) (newline)))

;; set convenient scrolling
(define-key evil-normal-state-map (kbd "J")
  (lambda () (interactive) (scroll-up 3)))
(define-key evil-normal-state-map (kbd "K")
  (lambda () (interactive) (scroll-down 3)))

;; convenient macro execution
(define-key evil-normal-state-map (kbd "RET") 'evil-execute-macro)

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

(define-key evil-insert-state-map (kbd "j") 'my-jk)

;; add digraphs
(setq evil-digraphs-table-user '(((?r ?a) . ?\x2192)))

;; convenient remap for ex state
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd ";") 'evil-ex))

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
(evil-leader/set-key "t" 'ansi-term)

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
