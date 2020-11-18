;; add MELPA index
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

;; package-install
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" default))
 '(evil-undo-system 'undo-tree)
 '(global-undo-tree-mode t)
 '(package-selected-packages
   '(evil-surround smooth-scrolling helm-ag helm-projectile helm evil-leader undo-tree gruvbox-theme evil)))

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

;; configure helm
(helm-mode 1)
(evil-leader/set-key "SPC" 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)

;; remove menu and tool bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; remove scrollbar
(toggle-scroll-bar -1)

;; smooth scrolling
(setq scroll-margin 5
scroll-conservatively 9999
scroll-step 1)

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
(global-set-key [escape] 'evil-exit-emacs-state)

;; set hybrid line numbers
(global-set-key [f1] (lambda ()
(interactive)
(setq-local display-line-numbers
	    (if (equal display-line-numbers nil) 'relative nil))))

;; hide line numbers for term
(add-hook 'term-mode-hook (lambda () (setq-local display-line-numbers nil)))

;; disable evil for term
(evil-set-initial-state 'term-mode 'emacs)

;; set theme
(load-theme 'gruvbox-dark-hard t)

;; remove startup screen
(setq inhibit-startup-screen t)

;; set default window size
(if (window-system) (set-frame-size (selected-frame) 110 30))

;; margins
(fringe-mode 0)
(add-hook 'window-configuration-change-hook
		  (lambda ()
			(set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 2 2)))

;; customize theme
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#121112" :foreground "#fdf4c1" :weight medium :height 140 :width normal :family "Victor Mono"))))
 '(line-number ((t (:background "#121112"))))
 '(line-number-current-line ((t (:background "#121112" :foreground "#fe8019")))))

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

;; tab width
(setq-default tab-width 4)
(setq c-basic-offset 4)
(setq-default indent-tabs-mode nil)

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

;; use fira code for greek letters
(set-fontset-font (face-attribute 'default :fontset)
				  '(#x0370 . #x03FF) (font-spec :family "Fira Code") nil 'append)

;; add digraphs
(setq evil-digraphs-table-user '(((?r ?a) . ?\x2192)))

;; sort of inelegant way of adding ligature support
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
			   (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
			   (36 . ".\\(?:>\\)")
			   (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
			   (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
			   (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
			   (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
			   (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
			   (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
			   (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
			   (48 . ".\\(?:x[a-zA-Z]\\)")
			   (58 . ".\\(?:::\\|[:=]\\)")
			   (59 . ".\\(?:;;\\|;\\)")
			   (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
			   (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
			   (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
			   (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
			   (91 . ".\\(?:]\\)")
			   (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
			   (94 . ".\\(?:=\\)")
			   (119 . ".\\(?:ww\\)")
			   (123 . ".\\(?:-\\)")
			   (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
			   (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
			   )
			 ))
  (dolist (char-regexp alist)
	(set-char-table-range composition-function-table (car char-regexp)
						  `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; convenient remap for ex state
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd ";") 'evil-ex))

;; start server -- not sure if this actually does anything
(server-start)

;; change cursor colors based on mode (state)
(setq evil-emacs-state-cursor '("white" box))
(setq evil-normal-state-cursor '("#aaa" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("#dacfa6" bar))
(setq evil-replace-state-cursor '("orange" hollow))
(setq evil-operator-state-cursor '("orange" hollow))

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

;; you're looking at a compulsive saver here
(setq make-backup-files nil)

;; stop moving backward on exiting insert mode
(setq evil-move-cursor-back nil)

;; select recently pasted
(evil-leader/set-key "V" 'exchange-point-and-mark)
