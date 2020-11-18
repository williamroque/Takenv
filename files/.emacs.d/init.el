;; Add MELPA index
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; package-install
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-undo-system 'undo-tree)
 '(global-undo-tree-mode t)
 '(package-selected-packages '(undo-tree gruvbox-theme evil)))

;; Set evil mode
(require 'evil)
(evil-mode 1)

;; Remove menu and tool bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Remove scrollbar
(toggle-scroll-bar -1)

;; Set hybrid line numbers
(global-set-key [f1] (lambda ()
		       (interactive)
		       (setq-local display-line-numbers
						   (if (equal display-line-numbers nil) 'relative nil))))

;; Hide line numbers for term
(add-hook 'term-mode-hook (lambda () (setq-local display-line-numbers nil)))

;; Set theme
(load-theme 'gruvbox t)

;; Remove startup screen
(setq inhibit-startup-screen t)

;; Set default window size
(if (window-system) (set-frame-size (selected-frame) 110 30))

;; Margins
(fringe-mode 0)
(add-hook 'window-configuration-change-hook
          (lambda ()
            (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 2 2)))

;; Customize theme
(custom-set-faces
 '(default ((t (:background "#121112" :foreground "#fdf4c1" :weight medium :height 140 :width normal :family "Victor Mono"))))
 '(line-number ((t (:background "#121112"))))
 '(line-number-current-line ((t (:background "#121112" :foreground "#fe8019")))))

;; Set convenient line-opening
(define-key evil-normal-state-map (kbd "zj")
  (lambda () (interactive) (call-interactively 'move-end-of-line) (newline)))
(define-key evil-normal-state-map (kbd "zk")
  (lambda () (interactive) (previous-line) (call-interactively 'move-end-of-line) (newline)))

;; Set convenient scrolling
(define-key evil-normal-state-map (kbd "J")
  (lambda () (interactive) (scroll-up 3)))
(define-key evil-normal-state-map (kbd "K")
  (lambda () (interactive) (scroll-down 3)))

;; Tab width
(setq tab-width 4)
(setq c-basic-offset 4)

;; Quick exit insert mode
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

;; Use fira code for greek letters
(set-fontset-font (face-attribute 'default :fontset)
				  '(#x0370 . #x03FF) (font-spec :family "Fira Code") nil 'append)

;; Add digraphs
(setq evil-digraphs-table-user '(((?r ?a) . ?\x2192)))

;; Sort of inelegant way of adding ligature support
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
