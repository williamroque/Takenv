;; remove menu and tool bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; remove scrollbar
(toggle-scroll-bar -1)

;; smooth scrolling
(setq scroll-margin 5
scroll-conservatively 9999
scroll-step 1)

;; set hybrid line numbers
(global-set-key [f1] (lambda ()
(interactive)
(setq-local display-line-numbers
	    (if (equal display-line-numbers nil) 'relative nil))))

;; hide line numbers for term
(add-hook 'term-mode-hook (lambda () (setq-local display-line-numbers nil)))

;; set theme
(load-theme 'doom-tomorrow-night t)

;; remove startup screen
(setq inhibit-startup-screen t)

;; set default window size
(if (window-system) (set-frame-size (selected-frame) 110 30))

;; margins
(fringe-mode 0)
(add-hook 'window-configuration-change-hook
		  (lambda ()
			(set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 2 2)))

;; use fira code for greek letters
(set-fontset-font (face-attribute 'default :fontset)
				  '(#x0370 . #x03FF) (font-spec :family "Fira Code") nil 'append)

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

;; hide frame
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark)) 
