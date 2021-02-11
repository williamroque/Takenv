;;; ui.el --- for UI-related config
;;; Commentary:
;; All the good UI-related stuff.
;;; Code:

;; colorscheme stuff
(defvar ansi-color-names-vector
      ["#3c3836" "#fb4933" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
(defvar pdf-view-midnight-colors '("#fdf4c1" . "#1d2021"))

;; random face configurations
(custom-set-faces
 '(default ((t (:background "#121112" :foreground "#fdf4c1" :weight medium :height 140 :width normal :family "Victor Mono"))))
 '(flycheck-error ((t (:underline "#cc6666"))))
 '(flycheck-info ((t (:underline "#b5bd68"))))
 '(flycheck-warning ((t (:underline "#f0c674"))))
 '(flymake-error ((t (:underline "#cc6666"))))
 '(flymake-note ((t (:underline "#b5bd68"))))
 '(flymake-warning ((t (:underline "#de935f"))))
 '(flyspell-duplicate ((t (:underline "DarkOrange"))))
 '(flyspell-incorrect ((t (:underline "#cc6666"))))
 '(font-lock-preprocessor-face ((t (:inherit bold :foreground "#886950"))))
 '(line-number ((t (:background "#121112"))))
 '(line-number-current-line ((t (:background "#121112" :foreground "#fe8019"))))
 '(markdown-code-face ((t (:background "#121112"))))
 '(markdown-pre-face ((t nil)))
 '(org-block ((t (:background "#121112"))))
 '(org-level-2 ((t (:box (:line-width 1 :color "unemphasizedSelectedContentBackgroundColor") :weight thin))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "#799")))))

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
(defun toggle-line-numbers ()
  (interactive)
  (setq-local display-line-numbers
              (if (equal display-line-numbers nil) 'relative nil)))

(add-hook 'prog-mode-hook (lambda ()
                            (unless (equal (buffer-name) "*scratch*")
                              (toggle-line-numbers))))

;; hide line numbers for term
(add-hook 'term-mode-hook (lambda () (setq-local display-line-numbers nil)))

;; set theme
(load-theme 'doom-tomorrow-night t)

;; remove startup screen
(setq inhibit-startup-screen t)

;; margins
(fringe-mode 0)
(add-hook 'window-configuration-change-hook
          (lambda ()
            (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 2 2)))

;; use fira code for greek letters
(defun setup-font ()
  (set-fontset-font (face-attribute 'default :fontset)
                    '(#x0370 . #x03FF) (font-spec :family "Fira Code") nil 'append))
(add-hook 'vterm-mode-hook #'setup-font)

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

;; set default frame size
(if (window-system) (set-frame-size (selected-frame) 110 30))

;; center window
(set-frame-position (selected-frame)
                    (- (round (/ (display-pixel-width) 2)) (round (/ (frame-outer-width) 2)))
                    (- (round (/ (display-pixel-height) 2)) (round (/ (frame-outer-height) 2))))

;; make sure client loading doesn't make Emacs forget this whole configuration thing ever happened
(add-hook 'before-make-frame-hook
          #'(lambda ()
              (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
              (add-to-list 'default-frame-alist '(height . 30))
              (add-to-list 'default-frame-alist '(width  . 110))))

;; raise frame
(add-hook 'server-switch-hook (lambda () (select-frame-set-input-focus (selected-frame))))

(set-face-attribute 'font-lock-function-name-face nil
                    :foreground "#6CA17A")
(set-face-attribute 'font-lock-keyword-face nil
                    :weight 'bold
                    :foreground "#cc6666")
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-constant-face nil
                    :foreground "#cc6666")
(set-face-attribute 'font-lock-variable-name-face nil
                    :foreground "#E0DBA9")
(set-face-attribute 'font-lock-type-face nil
                    :foreground "#6CA17A")

;; autoselect window with mouse on it
(setq mouse-autoselect-window t)

;; display time
(display-time-mode 1)

(use-package rainbow-mode
  :ensure t
  :config
  ;; let rainbows brighten your day every day, whether you like it or not
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; resize frame naturally (cf. by columns/rows)
(setq frame-resize-pixelwise t)

;; hide markdown for org-mode
(setq org-hide-emphasis-markers t)

(provide 'ui)
;;; ui.el ends here
