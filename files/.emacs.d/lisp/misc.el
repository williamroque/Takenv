;;; misc.el --- for random things
;;; Commentary:
;; All sorts of generally non-editing related things
;;; Code:

;; change scratch buffer message
;; (setq initial-scratch-message "")


;; put all backups in one place (not sure if this actually works)
(setq backup-directory-alist `(("." . "~/.saves")))

;; configure .curve for json
(add-to-list 'auto-mode-alist '("\\.curve\\'" . json-mode))

;; convenient next split
(global-set-key (kbd "M-h") 'evil-window-left)
(global-set-key (kbd "M-j") 'evil-window-down)
(global-set-key (kbd "M-k") 'evil-window-up)
(global-set-key (kbd "M-l") 'evil-window-right)

;; set default directory to home
(setq default-directory (concat (getenv "HOME") "/"))

;; add brazilian holidays to calendar
(use-package brazil-holidays
             :load-path "lisp/"
             :config
             (setq calendar-holidays holiday-brazil-holidays))

;; gitignore major mode 
(use-package gitignore-mode
             :load-path "lisp/")

;; convenient way to open lisp eval
(global-set-key (kbd "C-;") 'eval-expression)

;; delete char the fast way
(define-key key-translation-map (kbd "C-h") [127])

;; for debugging lists
(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

;; to add after index of list
(defun insert-after (lst index newelt)
  (push newelt (cdr (nthcdr index lst))) 
  lst)

;; create buffer toggle ring
(defvar buffer-toggle-ring '(t))
(defvar buffer-toggle-ring-index 0)

;; remove current buffer from ring
(defun remove-current-from-buffer-ring ()
  "Remove current buffer from buffer ring."
  (if (member (current-buffer) buffer-toggle-ring)
      (progn
        (setq buffer-toggle-ring (delete (current-buffer) buffer-toggle-ring))
        (print-elements-of-list buffer-toggle-ring)
        (setq buffer-toggle-ring-index (- buffer-toggle-ring-index 1)))))

;; add/remove current buffer from ring
(defun toggle-buffer-in-buffer-ring ()
  "Add/remove current buffer from ring."
  (interactive)
  (if (member (current-buffer) buffer-toggle-ring)
      (remove-current-from-buffer-ring)
    (progn
      (insert-after buffer-toggle-ring buffer-toggle-ring-index (current-buffer))
      (print-elements-of-list buffer-toggle-ring)
      (setq buffer-toggle-ring-index (+ buffer-toggle-ring-index 1)))))

;; switch to next buffer in ring
(defun switch-to-next-buffer-in-ring ()
  "Switch to next buffer in ring."
  (interactive)
  (if (> (length buffer-toggle-ring) 1)
      (progn
        (setq buffer-toggle-ring-index (+ (mod buffer-toggle-ring-index (- (length buffer-toggle-ring) 1)) 1))
        (switch-to-buffer (nth buffer-toggle-ring-index buffer-toggle-ring)))))

;; remove buffer from ring when buffer is closed
(add-hook 'kill-buffer-hook 'remove-current-from-buffer-ring)

(use-package evil
  :config
  (define-key evil-normal-state-map (kbd "<S-backspace>") 'toggle-buffer-in-buffer-ring)
  (define-key evil-normal-state-map (kbd "<backspace>") 'switch-to-next-buffer-in-ring))

;; open clipboard contents as a path
(defun open-clipboard-path ()
  "Open the contents of the clipboard as a path."
  (interactive)
  (find-file (current-kill 0 t)))

;; remove bell sound
(setq ring-bell-function 'ignore)

;; change template placeholder
(defvar my-placeholder-text "<++>"
  "Placeholder text to be replaced by `my-clear-next-placeholder'.")

(defun my-clear-next-placeholder (arg)
  "Jump forward to next occurrence of `my-placeholder-text' and remove.  ARG is not used."
  (interactive "i") ; ignore argument
  (if (search-forward my-placeholder-text nil t) ; ignore error
      (progn
        (delete-backward-char (length my-placeholder-text))
        (evil-insert-state))))


;; convenient options
;;(fset 'yes-or-no-p 'y-or-n-p)

;; stop creating lockfiles
(setq create-lockfiles nil)

;; delete a file by moving it to trash
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")


;; use backslash to end search highlighting
(define-key evil-normal-state-map (kbd "\\") 'evil-ex-nohighlight)

;; mark current line
(defun find-overlays-specifying (prop pos)
  (let ((overlays (overlays-at pos))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay prop)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))

(defun highlight-or-dehighlight-line ()
  "Toggle line highlight.  Probably from stackoverflow."
  (interactive)
  (if (find-overlays-specifying
       'line-highlight-overlay-marker
       (line-beginning-position))
      (remove-overlays (line-beginning-position) (+ 1 (line-end-position)))
    (let ((overlay-highlight (make-overlay
                              (line-beginning-position)
                              (+ 1 (line-end-position)))))
        (overlay-put overlay-highlight 'face '(:background "#313031"))
        (overlay-put overlay-highlight 'line-highlight-overlay-marker t))))

;; remove all line highlights
(defun remove-all-highlight ()
  "Remove all line highlighting."
  (interactive)
  (remove-overlays (point-min) (point-max)))

;; use command key as meta
(setq mac-command-modifier 'meta)

(use-package evil-leader
  :config
  (evil-leader/set-key (kbd "0") 'remove-all-highlight)

  ;; open non-fullscreen window
  (evil-leader/set-key (kbd "-") (lambda ()
                                   (interactive)
                                   (make-frame-command)
                                   (run-at-time .2 nil (lambda ()
                                                         (set-frame-parameter nil 'fullscreen nil)))))

  ;; switch frames easily
  (evil-leader/set-key (kbd "o") 'other-frame)

  (evil-leader/set-key (kbd "9") 'highlight-or-dehighlight-line)

  ;; convenient way to start markdown preview
  ;;(evil-leader/set-key "m" 'livedown-preview)

  ;; minimize frame
  (evil-leader/set-key "m" 'suspend-frame)

  ;; delete current file
  (evil-leader/set-key "d" (lambda ()
                             (interactive)
                             (if (yes-or-no-p "Really truly really delete this file? ")
                                 (delete-file (buffer-file-name)))))

  ;; close current buffer and open dired at cwd
  (evil-leader/set-key "i" (lambda ()
                             (interactive)
                             (kill-buffer (current-buffer))
                             (dired-at-point ".")))

  (evil-leader/set-key "r" 'my-clear-next-placeholder)

  ;; insert template placeholder
  (evil-leader/set-key "R" '(lambda ()
                              (interactive)
                              (ignore-errors (forward-char))
                              (insert my-placeholder-text)))

  ;; create scratch buffer
  (evil-leader/set-key "b" '(lambda () (interactive) (switch-to-buffer "*scratch*"))))

;; install pdf-tools
(pdf-tools-install)

;; split vertically by default
(setq split-width-threshold 0)
(setq split-height-threshold nil)

(provide 'misc)
;;; misc.el ends here
