;; configure helm
(helm-mode 1)
(evil-leader/set-key "SPC" 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)

(require 'helm-projectile)
(setq helm-mini-default-sources '(helm-source-projectile-recentf-list
                                  helm-source-projectile-buffers-list
                                  helm-source-projectile-projects
                                  helm-source-projectile-files-list
                                  helm-source-buffers-list
                                  helm-source-recentf))


;; put all backups in one place (not sure if this actually works)
(setq backup-directory-alist `(("." . "~/.saves")))

;; configure .curve for json
(add-to-list 'auto-mode-alist '("\\.curve\\'" . json-mode))

;; convenient next split
(global-set-key (kbd "M-h") 'evil-window-left)
(global-set-key (kbd "M-j") 'evil-window-down)
(global-set-key (kbd "M-k") 'evil-window-up)
(global-set-key (kbd "M-l") 'evil-window-right)

(add-hook 'vterm-mode-hook
          (lambda ()
            (define-key vterm-mode-map (kbd "M-h") 'evil-window-left)
            (define-key vterm-mode-map (kbd "M-j") 'evil-window-down)
            (define-key vterm-mode-map (kbd "M-k") 'evil-window-up)
            (define-key vterm-mode-map (kbd "M-l") 'evil-window-right)))

;; Rust LSP
(setq lsp-rust-server 'rust-analyzer)

;; Python environment
(elpy-enable)

;; set default directory to home
(setq default-directory (concat (getenv "HOME") "/"))

;; add brazilian holidays to calendar
(load-user-file "brazil-holidays.el")
(setq calendar-holidays holiday-brazil-holidays)

;; convenient way to open lisp eval
(global-set-key (kbd "C-;") 'eval-expression)

;; delete char the fast way
(define-key key-translation-map (kbd "C-h") [127])

;; convenient way to start markdown preview
(evil-leader/set-key "m" 'livedown-preview)

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
(setq buffer-toggle-ring '(t))
(setq buffer-toggle-ring-index 0)

;; remove current buffer from ring
(defun remove-current-from-buffer-ring ()
  "Removes current buffer from buffer ring"
  (if (member (current-buffer) buffer-toggle-ring)
      (progn
        (setq buffer-toggle-ring (delete (current-buffer) buffer-toggle-ring))
        (print-elements-of-list buffer-toggle-ring)
        (setq buffer-toggle-ring-index (- buffer-toggle-ring-index 1)))))

;; add/remove current buffer from ring
(defun toggle-buffer-in-buffer-ring ()
  (interactive)
  (if (member (current-buffer) buffer-toggle-ring)
      (remove-current-from-buffer-ring)
    (progn
      (insert-after buffer-toggle-ring buffer-toggle-ring-index (current-buffer))
      (print-elements-of-list buffer-toggle-ring)
      (setq buffer-toggle-ring-index (+ buffer-toggle-ring-index 1)))))

;; switch to next buffer in ring
(defun switch-to-next-buffer-in-ring ()
  (interactive)
  (if (> (length buffer-toggle-ring) 1)
      (progn
        (setq buffer-toggle-ring-index (+ (mod buffer-toggle-ring-index (- (length buffer-toggle-ring) 1)) 1))
        (switch-to-buffer (nth buffer-toggle-ring-index buffer-toggle-ring)))))

;; remove buffer from ring when buffer is closed
(add-hook 'kill-buffer-hook 'remove-current-from-buffer-ring)

(define-key evil-normal-state-map (kbd "<S-backspace>") 'toggle-buffer-in-buffer-ring)
(define-key evil-normal-state-map (kbd "<backspace>") 'switch-to-next-buffer-in-ring)

;; org mode custom HTML export style
(defun my-org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.css"))
           (homestyle (or (null dir) (null (file-exists-p path))))
           (final (if homestyle "~/.org_style.css" path)))
      (setq org-html-head-include-default-style nil)
      (setq org-html-head (concat
                           "<style type=\"text/css\">\n"
                           "<!--/*--><![CDATA[/*><!--*/\n"
                           (with-temp-buffer
                             (insert-file-contents final)
                             (buffer-string))
                           "/*]]>*/-->\n"
                           "</style>\n")))))

(add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)

;; Replace \vert with pipe for tables with latex
(require 'ox)
(defun my-code-filter-replace-pipes (text backend info)
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "\\\\vert" "|" text)))
(add-to-list 'org-export-filter-code-functions 'my-code-filter-replace-pipes)

;; org-download
(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)

;; toggle truncate lines in org-mode
(define-key org-mode-map "\M-q" 'toggle-truncate-lines)

;; send esc to vterm properly
(add-hook 'vterm-mode-hook
            (lambda () (local-set-key [escape] #'(vterm-send-key "<escape>"))))

;; open clipboard contents as a path
(defun open-clipboard-path ()
  "Open the contents of the clipboard as a path"
  (interactive)
  (find-file (current-kill 0 t)))

;; remove bell sound
(setq ring-bell-function 'ignore)
