;;; python-config.el --- for python-related config
;;; Commentary:
;; All sorts of python-related things.
;;; Code:

(use-package elpy
  :ensure t
  :config
  (elpy-enable)

  ;; stop weird indentation guides from elpy
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1))))

;; setting python executable
(defvar flycheck-python-flake8-executable "python")
(defvar flycheck-python-pycompile-executable "python")
(defvar flycheck-python-pylint-executable "python")

;; to run python files
(defun run-current-python ()
  "Run current Python file."
  (interactive)
  (shell-command-to-string (format "export PYTHONPATH='${PYTHONPATH}:/Users/jetblack/maxwell/src/' && python '%s'" (buffer-file-name))))

(use-package evil-leader
  :config
  (evil-leader/set-key "," (lambda ()
                             (interactive)
                             (princ (run-current-python))))
  (evil-leader/set-key "." (lambda ()
                             (interactive)
                             (forward-char)
                             (save-excursion (insert (string-trim (run-current-python)))))))

;; fix python linting issue
(custom-set-variables
 '(flycheck-python-flake8-executable "python")
 '(flycheck-python-pycompile-executable "python")
 '(flycheck-python-pylint-executable "python"))

;; better highlighting for python
(defface method-call-lock
  '((t :foreground "#b5bd68"))
  "Face for method calls."
  :group 'python-mode)

(defface operator-lock
  '((t :foreground "#cc6666"))
  "Face for operators."
  :group 'python-mode)

(add-hook 'python-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("^[^#][^#]*\\.\\([A-Za-z_]*?\\)(.*?)" 1 'method-call-lock t)
                                      ("\\([=+-/*><]\\)" . 'operator-lock)))))

;; set path
(setenv "PYTHONPATH" (shell-command-to-string "$SHELL --login -c 'echo -n $PYTHONPATH'"))

(provide 'python-config)
;;; python-config.el ends here
