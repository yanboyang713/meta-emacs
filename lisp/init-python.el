;;; init-python.el --- python programming -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package anaconda-mode
  :straight t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package pyvenv
  :straight t
  :config
  (let ((workon-home (expand-file-name "~/.pyenv/versions")))
    (setenv "WORKON_HOME" workon-home)
    (when (file-directory-p (expand-file-name "3.10.10" workon-home))
      (pyvenv-workon "3.10.10"))) ;; Default venv, when present
  (pyvenv-tracking-mode 1))

(provide 'init-python)
;;; init-oython.el ends here
