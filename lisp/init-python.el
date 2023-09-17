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
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  (pyvenv-workon "3.10.10") ;; Default venv
  (pyvenv-tracking-mode 1))

(provide 'init-python)
;;; init-oython.el ends here
