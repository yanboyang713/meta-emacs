;;; init-lisp.el --- commond LISP support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package sly
  :straight t
  :config
  (setq inferior-lisp-program (executable-find "sbcl")))


(provide 'init-lisp)
;;; init-lisp.el ends here
