;;; init-javascript.el --- javascript support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package js2-mode
  :straight t)

(use-package skewer-mode
  :straight t
  :after js2-mode)

(use-package npm-mode
  :straight t)

(use-package nodejs-repl
  :straight t)

(provide 'init-javascript)
;;; init-javascript.el ends here
