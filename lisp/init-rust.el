;;; init-rust.el --- rust support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rustic
  :straight t
  :hook
  (rustic-mode . electric-pair-mode))

(provide 'init-rust)
;;; init-rust.el ends here
