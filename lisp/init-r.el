;;; init-r.el --- R language support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ess
  :straight t
  :hook
  (ess-r-mode . electric-pair-mode)
  (inferior-ess-r-mode . electric-pair-mode))

(use-package ess-view-data
  :straight t)


(provide 'init-r)
;;; init-r.el ends here
