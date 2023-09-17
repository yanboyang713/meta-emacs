;;; init-c++.el --- C++ support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-hook 'c++-mode-hook #'electric-pair-mode)
(add-hook 'c-mode-hook #'electric-pair-mode)

(provide 'init-c++)
;;; init-c++.el ends here
