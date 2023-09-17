;;; init-ui.el --- UI -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Set theme
 (use-package monokai-theme
   :straight t
   :config
   (setq monokai-background "#151515"
	 monokai-green "#98C379")
   (load-theme 'monokai t))

;; Set font
(add-to-list 'default-frame-alist '(font . "RobotoMono Nerd Font 13"))
(set-face-attribute 'default t :font "RobotoMono Nerd Font 13") 

;; Tilde fringe
(use-package vi-tilde-fringe
  :straight t
  :config
  (global-vi-tilde-fringe-mode))

;; All the icons in dired
(use-package all-the-icons-dired
  :straight t
  :after (all-the-icons)
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(provide 'init-ui)
;;; init-ui.el ends here
