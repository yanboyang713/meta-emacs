;;; init-ui.el --- UI -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Set theme
(use-package monokai-theme
  :straight t
  :init
  (setq monokai-background "#151515"
        monokai-green "#98C379")
  :config
  (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4
                  org-level-5 org-level-6 org-level-7 org-level-8))
    (when (facep face)
      (set-face-attribute face nil :inherit 'default)))
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
