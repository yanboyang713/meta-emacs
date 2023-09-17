;;; init-evil.el --- evil configure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Vim Bindings
(use-package evil
  :straight t
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :config
  (evil-mode 1)
  :init
  (setq evil-want-keybinding nil
	evil-undo-system 'undo-fu
	evil-want-C-u-scroll t))


;; Vim Bindings Everywhere else
(use-package evil-collection
  :straight t
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

(provide 'init-evil)
;;; init-evil.el ends here
