;;; init-utilities.el --- utilities -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Undo
(use-package undo-fu
  :straight t)

(use-package undo-fu-session
  :straight t
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :init
  (undo-fu-session-global-mode))

;; Which key
(use-package which-key
  :straight t
  :config
  (which-key-mode)
  (setq which-key-popup-type 'minibuffer
	max-mini-window-height 0.5))

;; Focus
(use-package focus
  :straight t)

;; auth-source
(use-package auth-source
  :straight t
  :config (setq auth-sources '("~/.authinfo.gpg")))


;; rainbow delimiters
(use-package rainbow-delimiters
  :straight t
  :hook ((lisp-mode . rainbow-delimiters-mode)
	 (emacs-lisp-mode . rainbow-delimiters-mode)
	 (sly-mrepl-mode . rainbow-delimiters-mode)
	 (ess-r-mode . rainbow-delimiters-mode)
 	 (inferior-ess-r-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :straight t)

;; word count
(use-package wc-mode
  :straight t
  :config
  (add-to-list 'global-mode-string '("" wc-buffer-stats)))

;; use trash instead of rm
(setq delete-by-moving-to-trash t)

;; Get rid of stupid sound
(setq visible-bell 1)

;; Auto-follow symbolic links
(setq vc-follow-symlinks t)

;; Get rid of the annoying backup files
(setq make-backup-files nil) 

;; Vertico
(use-package vertico
  :straight t
  :init
  (vertico-mode)
  :config
  (setq read-file-name-completion-ignore-case t
	read-buffer-completion-ignore-case t
	completion-ignore-case t
	vertico-resize nil))

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :straight t
  :after marginalia
  :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Line numbers
(global-display-line-numbers-mode)
(menu-bar--display-line-numbers-mode-relative)

;; ibuffer
(use-package ibuffer
  :straight t
  :config
  (add-hook 'ibuffer-mode-hook
	    #'(lambda ()
		(ibuffer-switch-to-saved-filter-groups "home")))
  (setq ibuffer-saved-filter-groups
	(quote (("home"
		 ("dired" (mode . dired-mode))
		 ("emacs-config" (or (filename . "init.el")
				     (filename . "keybinds.el")))
		 ("Email" (name . "^\\*mu4e-main\\*$"))
		 ("emacs" (or
			   (name . "^\\*scratch\\*$")
			   (name . "^\\*Messages\\*$")
			   (name . "^\\*straight-process\\*$")
			   (name . "^\\*GNU Emacs*\\*$")))
		 ("Help" (or (name . "\*Help\*")
			     (name . "\*Apropos\*")
			     (name . "\*info\*")))
		 ("Org" (or (mode . org-mode)
			    (filename . "OrgMode"))))))))

;; Treemacs
(use-package treemacs
  :straight t)

(use-package treemacs-evil
  :straight t
  :after treemacs)

(use-package magit
  :straight t)

;; Mode-line
(use-package doom-modeline
  :straight t
  :init
  (doom-modeline-mode 1))
;; (use-package simple-modeline
;;   :straight t
;;   :hook (after-init . simple-modeline-mode))

;; Multiple cursors
(use-package multiple-cursors
  :straight t)

;; gdb
(setq gdb-many-windows t)

(provide 'init-utilities)

;;; init-utilities.el ends here
