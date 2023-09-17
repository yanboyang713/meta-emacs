;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "27.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; Add user emacs dir = lisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Bootstrap config

(setq custom-file (locate-user-emacs-file "custom.el"))

;;(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;;(require 'init-elpa)      ;; Machinery for installing required packages

(require 'init-straight-package)

;;;;;;;;;;;;;; 
;; STARTUP  ;;
;;;;;;;;;;;;;;

;; Starting buffer
(setq initial-buffer-choice t)

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


(require 'init-evil)
(require 'init-org)
(require 'init-spelling)

;;;;;;;;;;;;;;;;
;; Completion ;;
;;;;;;;;;;;;;;;;

(use-package company
  :straight t
  :config
  (global-company-mode)
  (setq company-show-numbers            t
	company-minimum-prefix-length   1
	company-idle-delay              1.5
	company-backends
	'((company-files          ; files & directory
	   company-keywords       ; keywords
	   company-capf           ; what is this?
	   company-yasnippet)
	  (company-abbrev company-dabbrev))))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

;;;;;;;;;;;;;
;; R-stuff ;;
;;;;;;;;;;;;;
(use-package ess
  :straight t
  :hook
  (ess-r-mode . electric-pair-mode)
  (inferior-ess-r-mode . electric-pair-mode))

(use-package ess-view-data
  :straight t)

;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;

;; chat-gpt
(use-package shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package chatgpt-shell
  :requires shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el")))

(setq chatgpt-shell-openai-key
      (auth-source-pick-first-password :host "api.openai.com"))

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

;;;;;;;;;;;;;;;;
;; Javascript ;;
;;;;;;;;;;;;;;;;

(use-package js2-mode
  :straight t)

(use-package skewer-mode
  :straight t
  :after js2-mode)

(use-package npm-mode
  :straight t)

(use-package nodejs-repl
  :straight t)

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

(require 'init-python)

;;;;;;;;;;
;; Rust ;;
;;;;;;;;;;

(use-package rustic
  :straight t
  :hook
  (rustic-mode . electric-pair-mode))

;;;;;;;;;
;; C++ ;;
;;;;;;;;;

(add-hook 'c++-mode-hook #'electric-pair-mode)
(add-hook 'c-mode-hook #'electric-pair-mode)

;;;;;;;;;;
;; YAML ;;
;;;;;;;;;;

(use-package yaml-mode
  :straight t)


;;;;;;;;;;;;;;;;;
;; Common Lisp ;;
;;;;;;;;;;;;;;;;;

(use-package sly
  :straight t
  :config
  (setq inferior-lisp-program (executable-find "sbcl")))

;;;;;;;;;;;;;;;
;; lsp stuff ;;
;;;;;;;;;;;;;;;

;; lsp
(use-package lsp-mode
  :straight t
  :commands lsp
  :config
  (setq gc-cons-threshold (* 100 1024 1024)
	read-process-output-max (* 1024 1024)
	treemacs-space-between-root-nodes nil
	company-idle-delay 0.0
	company-minimum-prefix-length 1
	lsp-idle-delay 0.1
	lsp-lens-enable nil) ;This resolves extreme cpu use 
  :hook ((c-mode . lsp)
	 (c++-mode . lsp)
	 (python-mode . lsp)
	 (js2-mode . lsp)
	 (ess-r-mode . lsp)
	 (inferior-ess-r-mode . lsp)
	 (lsp-mode . evil-normalize-keymaps)))

(use-package lsp-ui
  :straight t
  :commands (lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil
	lsp-ui-doc-delay 0.5))

;(use-package lsp-ivy
;  :straight t
;  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-errors-list)

(use-package yasnippet
  :straight t
  :hook ((lsp-mode . yas-minor-mode)))

(use-package ccls
  :after lsp-mode
  :straight t
  :config
  (setq ccls-executable "/usr/bin/ccls"
	ccls-initialization-options
	'(:index (:comments 2) :completion (:detailedLabel t))))

(use-package modern-cpp-font-lock
  :straight t)

(use-package disaster
  :straight t)

;; flycheck
(use-package flycheck
  :straight t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function
	#'flycheck-display-error-messages-unless-error-list
	flycheck-indication-mode nil))

(use-package flycheck-pos-tip
  :straight t
  :after flycheck
  :config
  (flycheck-pos-tip-mode))

;;;;;;;;;;;
;; Looks ;;
;;;;;;;;;;;

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

(require 'init-keybindings)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
