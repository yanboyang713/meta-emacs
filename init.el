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

(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
(require 'init-elpa)      ;; Machinery for installing required packages
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

;;;;;;;;;;;;;;;
;; VIM STUFF ;;
;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;
;; Org-mode ;;
;;;;;;;;;;;;;;
(use-package org
  :straight t (:type built-in)
  :hook ((org-mode . flyspell-mode)
	 (org-mode . +org-enable-auto-reformat-tables-h)
	 (org-mode . writegood-mode )
	 (org-mode . visual-line-mode))
  :config
  (setq org-directory "~/org/"
	org-hide-emphasis-markers t
	org-log-done 'time
	org-agenda-window-setup "only window"
	org-default-notes-file "~/org/refile.org"
	org-refile-targets (quote ((nil :maxlevel . 5)
				   (org-agenda-files :maxlevel . 5)))
	org-agenda-files (quote("~/org/"
				"~/org/synced/"
				"~/org/org-roam/"
				"~/org/org-roam/daily/"
				"~/org/org-roam/references/"
				))
	org-refile-use-outline-path 'file
	org-outline-path-complete-in-steps nil
	org-refile-allow-creating-parent-nodes (quote confirm)

	;; org keyword related stuff
	org-todo-keywords
	(quote ((sequence
		 "TODO(t)"
		 "PROJ(p)"
		 "LOOP(r)"
		 "STRT(s)"
		 "IDEA(i)"
		 "NEXT(n)"
		 "|"
		 "DONE(d)")
		(sequence
		 "WAIT(w@/!)"
		 "HOLD(h@/!)"
		 "|"
		 "KILL(k@/!)")
		(sequence
		 "[ ](T)"
		 "[-](S)"
		 "[?](W)"
		 "|"
		 "[X](D)"
		 )))

	org-todo-keyword-faces
	(quote (
		("NEXT" +-lock-constant-face bold)))

	org-todo-state-tags-triggers
	(quote (("KILL" ("KILL" . t))
		("WAIT" ("WAIT" . t))
		("HOLD" ("WAIT") ("HOLD" . t))
		(done ("WAIT") ("HOLD"))
		("TODO" ("WAIT") ("KILL") ("HOLD"))
		("NEXT" ("WAIT") ("KILL") ("HOLD"))
		("DONE" ("WAIT") ("KILL") ("HOLD"))))

	;; org capture related stuff
	org-capture-templates
	(quote (("r" "respond" entry (file+headline "~/org/refile.org" "Emails")
		 "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n")
		("p" "project" entry (file+headline "~/org/refile.org" "Projects")
		 "* PROJ %?\n%U\n%a\n")
		("t" "todo" entry (file+headline "~/org/refile.org" "Tasks")
		 "* TODO %?\nSCHEDULED: %t\n%U\n%a\n")
		("i" "idea" entry (file+headline "~/org/refile.org" "Ideas")
		 "* IDEA %?\n%U\n%a\n")
		("e" "external" entry (file+headline "~/org/refile.org" "External")
		 "* TODO %?\nSCHEDULED: %t\n%U\n%a\n %(progn (setq kk/delete-frame-after-capture 1) \"\")")
		)))

  ;; Caldav sync
  (setq diary-location "~/.local/share/diary/"
	calendars '(("outlook" . "http://localhost:1080/users/45995wsp@eur.nl/calendar/"))
	org-agenda-include-diary t
	diary-file "~/.local/share/diary/outlook")

  ;; Kill capture frame
  (defvar kk/delete-frame-after-capture 0 "Whether to delete the last frame after the current capture")

  (advice-add 'org-capture-finalize :after 'kk/delete-frame-if-neccessary)
  (advice-add 'org-capture-kill :after 'kk/delete-frame-if-neccessary)
  (advice-add 'org-capture-refile :after 'kk/delete-frame-if-neccessary)

  ;; Set up org-mode export stuff
  (setq org-latex-to-mathml-convert-command
	"java -jar %j -unicode -force -df %o %I"
	org-latex-to-mathml-jar-file
	"/home/wouter/Tools/math2web/mathtoweb.jar"))

;; Add latex classes; needs to be done after loading ox-latex
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
	       '("apa6"
		 "\\documentclass{apa6}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
	       '("report"
		 "\\documentclass{report}"
		 ("\\chapter{%s}" . "\\chapter*{%s}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

  (add-to-list 'org-latex-classes
	       '("koma-article"
		 "\\documentclass{scrartcl}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
	       '("memoir"
		 "\\documentclass{memoir}"
		 ("\\book{%s}" . "\\book*{%s}")
		 ("\\part{%s}" . "\\part*{%s}")
		 ("\\chapter{%s} .\\chapter*{%s}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
	       '("paper"
		 "\\documentclass{paper}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; Set up org-ref stuff
(use-package org-ref
  :straight t
  :after org
  :custom
  (org-ref-default-bibliography "/home/wouter/Tools/Zotero/bibtex/library.bib")
  (org-ref-default-citation-link "citep")
  (org-ref-insert-link-function 'org-ref-insert-link-hydra/body)
  (org-ref-insert-cite-function 'org-ref-cite-insert-helm)
  (org-ref-insert-label-function 'org-ref-insert-label-link)
  (org-ref-insert-ref-function 'org-ref-insert-ref-link)
  (org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))

  ;(setq org-ref-completion-library 'org-ref-ivy-cite
  (setq	org-export-latex-format-toc-function 'org-export-latex-no-toc
	org-ref-get-pdf-filename-function
	(lambda (key) (car (bibtex-completion-find-pdf key)))
	org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point
	;; For pdf export engines
	org-latex-pdf-process (list "latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -bibtex -f -output-directory=%o %f")
	org-ref-notes-function 'orb-edit-notes))

;; org-noter stuff
(use-package org-noter
  :straight t
  :after pdf-tools
  :config
  (setq org-noter-notes-search-path "~/org/org-roam/references/"
	org-noter-hide-other nil
	org-noter-separate-notes-from-heading t
	org-noter-always-create-frame t))

;; org-roam
(use-package org-roam
  :straight t
  :after org
  :hook (org-roam-mode . visual-line-mode)
  :config
  (setq org-roam-directory "~/org/org-roam/")
  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
		 (display-buffer-in-side-window)
		 (visual-line-mode)
		 (side . right)
		 (slot . 0)
		 (window-width . 0.33)
		 (window-parameters . ((no-other-window . t)
				       (no-delete-other-windows . t)))))
  
  (org-roam-db-autosync-mode)
  ;; Let's set up some org-roam capture templates
  (setq org-roam-capture-templates
	(quote (("d" "default" plain
		 "%?"
		 :target
		 (file+head "%<%Y-%m-%d-%H%M%S>-${slug}.org"
			    "#+title: ${title}\n")
		 :unnarrowed t)
		("j" "journal article" plain
		 (file "~/org/org-roam/templates/orb-capture")
		 :target
		 (file+head "references/${citekey}.org" "#+title: ${title}\n#+filetags: journal_article\n"))
		("c" "book chapter" plain
		 (file "~/org/org-roam/templates/orb-capture")
		 :target
		 (file+head "references/${citekey}.org" "#+title: ${title}\n#+filetags: book_chapter\n"))
		("b" "book" plain
		 (file "~/org/org-roam/templates/orb-capture")
		 :target
		 (file+head "references/${citekey}.org" "#+title: ${title}\n#+filetags: book\n"))
		("o" "other reference" plain
		 (file "~/org/org-roam/templates/orb-capture")
		 :target
		 (file+head "references/${citekey}.org" "#+title: ${title}\n"))
		)))

  ;; And now we set necessary variables for org-roam-dailies
  (setq org-roam-dailies-directory "daily/"
	org-roam-dailies-capture-templates
	'(("d" "default" entry
	   "* %?"
	   :target
	   (file+head "%<%Y-%m-%d>.org"
		      "#+title: %<%Y-%m-%d>\n")))))

;; For org-roam-ui
(use-package websocket
  :straight t
  :after org-roam)
(use-package org-roam-ui
  :straight t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t))

;; consult-org-roam
(use-package consult
  :straight t)

(use-package consult-org-roam
  :straight t
  :after org-roam
  :config
  (setq consult-org-roam-mode 1
	consult-org-roam-grep-func #'consult-ripgrep))

;; evil-org
(use-package evil-org
  :straight t
  :after org
  :hook (orgmode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; org-modern
(use-package org-modern
  :straight t
  :after org
  :config
  ;; Minimal UI
  (package-initialize)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq org-auto-align-tags nil
	org-tags-column 0
	org-catch-invisible-edits 'show-and-error
	org-special-ctrl-a/e t
	org-insert-heading-respect-content t
	;; Org styling, hide markup etc.
	org-hide-emphasis-markers t
	org-pretty-entities t
	org-ellipsis "…"
	;; Agenda styling
	org-agenda-tags-column 0
	org-agenda-block-separator ?─
	org-agenda-time-grid
	'((daily today require-timed)
	  (800 1000 1200 1400 1600 1800 2000)
	  " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
	org-agenda-current-time-string
	"⭠ now ─────────────────────────────────────────────────")
  (global-org-modern-mode))

;; org-reveal
(use-package ox-reveal
  :straight t
  :config
  (setq org-reveal-root "/home/wouter/Tools/reveal.js"))

;; org-ai
(use-package org-ai
  :straight t
  :hook (org-mode-hook . org-ai-mode)
  :init
  (org-ai-mode)
  (org-ai-global-mode))

;;;;;;;;;;;;;;
;; Spelling ;;
;;;;;;;;;;;;;;
(with-eval-after-load "flyspell"
  (setq ispell-program-name "hunspell"
	ispell-list-command "--list"
	ispell-dictionary "en_GB"))

(use-package flyspell-correct-popup
  :straight t
  :after flyspell)

(use-package langtool
  :straight t
  :config
  (setq langtool-java-classpath
	"/usr/share/languagetool:/usr/share/java/languagetool/*"
	langtool-default-language "en-GB"))

(use-package writegood-mode
  :straight t)

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

;; general (keybinds)
(use-package general
  :straight t
  :config
  (load "~/.config/emacs/keybinds.el")) 

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

(use-package marginalia
  :straight t
  :after (vertico)
  :general
  (:keymaps 'minibuffer-local-map
	    "M-A" 'marginalia-cycle)
  :custom
  (marginalia-align 'right)
  :init
  (marginalia-mode))

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

;; This is to use pdf-tools instead of doc-viewer
(use-package pdf-tools
  :straight t
  :hook (pdf-view-mode . (lambda() (display-line-numbers-mode 0)))
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;; helm-bibtex
(use-package helm-bibtex
  :straight t
  :hook (helm-major-mode . (lambda() (display-line-numbers-mode 0)))
  :custom
  (bibtex-completion-bibliography '("~/library.bib"))
  (reftex-default-bibliography '("~/library.bib"))
  (bibtex-completion-pdf-field "file")
  :config
  (setq bibtex-completion-display-formats '((t . "${author:36} ${title:100} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")))
  :hook (Tex . (lambda () (define-key Tex-mode-map "\C-ch" 'helm-bibtex))))

;; org-roam-bibtex stuff
(use-package org-roam-bibtex
  :straight t
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
	'("citekey" "title" "url" "author-or-editor" "keywords" "file")
	orb-process-file-keyword t
	orb-file-field-extensions '("pdf")))
(org-roam-bibtex-mode) ;;Only gets loaded properly when I put it here.

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

(use-package anaconda-mode
  :straight t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package pyvenv
  :straight t
  :config
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  (pyvenv-workon "3.10.10") ;; Default venv
  (pyvenv-tracking-mode 1))

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

;;;;;;;;;;;;;;;;;;;
;; New functions ;;
;;;;;;;;;;;;;;;;;;;
;(defun ws/verify-refile-target ()
;  "Exclude todo keywords with a done state"
;  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun getcal (url file)
  "Download ics file and add it to file"
  (let ((tmpfile (url-file-local-copy url)))
    (icalendar-import-file tmpfile file)
    (kill-buffer (car (last (split-string tmpfile "/"))))))

(defun getcals ()
  "Load a set of ICS calendars into Emacs diary files"
  (interactive)
  (mapcar #'(lambda (x)
	      (let ((file (concat diary-location (car x)))
		    (url (cdr x)))
		(message (concat "Loading " url " into " file))
		(find-file file)
		;; (flush-lines "^[& ]") ;; if you import ical as non marking
		(erase-buffer) ;; to avoid duplicating events
		(getcal url file)
		))
	  calendars))


(defun delete-visited-file (buffer-name)
  "Delete the file visited by the buffer named BUFFER-NAME."
  (interactive "bDelete file visited by buffer ")
  (let* ((buffer (get-buffer buffer-name))
	 (filename (buffer-file-name buffer)))
    (when buffer
      (when (and filename
		 (file-exists-p filename))
	(delete-file filename))
      (kill-buffer buffer))))

(defun kk/delete-frame-if-neccessary (&rest r)
  (cond
   ((= kk/delete-frame-after-capture 0) nil)
   ((> kk/delete-frame-after-capture 1)
    (setq kk/delete-frame-after-capture (- kk/delete-frame-after-capture 1)))
   (t
    (setq kk/delete-frame-after-capture 0)
    (delete-frame))))

;(defun ws/verify-refile-target ()
;  "Eclude todo keywords with a done state"
;  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
	 (key (car results))
	 (pdf-file (funcall org-ref-get-pdf-filename-function key)))
    (if (file-exists-p pdf-file)
	(find-file pdf-file)
      (message "No PDF found for %s" key))))

(defun org-export-latex-no-toc (depth)
  (when depth
    (format "%% Org-mode is exporting headings to %s levels.\n"
	    depth)))

(defun org-roam-capture-pdf-active-region ()
  (let* ((pdf-buf-name (plist-get org-capture-plist :original-buffer))
	 (pdf-buf (get-buffer pdf-buf-name)))
    (if (buffer-live-p pdf-buf)
	(with-current-buffer pdf-buf
	  (car (pdf-view-active-region-text)))
      (user-error "Buffer %S not alive" pdf-buf-name))))

;; Customize org-roam-minibuffer
(setq org-roam-node-display-template (concat "${title:100} "
					     (propertize "${tags:20}" 'face 'org-tag)))

;; I shamelessly copy-pasted these from doom emacs, because they are super useful.
(defun +org-realign-table-maybe-h ()
  "Auto-align table under cursor."
  (when (and org-table-automatic-realign (org-at-table-p) org-table-may-need-update)
    (let ((pt (point))
          (inhibit-message t))
      (if org-table-may-need-update (org-table-align))
      (goto-char pt))))

(defun +org-enable-auto-reformat-tables-h ()
  "Realign tables & update formulas when exiting insert mode (`evil-mode').
Meant for `org-mode-hook'."
  (when (featurep 'evil)
    (add-hook 'evil-insert-state-exit-hook #'+org-realign-table-maybe-h nil t)
    (add-hook 'evil-replace-state-exit-hook #'+org-realign-table-maybe-h nil t)
    (advice-add #'evil-replace :after #'+org-realign-table-maybe-a)))

(defun +org-realign-table-maybe-a (&rest _)
  "Auto-align table under cursor and re-calculate formulas."
  (when (eq major-mode 'org-mode)
    (+org-realign-table-maybe-h)))
