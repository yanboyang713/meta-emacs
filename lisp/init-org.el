;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(require 'init-publish)

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
	org-agenda-files '("~/org/refile.org"
			   "~/org/org-roam/projects/service-migration.org")
	org-refile-use-outline-path 'file
	org-outline-path-complete-in-steps nil
	org-refile-allow-creating-parent-nodes (quote confirm)
	
	;; hugo
	org-hugo-base-dir "/home/yanboyang713/quartz"
	org-hugo-front-matter-format "yaml"

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
	       '("IEEEtran"
		 "\\documentclass[conference]{IEEEtran}"
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
[
(use-package org-ref
  :straight t
  :after org
  :custom
  (org-ref-default-bibliography "~/org/library.bib")
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
]

;; All-the-icons (for pretty symbols in citar)
(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

;; Org-cite and Citar configuration using straight.el
(use-package citar
  :straight t
  :after oc
  :custom
  (org-cite-global-bibliography '("~/org/library.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography '("~/org/library.bib"))
  (citar-notes-paths '("~/org/org-roam/references/"))
  (citar-symbols
   `((file ,(all-the-icons-faicon "file-pdf-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
     (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
     (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (citar-symbol-separator "  "))

;; citar-org-roam only offers the citar-org-roam-note-title-template variable
;; for customizing the contents of a new note and no way to specify a custom
;; capture template. And the title template uses citar's own format, which means
;; we can't run arbitrary functions in it.
;;
;; Left with no other options, we override the
;; citar-org-roam--create-capture-note function and use our own template in it.
(defun dh/citar-org-roam--create-capture-note (citekey entry)
    "Open or create org-roam node for CITEKEY and ENTRY."
    ;; adapted from https://jethrokuan.github.io/org-roam-guide/#orgc48eb0d
    (let ((title (citar-format--entry
                  citar-org-roam-note-title-template entry)))
      (org-roam-capture-
       :templates
       '(("r" "reference" plain "%?" :if-new
          (file+head
           "%(concat
 (when citar-org-roam-subdir (concat citar-org-roam-subdir \"/\")) \"${citekey}.org\")"
           "#+title: ${title}\n\n#+begin_src bibtex\n%(dh/citar-get-bibtex citekey)\n#+end_src\n")
          :immediate-finish t
          :unnarrowed t))
       :info (list :citekey citekey)
       :node (org-roam-node-create :title title)
       :props '(:finalize find-file))
      (org-roam-ref-add (concat "@" citekey))))

;; citar has a function for inserting bibtex entries into a buffer, but none for
;; returning a string. We could insert into a temporary buffer, but that seems
;; silly. Plus, we'd have to deal with trailing newlines that the function
;; inserts. Instead, we do a little copying and implement our own function.
(defun dh/citar-get-bibtex (citekey)
    (let* ((bibtex-files
            (citar--bibliography-files))
           (entry
            (with-temp-buffer
              (bibtex-set-dialect)
              (dolist (bib-file bibtex-files)
                (insert-file-contents bib-file))
              (bibtex-search-entry citekey)
              (let ((beg (bibtex-beginning-of-entry))
                    (end (bibtex-end-of-entry)))
                (buffer-substring-no-properties beg end)))))
      entry))

(advice-add #'citar-org-roam--create-capture-note :override #'dh/citar-org-roam--create-capture-note)

(defun dh/org-cite-export-bibliography-advice (fn keyword _ info)
    (if (org-cite-list-keys info)
        (funcall fn keyword nil info)))

;; The CSL style we use causes an error when trying to export an empty bibliography. Wrap the relevant function to
;; prevent that from happening.
(advice-add #'org-cite-export-bibliography :around #'dh/org-cite-export-bibliography-advice)

(defun dh/org-roam-node-directory (node)
    (string-remove-suffix
     "/"
     (string-remove-prefix
      "/"
      (string-remove-prefix
       org-roam-directory
       (file-name-directory (org-roam-node-file node))))))

(defun dh/org-roam-articles ()
    (cl-remove-if-not
     (lambda (node)
       (string= "article" (cdr (assoc-string "KIND" (org-roam-node-properties node)))))
     (org-roam-node-list)))

(defun dh/org-roam-to-hugo (section files)
    "Call `org-hugo-export-to-md' on all Org FILES.
All files have to be in `org-roam-directory'. Output is written
relative to SECTION in `org-hugo-base-dir'. Org files in
subdirectories of `org-roam-directory' will get matching
subdirectories underneath SECTION."
    (mapcar
     (lambda (node)
       (with-current-buffer (find-file-noselect (org-roam-node-file node))
         (let ((org-hugo-section (file-name-concat section (dh/org-roam-node-directory node))))
           (org-hugo-export-to-md))))
     files))

(defun dh/org-insert-date-keyword ()
    (org-roam-set-keyword "date" (format-time-string "[%Y-%m-%d %a]" (current-time))))

(defun dh/org-export-before-parsing (backend)
    (when (string= backend "hugo")
      (org-roam-set-keyword
       "hugo_lastmod"
       (format-time-string "%Y-%m-%d" (file-attribute-modification-time (file-attributes (buffer-file-name)))))))

;; (dh/org-roam-to-hugo "articles" (dh/org-roam-articles))

(defun my/org-roam-export-all ()
  "Export all Org-roam files containing a #+title: property to Hugo-compatible Markdown.
If an error occurs during the export of a file, log the error and continue with the next file."
  (interactive)
  ;; Ensure Org-roam and ox-hugo are loaded
  (require 'org-roam)
  (require 'ox-hugo)
  ;; Iterate over all Org-roam files
  (dolist (file (org-roam-list-files))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      ;; Check if the file contains a #+title: property
      (when (re-search-forward "^#\\+title:" nil t)
        ;; Attempt to export the file to Markdown using ox-hugo
        (condition-case err
            (org-hugo-export-wim-to-md)
          (error
           (message "Error exporting file %s: %s" file (error-message-string err))))))))

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
  (setq org-roam-directory "~/org/org-roam/references/")
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
			    "#+title: ${title}\n#+date: %<%Y-%m-%d>\n")
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
        org-roam-ui-update-on-save t)
  ;; Enable org-roam-ui-mode globally.
  (org-roam-ui-mode 1))

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
  (bibtex-completion-bibliography '("~/org/library.bib"))
  (reftex-default-bibliography '("~/org/library.bib"))
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

;; Update last modified date for ox-hugo export
(with-eval-after-load 'org
  (setq time-stamp-active t
        time-stamp-start "#\\+hugo_lastmod:[ \t]*"
        time-stamp-end "$"
        time-stamp-format "[%Y-%m-%d]")
  (add-hook 'before-save-hook 'time-stamp)
  ;; Automatically enable `org-hugo-auto-export-mode` in Org-mode
  (add-hook 'org-mode-hook #'org-hugo-auto-export-mode)
  )

;; Install and configure `ox-hugo`
(use-package ox-hugo
  :straight t
  :after ox
  :config
  (require 'ox-hugo)
  (defun my/hugo-strip-relref-directory (text backend _info)
    "Trim directory components from Hugo relref shortcodes in exported TEXT."
    (if (and (eq backend 'hugo)
             (string-match "{{< relref" text))
        (replace-regexp-in-string
         "{{< relref \"\\(?:../\\)*[^\"/]+/\\([^\"/]+\\)\" >}}"
         "{{< relref \"\\1\" >}}"
         text)
      text))
  (add-hook 'org-export-filter-link-functions #'my/hugo-strip-relref-directory)
  (defun my/hugo-strip-org-todo-spans (text backend _info)
    "Remove Org TODO span wrappers from exported TEXT when targeting Hugo."
    (if (eq backend 'hugo)
        (replace-regexp-in-string
         "<span class=\"org-todo[^>]*>\\([^<]+\\)</span>"
         "\\1"
         text)
      text))
  (add-hook 'org-export-filter-final-output-functions #'my/hugo-strip-org-todo-spans))

(use-package nov
  :straight t
  :mode ("\\.epub\\'" . nov-mode))

(with-eval-after-load 'undo-fu-session
  (setq undo-fu-session-incompatible-major-modes
        '(org-mode org-roam-mode ox-hugo-mode)))

(provide 'init-org)
;;; init-org.el ends here
