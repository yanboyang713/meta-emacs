;;; init-keybindings.el --- set keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'init-funcs)

(use-package general
  :straight t
  :config
  ;; Setup evil
(general-evil-setup)
(evil-collection-init)

(general-define-key
 :states '(normal visual insert motion emacs)
 :keymaps '(override global)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "" '(nil :wk "leader")
 "f" '(:ignore t :wk "file")
 "ff" '(find-file :wk "find file")
 "fd" '(delete-file :wk "delete file")

 "a" '(:ignore t :wk "AI")
 "ar" '(:ignore t :wk "AI on region")
 "arq" '(org-ai-on-region :wk "ask question about region")
 "ars" '(org-ai-summarize :wk "summarize region")
 "arp" '(chatgpt-shell-proofread-region :wk "proofread region")
 "af" '(org-ai-refactor-code :wk "AI refactor")
 "am" '(org-ai-switch-chat-model :wk "AI switch model")
 "aa" '(org-ai-open-account-usage-page :wk "AI usage")
 "ad" '(org-ai-open-request-buffer :wk "AI debug")
 "ap" '(org-ai-prompt :wk "AI prompt")
 "ae" '(org-ai-complete-block :wk "AI evaluate block")

 "b" '(:ignore t :wk "buffers")
 "bb" '(switch-to-buffer :wk "switch buffer")
 "bk" '(kill-buffer :wk "kill buffer")
 "bl" '(ibuffer :wk "list buffers")

 "w" '(:ignore t :wk "windows")
 "ww" '(evil-window-next :wk "next window")
 "wp" '(evil-window-prev :wk "previous window")
 "w|" '(evil-window-set-width :wk "set window width")
 "wq" '(evil-window-delete :wk "delete window"p)

 "o" '(:ignore t :wk "open")
 "om" '(mu4e :wk "email")
 "ol" '(helm-bibtex :wk "literature")
 "on" '(helm-bibtex-with-notes :wk "literature with notes")
 "oa" '(org-agenda :wk "agenda")
 "od" '(dired :wk "dired")
 "og" '(magit :wk "magit")
 "oc" '(chatgpt-shell :wk "chatGPT")

 "n" '(:ignore t :wk "notes")
 "np" '(org-noter :w "noter")
 "nr" '(:ignore t :wk "org-roam")
 "nrf" '(org-roam-node-find :wk "find note")
 "nrr" '(org-roam-buffer-toggle :wk "buffer")
 "nrb" '(consult-org-roam-backlinks :wk "browse backlinks")
 "nrs" '(consult-org-roam-search :wk "search")
 "nri" '(:ignore t :wk "insert")
 "nrin" '(org-roam-node-insert :wk "insert general note")
 "nric" '(citar-create-note :wk "citar reference note")
 "nrib" '(orb-insert-link :wk "insert bibliographic note")
 "nrd" '(:ignore t :wk "dailies")
 "nrdt" '(org-roam-dailies-goto-today :wk "today")
 "nrdd" '(org-roam-dailies-find-date :wk "pick date")
 
 "X" '(org-capture :wk "org capture")
 "U" '(straight-pull-all :wk "update all packages")
 "u" '(universal-argument :wk "universal argument")
 "c" '(compile :wk "compile")

 "i" '(:ignore t :wk "insert")
 "ic" '(insert-char :wk "character")

 "W" '(:ignore t :wk "Writing")
 "Wf" '(:ignore t :wk "focus")
 "Wff" '(focus-mode :wk "toggle focus-mode")
 "Wfc" '(focus-change-thing :wk "change focus")

 "t" '(:ignore t :wk "treemacs")
 "to" '(treemacs :wk "open browser")
 "tw" '(:ignore t :wk "workspaces")
 "tws" '(treemacs-switch-workspace :wk "switch workspace")
 "twe" '(treemacs-edit-workspaces :wk "edit workspaces"))

(general-define-key
 :keymaps '(vertico-map)
 "C-J" #'vertico-next-group
 "C-K" #'vertico-previous-group
 "C-j" #'vertico-next
 "C-k" #'vertico-previous)

(general-define-key
 :keymaps '(helm-map)
 "C-j" #'helm-next-line
 "C-k" #'helm-previous-line)

(general-define-key
 :states '(normal visual insert motion emacs)
 :keymaps '(override global)
 "C->" 'mc/mark-next-like-this
 "C-<" 'mc/mark-previous-like-this
 "C-!" 'mc/mark-all-like-this)

(general-define-key
 :keymaps 'flyspell-mode-map
 :states 'normal
 "C-;" 'flyspell-correct-wrapper
 "C-'" '+flyspell-add-word)

(global-set-key (kbd "C-c N")
                (lambda()(interactive)
                  (ispell-change-dictionary "nl_NL")
                  (flyspell-buffer)))
(global-set-key (kbd "C-c E")
		(lambda()(interactive)
		  (ispell-change-dictionary "en_GB")
		  (flyspell-buffer)))
(global-set-key (kbd "C-c U")
		(lambda()(interactive)
		  (ispell-change-dictionary "en_US")
		  (flyspell-buffer)))

(general-define-key
 :keymaps 'org-agenda-mode-map
 :states 'motion
 :prefix "SPC"
 "" '(nil :wk "leader")
 "m" '(:ignore t :wk "localleader")
 "mc" '(:ignore t :wk "clock")
 "mci" '(org-agenda-clock-in :wk "clock in")
 "mco" '(org-agenda-clock-out :wk "clock out")
 "md" '(:ignore t :wk "dates")
 "mds" '(org-agenda-schedule :wk "schedule")
 "mdd" '(org-agenda-deadline :wk "set deadline")
 "ms" '(:ignore t :wk "toggles")
 "msm" '(org-modern-mode :wk "org modern mode")
 "mt" '(:ignore t :wk "todo")
 "mtt" '(org-agenda-todo :wk "todo states")
 "mr" '(org-agenda-refile :wk "refile"))

(eval-after-load "org-mode"
  (general-define-key
   :keymaps 'org-mode-map
   "C-c [" nil
   "C-c ]" 'org-ref-insert-link
   "s-[" 'org-ref-insert-link-hydra/body
   "C-<return>" #'+org--insert-item-below
   "C-S-<return>" #'+org--insert-item-above))
(eval-after-load "org-mode"
  (general-define-key
    :keymaps 'org-mode-map
    :states 'normal
    "RET" 'org-open-at-point))

(general-define-key
 :keymaps 'org-mode-map
 :states '(normal visual insert replace emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "m" '(:ignore t :wk "localleader")
 "me" '(org-export-dispatch :wk "export")
 "mi" '(:ignore t :wk "insert")
 "mir" '(org-ref-insert-cite-link :wk "citation")
 "mc" '(:ignore t :wk "create")
 "mci" '(org-id-get-create :wk "org id")
 "mt" '(:ignore t :wk "todo")
 "mtt" '(org-todo :wk "todo states")
 "ms" '(:ignore t :wk "toggles")
 "mst" '(visual-line-mode :wk "visual line mode")
 "msm" '(org-modern-mode :wk "org modern mode")
 "msr" '(rainbow-mode :wk "rainbow mode")
 "md" '(:ignore t :wk "dates")
 "mds" '(org-schedule :wk "schedule")
 "mdd" '(org-deadline :wk "set deadline")
 "mc" '(:ignore t :wk "clock")
 "mci" '(org-clock-in :wk "clock in")
 "mco" '(org-clock-out :wk "clock out")
 "mr" '(org-refile :wk "refile"))

(eval-after-load "org-noter"
  (general-define-key
   :keymaps '(org-noter-doc-mode-map org-noter-notes-mode-map)
   "C-M-i" 'org-noter-insert-note
   "C-M-p" 'org-noter-insert-precise-note
   "C-M-k" 'org-noter-sync-prev-note
   "C-M-j" 'org-noter-sync-next-note
   "C-M-s" 'org-noter-create-skeleton
   "C-M-q" 'org-noter-kill-session))

(general-define-key
 :keymaps 'ess-r-mode-map
 :states '(normal visual insert replace emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "m" '(:ignore t :wk "localleader")
 "me" '(:ignore t :wk "evaluate")
 "mer" '(ess-eval-region :wk "region")
 "mel" '(ess-eval-line :wk "line")
 "meb" '(ess-eval-buffer :wk "buffer"))

(general-define-key
 :keymaps 'ess-rdired-mode-map
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "m" '(:ignore t :wk "localleader")
 "mv" '(ess-view-data-print :wk "view data"))

(general-define-key
 :keymaps 'ess-view-data-mode-map
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "m" '(:ignore t :wk "localleader")
 "ms" '(ess-view-data-sort :wk "sort")
 "mq" '(ess-view-data-quit :wk "quit"))

(general-define-key
 :states '(normal visual replace emacs)
 :keymaps '(lsp-mode-map c-mode-map)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "l" '(:ignore t :wk "lsp")

 "lw" '(:ignore t :wk "workspaces")
 "lwD" '(lsp-disconnect :wk "disconnect")
 "lwd" '(lsp-describe-session :wk "describe session")
 "lwq" '(lsp-workspace-shutdown :wk "shutdown server")
 "lwr" '(lsp-workspace-restart :wk "restart server")
 "lws" '(lsp :wk "start server")

 "l=" '(:ignore t :wk "format")
 "l==" '(lsp-format-buffer :wk"format buffer")
 "l=r" '(lsp-format-region :wk "format region")

 "lF" '(:ignore t :wk "folders")
 "lFa" '(lsp-workspace-folders-add :wk "add folder")
 "lFb" '(lsp-workspace-blacklist-remove :wk "un-blacklist folder")
 "lFr" '(lsp-workspace-folders-remove :wk "remove folder")

 "lT" '(:ignore t :wk "toggles")
 "lTD" '(lsp-modeline-diagnostics-mode :wk "toggle modeline diagnostics")
 "lTL" '(lsp-toggle-trace-io :wk "toggle log io")
 "lTS" '(lsp-ui-sideline-mode :wk "toggle sideline")
 "lTT" '(lsp-treemacs-sync-mode :wk "toggle treemacs integration")
 "lTa" '(lsp-modeline-code-actions-mode :wk "toggle modeline code actions")
 "lTb" '(lsp-headerline-breadcrumb-mode :wk "toggle breadcrumb")
 "lTd" '(lsp-ui-doc-mode :wk "toggle documentation popup")
 "lTf" '(lsp-toggle-on-type-formatting :wk "toggle on type formatting")
 "lTh" '(lsp-toggle-symbol-highlight :wk "toggle highlighting")
 "lTl" '(lsp-lens-mode :wk "toggle lenses")
 "lTs" '(lsp-toggle-signature-auto-activate :wk "toggle signature")
 
 "lg" '(:ignore t :wk "goto")
 "lga" '(xref-find-apropos :wk "find symbol in workspace")
 "lgd" '(lsp-find-declaration :wk "find declarations")
 "lge" '(lsp-treemacs-errors-list :wk "show errors")
 "lgg" '(lsp-find-definition :wk "find definitions")
 "lgh" '(lsp-treemacs-call-hierarchy :wk "call hierarchy")
 "lgi" '(lsp-find-implementation :wk "find implementations")
 "lgr" '(lsp-find-references :wk "find references")
 "lgt" '(lsp-find-type-definition :wk "find type definition")

 "lh" '(:ignore t :wk "help")
 "lhg" '(lsp-ui-doc-glance :wk "glance symbol")
 "lhh" '(lsp-describe-thing-at-point :wk "describe symbol at point")
 "lhs" '(lsp-signature-activate :wk "signature help")

 "lr" '(:ignore t :wk "refactoring")
 "lro" '(lsp-organize-imports :wk "organize imports")
 "lrr" '(lsp-rename :wk "rename")

 "la" '(:ignore t :wk "actions")
 "laa" '(lsp-execute-code-action :wk "code actions")
 "lah" '(lsp-document-highlight :wk "highlight symbol")
 "lal" '(lsp-avy-lens :wk "lens")

 ;; peeks
 "lG" '(:ignore t :wk "peeks")
 "lGg" '(lsp-ui-peek-find-definitions :wk "peek definitions")
 "lGi" '(lsp-ui-peek-find-implementation :wk "peek implementations")
 "lGr" '(lsp-ui-peek-find-references :wk "peek references")
 "lGs" '(lsp-ui-peek-find-workspace-symbol :wk "peek workspace symbol"))

)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
