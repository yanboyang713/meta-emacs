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

;; Ensure emacsclient can connect (required by org-roam-mcp)
(unless noninteractive
  (require 'server)
  (unless (server-running-p)
    (server-start)))

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


(require 'init-lsp-bridge)
(require 'init-evil)
(require 'init-org)
(require 'init-spelling)
;;(require 'init-completion)
(require 'init-r)

(require 'init-utilities)

(require 'init-javascript)
(require 'init-python)

(require 'init-rust)

(require 'init-c++)
(require 'init-yaml)
(require 'init-lisp)
;;(require 'init-lsp)
(require 'init-ui)
(require 'init-keybindings)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here

;; Use qutebrowser for opening URLs from Emacs
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser")

;; Ensure org-roam-ui opens in qutebrowser
(with-eval-after-load 'org-roam-ui
  (setq org-roam-ui-browser-function 'browse-url-generic
        browse-url-generic-program "qutebrowser"))

(with-eval-after-load 'org-roam-ui
  (setq org-roam-ui-browser-function
        (lambda (url)
          (start-process "qutebrowser" nil "qutebrowser" "--target" "tab" url))))

;; Use qutebrowser via browse-url-generic (user request)
(setq browse-url-browser-function '''browse-url-generic
      browse-url-generic-program "qutebrowser"
      org-roam-ui-browser-function '''browse-url-generic)

;; User-requested browser settings
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser"
      org-roam-ui-browser-function 'browse-url-generic)

;; Ensure org-roam-ui uses browse-url-generic (override any earlier hooks)
(with-eval-after-load 'org-roam-ui
  (setq org-roam-ui-browser-function 'browse-url-generic))
