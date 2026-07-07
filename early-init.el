;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil)

;; Make `emacs --daemon` work in environments where `/run/user/<uid>/emacs`
;; is not writable/available (common in containers/sandboxes).
(setq server-auth-dir (expand-file-name "server/" user-emacs-directory))
(unless (file-directory-p server-auth-dir)
  (make-directory server-auth-dir t))
;; Emacs refuses to use an insecure server dir (must not be accessible by others).
(set-file-modes server-auth-dir #o700)

;; org-roam-mcp uses `emacsclient --server-file=...` (TCP auth file), so force
;; the Emacs server to use TCP and bind locally.
(setq server-use-tcp t)
(setq server-host "127.0.0.1")

;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
