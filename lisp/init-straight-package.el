;;; init-straight-package.el --- Set-up straight package manger -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Straight package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

;; Use-package
(straight-use-package 'use-package)
(straight-use-package 'seq)
(straight-use-package 'compat)
(straight-use-package 'transient)

(defun yb/patch-transient-box-faces ()
  "Patch Transient face specs that this Emacs build rejects in GUI frames."
  (let ((transient-file
         (expand-file-name "straight/repos/transient/lisp/transient.el"
                           user-emacs-directory))
        (transient-elc
         (expand-file-name "straight/build/transient/transient.elc"
                           user-emacs-directory)))
    (when (file-readable-p transient-file)
      (with-temp-buffer
        (insert-file-contents transient-file)
        (goto-char (point-min))
        (when (search-forward ":line-width (-1 . -1)" nil t)
          (goto-char (point-min))
          (while (search-forward ":line-width (-1 . -1)" nil t)
            (replace-match ":line-width -1" t t))
          (write-region (point-min) (point-max) transient-file)
          (when (file-exists-p transient-elc)
            (delete-file transient-elc)))))))

(yb/patch-transient-box-faces)
(require 'seq)
(require 'compat)
(require 'transient)

(provide 'init-straight-package)
;;; init-straight-package.el ends here
