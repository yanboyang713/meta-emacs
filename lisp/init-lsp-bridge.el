;;; init-lsp-bridge.el --- Configuration for lsp-bridge

;; Filename: init-lsp-bridge.el
;; Description: Configuration for display line number
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-08-26 02:03:32
;; Version: 0.1
;; Last-Updated: 2018-08-26 02:03:32
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-lsp-bridge.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Configuration for display line number
;;

;;; Installation:
;;
;; Put init-lsp-bridge.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
(use-package yasnippet
  :straight t
  :hook ((lsp-mode . yas-minor-mode)))

(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))
  :init
  (global-lsp-bridge-mode))
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-lsp-bridge)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-lsp-bridge RET
;;

;;; Change log:
;;
;; 2018/08/26
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'lsp-bridge)
(require 'lsp-bridge-jdtls)

;;; Code:

(setq lsp-bridge-enable-completion-in-minibuffer t)
(setq lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
(setq lsp-bridge-enable-with-tramp t)
(setq lsp-bridge-enable-org-babel t)
(setq acm-enable-quick-access t)
(setq acm-backend-yas-match-by-trigger-keyword t)
(setq acm-enable-tabnine nil)
(setq acm-enable-codeium t)

(global-lsp-bridge-mode)

;; 打开日志，开发者才需要
;; (setq lsp-bridge-enable-log t)

(setq lsp-bridge-get-multi-lang-server-by-project
      (lambda (project-path filepath)
        ;; If typescript file include deno.land url, then use Deno LSP server.
        (save-excursion
          (when (string-equal (file-name-extension filepath) "ts")
            (dolist (buf (buffer-list))
              (when (string-equal (buffer-file-name buf) filepath)
                (with-current-buffer buf
                  (goto-char (point-min))
                  (when (search-forward-regexp (regexp-quote "from \"https://deno.land") nil t)
                    (return "deno")))))))))

;; Support jump to define of EAF root from EAF application directory.
;;(setq lsp-bridge-get-project-path-by-filepath
      ;;(lambda (filepath)
        ;;(when (string-prefix-p (expand-file-name "~/lazycat-emacs/site-lisp/extensions/emacs-application-framework/app") filepath)
          ;;(expand-file-name "~/lazycat-emacs/site-lisp/extensions/emacs-application-framework/"))))


(provide 'init-lsp-bridge)

;;; init-lsp-bridge.el ends here
