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
(straight-use-package 'markdown-mode)

(use-package yasnippet
:straight t
:hook ((lsp-mode . yas-minor-mode)))

(use-package lsp-bridge
  :straight (:type git :host github :repo "manateelazycat/lsp-bridge"
                   :files (:defaults
                           "*")
                   :includes (acm
                              core
                              langserver
                              multiserver
                              resources)
		   :build (:not compile))
  :init
  (global-lsp-bridge-mode)
  :config

  ;; disable tabnine: it's not open source
  (setq acm-enable-tabnine nil)
  (setq lsp-bridge-python-command "/usr/bin/python3")

  ;; enable signature help in posframe
  (setq lsp-bridge-enable-signature-help t)
  (setq lsp-bridge-signature-help-fetch-idle 0.3)
  (setq lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
  (setq lsp-bridge-signature-show-with-frame-position 'point)

  ;; combine lsp-bridge with orderless
  (setq acm-candidate-match-function 'orderless-flex)
  (setq acm-backend-lsp-candidate-min-length 1)

  ;; small QoL
  (setq acm-enable-quick-access t)

  ;; language servers
  (setq lsp-bridge-c-lsp-server "ccls")
  (setq lsp-bridge-python-lsp-server "pyright")
  ;; (global-lsp-bridge-mode)
  :hook (prog-mode . lsp-bridge-mode)
  ) ;; global-lsp-bridge-mode maybe?

;; needed for terminal, there is a visual bug
;; if loading them in graphics mode
(unless (or (display-graphic-p) (daemonp))
  (require 'popon-setup)
  (require 'acm-terminal-setup))

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


(provide 'init-lsp-bridge)

;;; init-lsp-bridge.el ends here
