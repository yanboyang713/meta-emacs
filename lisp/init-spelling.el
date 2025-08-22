;;; init-spelling.el --- spelling config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load "flyspell"
  (setq ispell-program-name "hunspell"
	ispell-list-command "--list"
	ispell-dictionary "en_US"))

(use-package flyspell-correct-popup
  :straight t
  :after flyspell)

(use-package langtool
  :straight t
  :config
  (setq langtool-java-classpath
	"/usr/share/languagetool:/usr/share/java/languagetool/*"
	langtool-default-language "en-US"))

(use-package writegood-mode
  :straight t)

(provide 'init-spelling)
;;; init-spelling.el ends here
