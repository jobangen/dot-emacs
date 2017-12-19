;;; package
(require 'package)
(setq package-archives '(("gnu"       . "https://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))
(package-initialize)
(setq package-enable-at-startup nil)

;;; load-paths
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/use-package"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/etc"))

;;; use-package
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(use-package diminish
  :ensure t)

;;; custom-file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; EXWM
(use-package dot-exwm
  :ensure exwm)

;;; org-mode
(use-package org
  :load-path ("site-lisp/org-mode/lisp"
              "site-lisp/org-mode/contrib/lisp"))

(org-babel-load-file "~/.emacs.d/myinit.org")


