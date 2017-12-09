(require 'package)
(setq package-archives '(("gnu"       . "https://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))
(package-initialize)
(setq package-enable-at-startup nil)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/use-package"))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(use-package diminish
  :ensure t)

(use-package org
  :load-path ("site-lisp/org-mode/lisp"
              "site-lisp/org-mode/contrib/lisp"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(org-babel-load-file "~/.emacs.d/myinit.org")


(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
