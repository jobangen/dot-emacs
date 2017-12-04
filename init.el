(require 'package)
(setq package-archives '(("gnu"       . "https://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("org"       . "http://orgmode.org/elpa/")))
(package-initialize)
(setq package-enable-at-startup nil)

(unless package-archive-contents
   (package-refresh-contents))
(unless (package-installed-p 'use-package )
   (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(use-package diminish
  :ensure t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(org-babel-load-file "~/.emacs.d/myinit.org")


(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
