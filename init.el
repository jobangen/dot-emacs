(package-initialize nil)
(setq package-enable-at-startup nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(org-babel-load-file "~/.emacs.d/myinit.org")

(put 'narrow-to-region 'disabled nil)
