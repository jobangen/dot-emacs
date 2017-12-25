;;;
(require 'package)
(setq package-archives '(("gnu"       . "https://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))
(package-initialize)
(setq package-enable-at-startup nil)

;;;
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/use-package"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/etc"))

;;; use-package
(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(use-package diminish
  :ensure t)

;;;
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;;
(use-package dot-exwm
  :ensure exwm)


;;;
(use-package dot-org
  :load-path ("site-lisp/org-mode/lisp"
              "site-lisp/org-mode/contrib/lisp")
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c i" . org-clock-in)
         ("C-c l" . org-store-link))
  :mode ("\\.txt\\'" . org-mode))

;;;
(org-babel-load-file "~/.emacs.d/myinit.org")

;;; pkg:latex
;;;
(use-package dot-auctex
  :ensure auctex
  :mode ("\\.tex$" . TeX-latex-mode)
  :hook ((TeX-mode . TeX-fold-mode)
         (TeX-mode . variable-pitch-mode)
         (TeX-mode . linum-mode)
         (TeX-mode . LaTeX-math-mode)))

;;;
(use-package latex-extra
  :ensure t
  :defer t
  :diminish latex-extra-mode
  :hook (TeX-mode . latex-extra-mode))


;;;
(use-package dot-defun
  :bind (("C-a" . job/beginning-of-line-or-indentation)
         ("C-k" . job/kill-line)
         ("C-w" . job/kill-word-or-region)
         ("C-c d" . job/insert-date)
         ("C-x C-v" . job/find-file-as-sudo)
         ("M-c" . job/capitalize-last-word)
         ("M-l" . job/downcase-last-word)))

;;; shell-interaction
(use-package shell-interaction)
