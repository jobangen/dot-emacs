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

(setq use-package-always-ensure t)
(require 'bind-key)
(use-package diminish
  :ensure t)

;;;
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;;
(use-package dot-exwm :ensure exwm)

;;;
(use-package dot-org :pin manual
  :load-path ("site-lisp/org-mode/lisp"
              "site-lisp/org-mode/contrib/lisp")
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c i" . org-clock-in)
         ("C-c l" . org-store-link))
  :mode ("\\.txt\\'" . org-mode))

;;;
(org-babel-load-file "~/.emacs.d/myinit.org")

;;; Libraries
(use-package counsel-notmuch :defer t)
(use-package dired-collapse :hook dired-mode)
(use-package dired-subtree :commands dired-subtree-insert)
(use-package flyspell-correct-ivy :after (flyspell-correct ivy))
(use-package git-timemachine)
(use-package gnuplot-mode :mode "\\.plot\\'")
(use-package haskell-mode :defer t)
(use-package ledger-mode :mode "\\.dat\\'")
(use-package lispy :hook (emacs-lisp-mode . lispy-mode) :diminish lispy-mode)
(use-package org-notmuch :pin manual)
(use-package peep-dired :defert t)
(use-package pomodoro :defer t)
(use-package shell-interaction :pin manual)
(use-package rainbow-delimiters :hook (emacs-lisp-mode . rainbow-delimiters-mode))

;;;
(use-package csv-mode :defer t
  :config
  (setq csv-separators '("," ";"))
  (setq csv-align-padding 2))


;;;
(use-package dot-auctex :ensure auctex
  :demand t
  :mode ("\\.tex$" . TeX-latex-mode)
  :hook ((TeX-mode . TeX-fold-mode)
         (TeX-mode . variable-pitch-mode)
         (TeX-mode . linum-mode)
         (TeX-mode . LaTeX-math-mode)))

;;;
(use-package dot-defun
  :pin manual
  :bind (("C-a" . job/beginning-of-line-or-indentation)
         ("C-k" . job/kill-line)
         ("C-w" . job/kill-word-or-region)
         ("C-c d" . job/insert-date)
         ("C-x C-v" . job/find-file-as-sudo)
         ("M-c" . job/capitalize-last-word)
         ("M-l" . job/downcase-last-word)))

;;;
(use-package expand-region
  :bind (("C-c m" . er/expand-region)))

;;;
(use-package flyspell-correct
  :config
  (setq flyspell-correct-interface 'flyspell-correct-ivy))

;;;
(use-package ivy-hydra
  :after (ivy hydra))

;;;
(use-package latex-extra
  :diminish latex-extra-mode
  :hook (TeX-mode . latex-extra-mode))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-<" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :diminish multiple-cursors)

;;;
(use-package paperless :defer t
  :config
  (bind-key "C-m" 'paperless-display paperless-mode-map)
  (setq paperless-capture-directory "~/texte/texteingang")
  (setq paperless-root-directory "~/"))

;;;
(use-package winner
  :init
  (winner-mode))





