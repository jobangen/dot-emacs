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
(use-package dot-org
  :load-path ("site-lisp/org-mode/lisp"
              "site-lisp/org-mode/contrib/lisp")
  :ensure nil
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c i" . org-clock-in)
         ("C-c l" . org-store-link))
  :mode ("\\.txt\\'" . org-mode))

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
(use-package org-notmuch :ensure nil)
(use-package peep-dired :defer t)
(use-package pomodoro :defer t)
(use-package shell-interaction :ensure nil)
(use-package smex)
(use-package rainbow-delimiters :hook (emacs-lisp-mode . rainbow-delimiters-mode))

;;; A
(use-package ace-window
  :after (avy)
  :bind ("C-c k" . ace-delete-window)
  :config
  (progn
    (setq aw-scope 'frame)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    (setq aw-dispatch-always nil)))

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char-timer)
  :config
  (progn
    (setq avy-all-windows t)
    (setq avy-keys '(?w ?e ?r ?u ?i ?o ?a ?s ?d ?f ?g ?h ?j ?k ?l ?ö ?v ?b ?n ?m))
    (define-key input-decode-map (kbd "C-i") (kbd "H-i"))))


;;; C
(use-package char-menu
  :defer t
  :config
  (setq char-menu
        '("–" "—" "„“" "‘’" "“”" "»«" "…"
          ("Typography" "•" "©" "†" "‡" "°" "·" "§" "№" "★")
          ("Math" "≈" "≡" "≠" "∞" "×" "±" "∓" "÷" "√" "⊂" "⊃")
          ("Arrows" "←" "→" "↑" "↓" "⇐" "⇒" "⇑" "⇓")
          ("Greek" "α" "β" "Y" "δ" "ε" "ζ" "η" "θ" "ι" "κ" "λ" "μ" "ν" "ξ" "ο" "π" "ρ" "σ" "τ" "υ" "φ" "χ" "ψ" "ω")
          ("Other Languages" "Œ"))))

(use-package csv-mode :defer t
  :config
  (setq csv-separators '("," ";"))
  (setq csv-align-padding 2))


;;; D
(use-package dot-auctex :ensure auctex
  :demand t
  :mode ("\\.tex$" . TeX-latex-mode)
  :hook ((TeX-mode . TeX-fold-mode)
         (TeX-mode . variable-pitch-mode)
         (TeX-mode . linum-mode)
         (TeX-mode . LaTeX-math-mode)))

(use-package dot-defun
  :ensure nil
  :bind (("C-a" . job/beginning-of-line-or-indentation)
         ("C-k" . job/kill-line)
         ("C-w" . job/kill-word-or-region)
         ("C-c d" . job/insert-date)
         ("C-x C-v" . job/find-file-as-sudo)
         ("M-c" . job/capitalize-last-word)
         ("M-l" . job/downcase-last-word)))

;;; E
(use-package expand-region
  :bind (("C-c m" . er/expand-region)))

;;; F
(use-package flyspell-correct
  :config
  (setq flyspell-correct-interface 'flyspell-correct-ivy))

;;; I
(use-package ivy-hydra
  :after (ivy hydra))

;;; K
(use-package key-chord
  :after (avy ace-window)
  :init
  (progn
    (setq key-chord-two-keys-delay 0.15)
    (setq key-chord-one-key-delay 0.25)
    (key-chord-mode 1)
    (key-chord-define-global "jk" 'avy-goto-char-timer)
    (key-chord-define-global "jl" 'avy-goto-line)
    (key-chord-define-global "jf" 'ace-window)))


;;; L
(use-package latex-extra
  :diminish latex-extra-mode
  :hook (TeX-mode . latex-extra-mode))

(use-package link-hint
  :after (avy)
  :bind ("C-c h" . link-hint-open-link))

;;; M
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-<" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :diminish multiple-cursors)

;;; P
(use-package paperless :defer t
  :config
  (bind-key "C-m" 'paperless-display paperless-mode-map)
  (setq paperless-capture-directory "~/texte/texteingang")
  (setq paperless-root-directory "~/"))

;;; S
(use-package smart-mode-line
  :init
  (setq sml/theme 'dark)
  (sml/setup)
  (setq sml/shorten-directory t)
  (setq sml/shorten-modes t)
  (setq sml/name-width 25)
  (setq sml/mode-width 'full)
  (add-to-list 'sml/replacer-regexp-list '("^:DB:diss/" ":ds:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:ds:tex/" ":ds:tx:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:ds:tx:parts/" ":ds:tx:p:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:ds:tx:p:part1/" ":ds:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/Dokumente/" ":Dok:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/archiv/" ":arch:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/Downloads/" ":DL:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/texte/" ":TXT:") t))

(use-package shell-pop
  :bind (("C-c j" . shell-pop))
  :config
  (setq shell-pop-shell-type
        (quote ("ansi-term" "*ansi-term*"
                (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/usr/bin/zsh")
  (setq shell-pop-window-size 45)
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(use-package swiper
  :load-path "site-lisp/swiper"
  :ensure nil)

;;; V
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :init
  (volatile-highlights-mode t))

;;; W
(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode))

(use-package winner
  :init
  (winner-mode))

