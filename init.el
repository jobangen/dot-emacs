;(package-initialize)
(setq package-enable-at-startup nil)

(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/etc"))

(use-package diminish)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(use-package dot-exwm :straight exwm)

;;;
(use-package dot-org
  :straight org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c i" . org-clock-in)
         ("C-c l" . org-store-link))
  :mode ("\\.txt\\'" . org-mode))


;;; Libraries
(use-package counsel-notmuch      :defer t)
(use-package define-word          :commands define-word define-word-at-point)
(use-package dired-collapse       :hook dired-mode)
(use-package dired-subtree        :commands dired-subtree-insert)
(use-package ess                  :commands R)
(use-package flyspell-correct-ivy :after (flyspell-correct ivy))
(use-package git-timemachine      :defer t)
(use-package goldendict           :commands goldendict-dwim)
(use-package gnuplot-mode         :mode "\\.plot\\'")
(use-package haskell-mode         :defer t)
(use-package hydra)
(use-package iso-transl           :straight nil)
(use-package neato-graph-bar      :defer t)
(use-package org-notmuch          :straight org :load-path "~/.emacs.d/straight/repos/org/contrib/lisp")
(use-package org-pdfview          :after (org pdf-tools))
(use-package pass)
(use-package peep-dired           :defer t)
(use-package pomodoro             :defer t)
(use-package smex)
(use-package rainbow-delimiters   :hook (emacs-lisp-mode . rainbow-delimiters-mode))
(use-package wgrep)

(org-babel-load-file "~/.emacs.d/myinit.org")


;;; A
(use-package abbrev
  :straight nil
  :diminish abbrev-mode
  :config
  (progn
    (setq save-abbrevs 'silently)
    (setq save-abbrevs t)
    (setq-default abbrev-mode t)))

(use-package ace-window
  :after (avy)
  :bind ("C-c k" . ace-delete-window)
  :config
  (progn
    (setq aw-scope 'frame)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    (setq aw-dispatch-always nil)))

(use-package avy
  :bind ("M-s" . avy-goto-char-timer)
  :config
  (progn
    (setq avy-all-windows t)
    (setq avy-keys '(?w ?e ?r ?u ?i ?o ?a ?s ?d ?f ?g ?h ?j ?k ?l ?ö ?v ?b ?n ?m))
    (define-key input-decode-map (kbd "C-i") (kbd "H-i"))))

;;; B
(use-package bibtex
  :straight nil
  :mode ("\\.bib$". bibtex-mode)
  :config
  (setq bibtex-dialect 'biblatex)
  (setq bibtex-maintain-sorted-entries t)
  (setq bibtex-autokey-year-use-crossref-entry t
        bibtex-autokey-year-length 10
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titleword-length 3)

  (defun bibtex-autokey-get-year ()
    "Return year field contents as a string obeying `bibtex-autokey-year-length'."
    (let ((yearfield (bibtex-autokey-get-field "date")))
      (substring yearfield (max 0 (- (length yearfield)
                                     bibtex-autokey-year-length))))))

(use-package bookmark+
  :init
  (setq bmkp-bmenu-state-file
        (no-littering-expand-var-file-name "bmkp/bmenu-state.el"))
  (setq bookmark-save-flag 1))


;;; C
(use-package calfw
  :bind (("C-c f" . job/open-org-calendar))
  :config
  (setq calendar-week-start-day 1)
  ;;
  (defun job/open-org-calendar ()
    (interactive)
    (delete-other-windows)
    (cfw:open-org-calendar)))

(use-package calfw-org
  :after (calfw)
  :config
  (setq cfw:org-agenda-schedule-args '(:sexp :timestamp)))

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

(use-package contacts
  :straight (contacts :local-repo "~/.emacs.d/lisp/scimax"
                      :files ("contacts.el"))
  :bind (("C-c g" . ivy-contacts))
  :config
  (setq contacts-files '("~/Dropbox/db/contacts.org"))
  (setq contacts-cache-file (no-littering-expand-var-file-name "contacts-cache.el")))

(use-package counsel
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-c u" . counsel-linux-app)
         ("C-M-s" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("M-y" . counsel-yank-pop)
         ("M-x" . counsel-M-x)
         ("C-x C-m" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-SPC" . counsel-mark-ring)
         ("C-c o" . counsel-outline)
         ("C-c n" . counsel-imenu)))

(use-package csv-mode :defer t
  :config
  (setq csv-separators '("," ";"))
  (setq csv-align-padding 2))


;;; D
(use-package deft
  :defer t
  :config
  (setq deft-directory "~/Dropbox/db/zk/zettel")
  (bind-key "C-h" 'deft-filter-decrement deft-mode-map)
  (bind-key "C-w" 'deft-filter-decrement-word deft-mode-map))

(use-package dot-auctex :straight auctex
  :demand t
  :mode ("\\.tex$" . TeX-latex-mode)
  :hook ((TeX-mode . TeX-fold-mode)
         (TeX-mode . variable-pitch-mode)
         (TeX-mode . linum-mode)
         (TeX-mode . LaTeX-math-mode)))

(use-package dot-defun
  :straight nil
  :demand t
  :bind (("C-a" . job/beginning-of-line-or-indentation)
         ("C-k" . job/kill-line)
         ("C-w" . job/kill-word-or-region)
         ("C-c d" . job/insert-date)
         ("C-x C-v" . job/find-file-as-sudo)
         ("M-c" . capitalize-word)
         ("M-l" . downcase-word)))

(use-package dired
  :straight nil
  :bind (:map dired-mode-map
              ("C-m" . dired-find-file)
              (";" . dired-subtree-insert)
              ("i" . dired-subtree-remove)
              ("P" . peep-dired))
  :init
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t
        trash-directory "~/.local/share/Trash")
  (setq dired-listing-switches "--group-directories-first -alh1v")
  (put 'dired-find-alternate-file 'disabled nil))


(use-package dired-hide-details
  :straight nil
  :hook (dired-mode .  dired-hide-details-mode))

(use-package dired-filter
  :hook (dired-mode . dired-filter-group-mode)
  :config
  (setq dired-filter-group-saved-groups
        '(("default"
           ("DIR"
            (directory))
           ("PDF"
            (extension "pdf"))
           ("LaTeX"
            (extension "tex" "bib"))
           ("Text & Data"
            (extension "org" "txt" "doc" "docx" "csv" "odt"))
           ("Media"
            (extension "JPG" "jpg" "PNG" "png" "gif" "bmp"))
           ("Archives"
            (extension "zip" "rar" "gz" "bz2" "tar" "org_archive"))))))

(use-package dired-launch
  :hook (dired-mode . dired-launch-mode)
  :diminish dired-launch-mode
  :init
  (setf dired-launch-extensions-map
        '( ;;Archives
          ("gz" ("file-roller"))
          ;; Office
          ("odt" ("libreoffice"))
          ("doc" ("libreoffice"))
          ("docx" ("libreoffice"))
          ("csv" ("libreoffice"))
          ("ppt" ("libreoffice"))
          ("pptx" ("libreoffice"))
          ("pdf" ("evince" "gimp-2.8"))
          ("PDF" ("evince " "gimp-2.8"))
          ;; Web
          ("html" ("firefox"))
          ;; Pictures
          ("jpg" ("eog" "gimp-2.8"))
          ("png" ("eog" "gimp-2.8"))
          ;; Video
          ("mov" ("totem" "vlc")))))

;;; E
(use-package epa-file
  :straight nil
  :config
  (epa-file-enable)
  ;; Symmetric Encryption.
  (setq epa-file-select-keys nil))

(use-package expand-region
  :bind (("C-c m" . er/expand-region)))

;;; F
(use-package flyspell-correct
  :config
  (setq flyspell-correct-interface 'flyspell-correct-ivy))

;;; G
(use-package gnorb
  :init
  (gnorb-tracking-initialize)
  (setq gnorb-gnus-sent-groups '(("nnimap+gmail:sent")
                                 ("nnimap+zedat:sent")
                                 ("nnimap+zedatma:sent")))
  :config
  (global-set-key (kbd "C-c A") 'gnorb-restore-layout)

  (eval-after-load "gnorb-org"
    '(progn
       (org-defkey org-mode-map (kbd "C-c t") #'gnorb-org-handle-mail)
       (org-defkey org-mode-map (kbd "C-c v") #'gnorb-org-view)
       (org-defkey org-mode-map (kbd "C-c E") #'gnorb-org-email-subtree)
       (setq gnorb-org-agenda-popup-bbdb t)
       (eval-after-load "org-agenda"
         '(progn (org-defkey org-agenda-mode-map (kbd "C-c t") #'gnorb-org-handle-mail)
                 (org-defkey org-agenda-mode-map (kbd "C-c v") #'gnorb-org-view)))))

  (eval-after-load "gnorb-gnus"
    '(progn
       (define-key gnus-summary-mime-map "a" #'gnorb-gnus-article-org-attach)
       (define-key gnus-summary-mode-map (kbd "C-c t") #'gnorb-gnus-incoming-do-todo)
       (define-key gnus-summary-mode-map (kbd "C-c v") #'gnorb-gnus-view)
       (define-key gnus-summary-mode-map (kbd "C-c C-t") #'gnorb-gnus-tag-message)
       (define-key gnus-summary-limit-map (kbd "g") #'gnorb-gnus-insert-tagged-messages)
       (define-key gnus-summary-limit-map (kbd "G") #'gnorb-gnus-insert-tracked-messages)
       (setq gnorb-gnus-capture-always-attach t)
       (push '("attach to org heading" . gnorb-gnus-mime-org-attach)
             gnus-mime-action-alist)
       ;; The only way to add mime button command keys is by redefining
       ;; gnus-mime-button-map, possibly not ideal. Ideal would be a
       ;; setter function in gnus itself.
       (push '(gnorb-gnus-mime-org-attach "a" "Attach to Org heading")
             gnus-mime-button-commands)
       (setq gnus-mime-button-map
             (let ((map (make-sparse-keymap)))
               (dolist (c gnus-mime-button-commands)
                 (define-key map (cadr c) (car c)))
               map))))

  (eval-after-load "message"
    '(progn
       (define-key message-mode-map (kbd "C-c t") #'gnorb-gnus-outgoing-do-todo))))


(use-package gnus
  :straight nil
  :init
  (setq gnus-init-file (no-littering-expand-etc-file-name "gnus-config.el")))

(use-package gnus-dired
  :straight nil
  :config
  (progn
    (defun gnus-dired-mail-buffers ()
      "Return a list of active message buffers."
      (let (buffers)
        (save-current-buffer
          (dolist (buffer (buffer-list t))
            (set-buffer buffer)
            (when (and (derived-mode-p 'message-mode)
                       (null message-sent-message-via))
              (push (buffer-name buffer) buffers))))
        (nreverse buffers)))
    (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)))


(use-package gscholar-bibtex
  :commands gscholar-bibtex
  :config
  (setq gscholar-bibtex-database-file (expand-file-name (concat db-dir "biblio.bib")))
  (setq gscholar-bibtex-default-source "Google Scholar")
  (gscholar-bibtex-source-on-off :off "IEEE Xplore")
  (gscholar-bibtex-source-on-off :off "DBLP")
  (gscholar-bibtex-source-on-off :off "ACM Digital Library"))

;;; H
(use-package hippie-expand
  :straight nil
  :bind ("M-<tab>" . hippie-expand)
  :init
  (setq hippie-expand-verbose t)

  (setq hippie-expand-try-functions-list
        '(yas-hippie-try-expand
          try-expand-dabbrev
          try-expand-all-abbrevs
          try-complete-file-name-partially
          try-complete-file-name
          ;; try-expand-dabbrev-from-kill
          try-expand-dabbrev-all-buffers
          ;; try-expand-list
          ;; try-expand-line
          ;; try-complete-lisp-symbol-partially
          ;; try-complete-lisp-symbol
          )))

;;; I
(use-package ispell
  :config
  (progn
    (setq-default ispell-program-name "aspell")
    (setq args (list "--sug-mode=ultra" "--lang=de_DE-neu"))
    (setq ispell-parser 'use-mode-name)))

(use-package ivy-hydra
  :after (ivy hydra))

;;; K
(use-package key-chord
  :after (avy ace-window)
  :demand t
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

(use-package ledger-mode
  :mode "\\.dat\\'"
  :config
  (setq ledger-binary-path "~/ledger/ledger"))


(use-package link-hint
  :after (avy)
  :bind ("C-c h" . link-hint-open-link))

(use-package lispy
  :hook (emacs-lisp-mode . lispy-mode)
  :diminish lispy-mode)

;;; M
(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-diff-refine-hunk 'all))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-<" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :diminish multiple-cursors)

;;; O
(use-package offlineimap
  :hook (gnus-before-startup . offlineimap)
  :config
  (setq offlineimap-timestamp "%Y-%m-%d-%H:%M:%S "))

(use-package org-gcal
  :config
  (setq org-gcal-auto-archive t)
  (setq org-gcal-down-days 365)
  (setq org-gcal-client-id "553301842275-clecdgmr7i8741e3ck5iltlgfk3qf79r.apps.googleusercontent.com")
  (setq org-gcal-client-secret "4zyEbm_F_BMuJsA7rZZmgFBm")
  (setq org-gcal-file-alist '(("jobangen@googlemail.com" . "~/Dropbox/db/org/calender.org"))))

;;; P
(use-package paperless :defer t
  :config
  (bind-key "C-m" 'paperless-display paperless-mode-map)
  (setq paperless-capture-directory "~/texte/texteingang")
  (setq paperless-root-directory "~/"))

(use-package pdf-tools
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward)
              ("am" . pdf-annot-add-markup-annotation)
              ("al" . pdf-annot-list-annotations)
              ("ad" . pdf-annot-delete)
              ("d" . pdf-annot-delete)
              ("ah" . pdf-annot-add-highlight-markup-annotation)
              ("h" . pdf-annot-add-highlight-markup-annotation)
              ("aq" . pdf-annot-add-squiggly-markup-annotation)
              ("as" . pdf-annot-add-strikeout-markup-annotation)
              ("at" . pdf-annot-add-text-annotation)
              ("t" . pdf-annot-add-text-annotation)
              ("au" . pdf-annot-add-underline-markup-annotation)
              ("j" . pdf-view-goto-page)
              ("sa" . pdf-view-auto-slice-minor-mode))
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (setq pdf-view-resize-factor 1.1)

  (defun pdf-view--rotate (&optional counterclockwise-p page-p)
    "Rotate PDF 90 degrees.  Requires pdftk to work.\n
Clockwise rotation is the default; set COUNTERCLOCKWISE-P to
non-nil for the other direction.  Rotate the whole document by
default; set PAGE-P to non-nil to rotate only the current page.
\nWARNING: overwrites the original file, so be careful!"
    ;; error out when pdftk is not installed
    (if (null (executable-find "pdftk"))
        (error "Rotation requires pdftk")
      ;; only rotate in pdf-view-mode
      (when (eq major-mode 'pdf-view-mode)
        (let* ((rotate (if counterclockwise-p "left" "right"))
               (file (format "\"%s\"" (pdf-view-buffer-file-name)))
               (page (pdf-view-current-page))
               (pages (cond ((not page-p) ; whole doc?
                             (format "1-end%s" rotate))
                            ((= page 1) ; first page?
                             (format "%d%s %d-end"
                                     page rotate (1+ page)))
                            ((= page (pdf-info-number-of-pages)) ; last page?
                             (format "1-%d %d%s"
                                     (1- page) page rotate))
                            (t          ; interior page?
                             (format "1-%d %d%s %d-end"
                                     (1- page) page rotate (1+ page))))))
          ;; empty string if it worked
          (if (string= "" (shell-command-to-string
                           (format (concat "pdftk %s cat %s "
                                           "output %s.NEW "
                                           "&& mv %s.NEW %s")
                                   file pages file file file)))
              (pdf-view-revert-buffer nil t)
            (error "Rotation error!"))))))

  (defun pdf-view-rotate-clockwise (&optional arg)
    "Rotate PDF page 90 degrees clockwise.  With prefix ARG, rotate
entire document."
    (interactive "P")
    (pdf-view--rotate nil (not arg)))

  (defun pdf-view-rotate-counterclockwise (&optional arg)
    "Rotate PDF page 90 degrees counterclockwise.  With prefix ARG,
rotate entire document."
    (interactive "P")
    (pdf-view--rotate :counterclockwise (not arg)))

  ;; http://pragmaticemacs.com/emacs/even-more-pdf-tools-tweaks/
  (advice-add 'pdf-annot-edit-contents-commit :after 'job/save-buffer-no-args)

  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . org-pdfview-open))
  (add-to-list 'org-file-apps
               '("\\.pdf::\\([[:digit:]]+\\)\\'" . org-pdfview-open))
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link)))))

;;; R
(use-package remem
  :commands remem-toggle
  :config
  (setq remem-database-dir (job/custom-temp-file-name "ra-index"))
  (setq remem-scopes-list '(("zettelkasten" 5 2 500)
                            ("texte" 5 2 500)))
  (setq remem-print-exact-relevance-p t)
  (setq remem-load-original-suggestion t)
  (setq remem-log-p t)
  (setq remem-logfile (expand-file-name "~/.custom-temp/.remem-log-file"))

  (setq remem-format-default
        '((0 2 (field 0 mouse-face remem-hilite2) nil)        ; Number
          (1 2 (face remem-even field 1) nil)                 ; sim
          (9 3 (face remem-odd field 9 mouse-face remem-hilite) nil) ; person
          (8 25 (face remem-even field 8 mouse-face remem-hilite) nil) ; subject
          (28 50 (face remem-odd field 28 mouse-face remem-hilite) nil))) ; keywords
  )

;;; S
(use-package sdcv-mode
  :straight (sdcv-mode :type git
                       :host github
                       :repo "pluskid/sdcv-mode")
  :commands sdcv-search
  :config
  (bind-key "<tab>" 'sdcv-toggle-entry sdcv-mode-map))

(use-package shell-interaction
  :straight (shell-interaction :local-repo "~/.emacs.d/lisp/shell-interaction"))


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

(use-package smartparens
  :diminish smartparens-mode
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (add-hook 'yas-before-expand-snippet-hook (lambda () (smartparens-mode -1)))
  (add-hook 'yas-after-exit-snippet-hook (lambda () (smartparens-mode 1))))

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

(use-package swiper)

;;; U
(use-package undo-tree
  :bind (("C-x u" . undo-tree-visualize))
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode 1)
  :config
  (progn
    (setq undo-tree-visualizer-timestamps nil)
    (setq undo-tree-visualizer-diff t)))

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

(use-package writegood-mode
  :defer t
  :config
   (progn
    (setq writegood-weasel-words
     '("TODO" "wichtig" "wichtige" "vielleicht" "auch" "dabei" "sehr" "ziemlich" "möglicherweise" "wohl" "recht" "dann" "paar" "bisschen"))
    (setq writegood-passive-voice-irregulars
     '("gemacht" "geworden" "vorgenommen" "durchgeführt"))))

;;; Y
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (progn
    (add-hook 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
    (yas-global-mode 1)
    (setq require-final-newline nil)))

;;; Z
(use-package zettelkasten
  :straight (zettelkasten :local-repo "~/.emacs.d/lisp/zettelkasten")
  :bind ("C-c z" . hydra-zettelkasten/body)
  :init
  (setq zettelkasten-main-directory "~/Dropbox/db/zk/")
  (setq zettelkasten-temp-directory "~/.emacs.d/var/zettelkasten/")
  (setq zettelkasten-bibliography-file job/bibliography-file)
  (setq zettelkasten-texts-directory "~/texte/")

  (defun zettelkasten-txt-query ()
    (interactive)
    (counsel-ag nil "~/.custom-temp/pdfs-extracted" nil))

  (defun job/open-at-point ()
    (interactive)
    (if (equal major-mode 'dired-mode)
        (dired-find-file))
    (if (equal major-mode 'org-mode)
        (org-open-at-point))))
