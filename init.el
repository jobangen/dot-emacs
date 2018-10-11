(setq package-enable-at-startup nil)
(server-start)

;;; Config
(setq split-width-threshold 110)
(setq inhibit-splash-screen t) ;;Remove splash screen
(fset 'yes-or-no-p 'y-or-n-p)
(setq large-file-warning-threshold nil)
(setq sentence-end-double-space nil)
(setq visible-bell t) ;; blinken bei error

(transient-mark-mode nil) ;; No region when it is not highlighted
(global-font-lock-mode 1) ;;syntax highlighting everywhere
(global-visual-line-mode 1) ;;Add proper word wrapping
(global-auto-revert-mode t) ;;aktualisiert buffer automatisch
(setq auto-revert-interval 3) ;; Prüfinterval in Sek.
(setq auto-revert-verbose nil)
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8) ;;UTF 8 by default
(set-keyboard-coding-system 'utf-8) ;;(prefer-coding-system 'utf-8)
(setq-default indent-tabs-mode nil ;; Insert tabs as spaces (not tabs)
              indicate-buffer-boundaries 'left ;; Graphical gimmick
              indicate-empty-lines t           ;; Graphical gimmick
              )
(setq next-line-add-newlines nil) ;; C-n erzeugt Absatz am Ender der Zeile
(setq recenter-positions '(top middle bottom))

;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
;; https://github.com/dakrone/eos/blob/master/eos.org
(setq save-interprogram-paste-before-kill t)

(setq display-time-format "[%H:%M]"
      display-time-default-load-average nil)
(display-time-mode t)

(setq time-stamp-active t
      time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S"
      time-stamp-start "#\\+DATE:[ \t]+\\\\?[\[\"<]+"
      time-stamp-end "\\\\?[\]\">]")
(add-hook 'write-file-hooks 'time-stamp)

(setq reb-re-syntax 'rx)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser")

;;; Bootstrap
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
(setq use-package-verbose t)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/etc"))

(use-package diminish)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(use-package dot-exwm :straight exwm)

(use-package no-littering)

;;;
(use-package dot-org
  :demand t
  :straight org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c i" . org-clock-in)
         ("C-c l" . org-store-link))
  :mode ("\\.txt\\'" . org-mode))

;;; Setup
;; https://sigquit.wordpress.com/2008/09/28/single-dot-emacs-file/
(defun system-type-is-gnu ()
  "Return true if system is GNU/Linux-based"
  (interactive)
  (string-equal system-type "gnu/linux"))

(defun system-type-is-windows ()
  "Return true if system is Windows-based"
  (interactive)
  (string-equal system-type "windows-nt"))

;; Dir
(defvar backup-dir
   (expand-file-name (convert-standard-filename "backups/") user-emacs-directory))

(defvar autosave-dir
   (expand-file-name (convert-standard-filename "autosave/")  user-emacs-directory))

(if (system-type-is-gnu)
   (let ((default-directory  "~/.emacs.d/lisp/"))
     (normal-top-level-add-subdirs-to-load-path)))

(defvar custom-temp
   (expand-file-name "~/.custom-temp/"))

(defun job/custom-temp-file-name (file)
    (expand-file-name (convert-standard-filename file) custom-temp))

(defvar texte-dir
   (expand-file-name "~/archive/texts/"))

(defvar dropbox-dir
  (if (system-type-is-gnu)
    (expand-file-name "~/Dropbox/"))
  (if (system-type-is-windows)
    (expand-file-name "C:/Users/job/Dropbox/")))

(defvar db-dir
  (expand-file-name (convert-standard-filename "db/") dropbox-dir))

(setq org-directory
      (expand-file-name (convert-standard-filename "db/org/") dropbox-dir))

(defvar zettel-dir
  (expand-file-name (convert-standard-filename "db/zk/zettel/") dropbox-dir))

(defvar job/bibliography-file
  (expand-file-name (convert-standard-filename "db/biblio.bib") dropbox-dir))

;; Backup
(setq backup-directory-alist
      `(("." . ,backup-dir)))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms
      `((".*" ,autosave-dir t)))

;;; Libraries
(use-package counsel-projectile   :defer 3)
(use-package define-word          :commands define-word define-word-at-point)
(use-package dired-collapse       :hook dired-mode)
(use-package dired-subtree        :commands dired-subtree-insert)
(use-package ess                  :commands R)
(use-package flyspell-correct-ivy :after (flyspell-correct ivy))
(use-package git-timemachine      :defer t)
(use-package goldendict           :commands goldendict-dwim)
(use-package hydra)
(use-package iso-transl           :defer 2 :straight nil)
(use-package ivy-hydra            :after (ivy hydra))
(use-package ivy-pass             :defer t :after (ivy pass))
(use-package neato-graph-bar      :defer t)
(use-package neotree              :defer 3)
(use-package nov                  :mode ("\\.epub\\'" . nov-mode))
(use-package org-notmuch          :defer 3 :straight org :load-path "~/.emacs.d/straight/repos/org/contrib/lisp")
(use-package org-pdfview          :after (org pdf-tools))
(use-package pass                 :defer t)
(use-package peep-dired           :defer t)
(use-package smex)
(use-package rainbow-delimiters   :hook (emacs-lisp-mode . rainbow-delimiters-mode))
(use-package wgrep                :defer 3)

(org-babel-load-file "~/.emacs.d/myinit.org")

;;; A
(use-package abbrev
  :defer 2
  :straight nil
  :diminish abbrev-mode
  :config
  (progn
    (setq save-abbrevs 'silently)
    (setq save-abbrevs t)
    (setq-default abbrev-mode t)))

(use-package academic-phrases
  :defer 2
  :straight (academic-phrases :type git
                              :host github
                              :repo "nashamri/academic-phrases"))

(use-package ace-window
  :after (avy)
  :bind ("C-c k" . ace-delete-window)
  :demand t
  :config
  (progn
    (setq aw-scope 'frame)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    (setq aw-dispatch-always nil)))

(use-package auto-yasnippet
  :bind (("H-w" . aya-create)
         ("H-y" . aya-expand)))

(use-package avy
  :bind ("M-s" . avy-goto-char-timer)
  :config
  (progn
    (setq avy-all-windows t)
    (setq avy-keys '(?w ?e ?r ?u ?i ?o ?a ?s ?d ?f ?g ?h ?j ?k ?l ?ö ?v ?b ?n ?m))
    (define-key input-decode-map (kbd "C-i") (kbd "H-i"))))

;;; B
(use-package beacon
  :defer 2
  :diminish beacon-mode
  :config
  (beacon-mode)
  (setq beacon-size 20)
  (setq beacon-color "#ff0000"))

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
                                     bibtex-autokey-year-length)))))

  (setq bibtex-biblatex-entry-alist
        '(("Article" "Article in Journal"
           (("author") ("title")        ;rm ("journaltitle")
            ("year" nil nil 0) ("date" nil nil 0))
           nil
           (("origdate")                ;add "origdate"
            ("translator") ("annotator") ("commentator") ("subtitle") ("titleaddon")
            ("editor") ("editora") ("editorb") ("editorc")
            ("journaltitle") ("journalsubtitle") ("issuetitle") ("issuesubtitle")
            ("language") ("origlanguage") ("series") ("volume") ("number") ("eid")
            ("issue") ("month") ("pages") ("version") ("note") ("issn")
            ("addendum") ("pubstate") ("doi") ("eprint") ("eprintclass")
            ("eprinttype") ("url") ("urldate")))
          ("Book" "Single-Volume Book"
           (("author") ("title") ("year" nil nil 0) ("date" nil nil 0))
           nil
           (("editor") ("editora") ("editorb") ("editorc")
            ("translator") ("annotator") ("commentator")
            ("introduction") ("foreword") ("afterword") ("subtitle") ("titleaddon")
            ("maintitle") ("mainsubtitle") ("maintitleaddon")
            ("language") ("origlanguage") ("volume") ("part") ("edition") ("volumes")
            ("series") ("number") ("note") ("publisher") ("location") ("isbn")
            ("chapter") ("pages") ("pagetotal") ("addendum") ("pubstate") ("doi")
            ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
          ("MVBook" "Multi-Volume Book"
           (("author") ("title") ("year" nil nil 0) ("date" nil nil 0))
           nil
           (("editor") ("editora") ("editorb") ("editorc")
            ("translator") ("annotator") ("commentator")
            ("introduction") ("foreword") ("afterword") ("subtitle")
            ("titleaddon") ("language") ("origlanguage") ("edition") ("volumes")
            ("series") ("number") ("note") ("publisher")
            ("location") ("isbn") ("pagetotal") ("addendum") ("pubstate") ("doi")
            ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
          ("InBook" "Chapter or Pages in a Book"
           (("title") ("year" nil nil 0) ("date" nil nil 0))
           (("author") ("booktitle"))
           (("bookauthor") ("editor") ("editora") ("editorb") ("editorc")
            ("translator") ("annotator") ("commentator") ("introduction") ("foreword")
            ("afterword") ("subtitle") ("titleaddon") ("maintitle") ("mainsubtitle")
            ("maintitleaddon") ("booksubtitle") ("booktitleaddon")
            ("language") ("origlanguage") ("volume") ("part") ("edition") ("volumes")
            ("series") ("number") ("note") ("publisher") ("location") ("isbn")
            ("chapter") ("pages") ("addendum") ("pubstate")
            ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
          ("BookInBook" "Book in Collection" ; same as @inbook
           (("title") ("year" nil nil 0) ("date" nil nil 0))
           (("author") ("booktitle"))
           (("bookauthor") ("editor") ("editora") ("editorb") ("editorc")
            ("translator") ("annotator") ("commentator") ("introduction") ("foreword")
            ("afterword") ("subtitle") ("titleaddon") ("maintitle") ("mainsubtitle")
            ("maintitleaddon") ("booksubtitle") ("booktitleaddon")
            ("language") ("origlanguage") ("volume") ("part") ("edition") ("volumes")
            ("series") ("number") ("note") ("publisher") ("location") ("isbn")
            ("chapter") ("pages") ("addendum") ("pubstate")
            ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
          ("SuppBook" "Supplemental Material in a Book" ; same as @inbook
           (("title") ("year" nil nil 0) ("date" nil nil 0))
           (("author") ("booktitle"))
           (("bookauthor") ("editor") ("editora") ("editorb") ("editorc")
            ("translator") ("annotator") ("commentator") ("introduction") ("foreword")
            ("afterword") ("subtitle") ("titleaddon") ("maintitle") ("mainsubtitle")
            ("maintitleaddon") ("booksubtitle") ("booktitleaddon")
            ("language") ("origlanguage") ("volume") ("part") ("edition") ("volumes")
            ("series") ("number") ("note") ("publisher") ("location") ("isbn")
            ("chapter") ("pages") ("addendum") ("pubstate")
            ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
          ("Booklet" "Booklet (Bound, but no Publisher)"
           (("author" nil nil 0) ("editor" nil nil 0) ("title")
            ("year" nil nil 1) ("date" nil nil 1))
           nil
           (("subtitle") ("titleaddon") ("language") ("howpublished") ("type")
            ("note") ("location") ("chapter") ("pages") ("pagetotal") ("addendum")
            ("pubstate") ("doi") ("eprint") ("eprintclass") ("eprinttype")
            ("url") ("urldate")))
          ("Collection" "Single-Volume Collection"
           (("editor") ("title") ("year" nil nil 0) ("date" nil nil 0))
           nil
           (("editora") ("editorb") ("editorc") ("translator") ("annotator")
            ("commentator") ("introduction") ("foreword") ("afterword")
            ("subtitle") ("titleaddon") ("maintitle") ("mainsubtitle")
            ("maintitleaddon") ("language") ("origlanguage") ("volume")
            ("part") ("edition") ("volumes") ("series") ("number") ("note")
            ("publisher") ("location") ("isbn") ("chapter") ("pages") ("pagetotal")
            ("addendum") ("pubstate") ("doi") ("eprint") ("eprintclass")
            ("eprinttype") ("url") ("urldate")))
          ("MVCollection" "Multi-Volume Collection"
           (("editor") ("title") ("year" nil nil 0) ("date" nil nil 0))
           nil
           (("editora") ("editorb") ("editorc") ("translator") ("annotator")
            ("commentator") ("introduction") ("foreword") ("afterword")
            ("subtitle") ("titleaddon") ("language") ("origlanguage") ("edition")
            ("volumes") ("series") ("number") ("note") ("publisher")
            ("location") ("isbn") ("pagetotal") ("addendum") ("pubstate") ("doi")
            ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
          ("InCollection" "Article in a Collection"
           (("author") ("title") ("year" nil nil 0) ("date" nil nil 0))
           (("booktitle"))
           (("editor") ("editora") ("editorb") ("editorc") ("translator") ("annotator")
            ("commentator") ("introduction") ("foreword") ("afterword")
            ("subtitle") ("titleaddon") ("maintitle") ("mainsubtitle")
            ("maintitleaddon") ("booksubtitle") ("booktitleaddon")
            ("language") ("origlanguage") ("volume") ("part") ("edition")
            ("volumes") ("series") ("number") ("note") ("publisher") ("location")
            ("isbn") ("chapter") ("pages") ("addendum") ("pubstate") ("doi")
            ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
          ("SuppCollection" "Supplemental Material in a Collection" ; same as @incollection
           (("author") ("editor") ("title") ("year" nil nil 0) ("date" nil nil 0))
           (("booktitle"))
           (("editora") ("editorb") ("editorc") ("translator") ("annotator")
            ("commentator") ("introduction") ("foreword") ("afterword")
            ("subtitle") ("titleaddon") ("maintitle") ("mainsubtitle")
            ("maintitleaddon") ("booksubtitle") ("booktitleaddon")
            ("language") ("origlanguage") ("volume") ("part") ("edition")
            ("volumes") ("series") ("number") ("note") ("publisher") ("location")
            ("isbn") ("chapter") ("pages") ("addendum") ("pubstate") ("doi")
            ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
          ("Manual" "Technical Manual"
           (("author" nil nil 0) ("editor" nil nil 0) ("title")
            ("year" nil nil 1) ("date" nil nil 1))
           nil
           (("subtitle") ("titleaddon") ("language") ("edition")
            ("type") ("series") ("number") ("version") ("note")
            ("organization") ("publisher") ("location") ("isbn") ("chapter")
            ("pages") ("pagetotal") ("addendum") ("pubstate")
            ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
          ("Misc" "Miscellaneous"
           (("author" nil nil 0) ("editor" nil nil 0) ("title")
            ("year" nil nil 1) ("date" nil nil 1))
           nil
           (("subtitle") ("titleaddon") ("language") ("howpublished") ("type")
            ("version") ("note") ("organization") ("location")
            ("date") ("month") ("year") ("addendum") ("pubstate")
            ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
          ("Online" "Online Resource"
           (("author" nil nil 0) ("editor" nil nil 0) ("title")
            ("year" nil nil 1) ("date" nil nil 1) ("url"))
           nil
           (("subtitle") ("titleaddon") ("language") ("version") ("note")
            ("organization") ("date") ("month") ("year") ("addendum")
            ("pubstate") ("urldate")))
          ("Patent" "Patent"
           (("author") ("title") ("number") ("year" nil nil 0) ("date" nil nil 0))
           nil
           (("holder") ("subtitle") ("titleaddon") ("type") ("version") ("location")
            ("note") ("date") ("month") ("year") ("addendum") ("pubstate")
            ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
          ("Periodical" "Complete Issue of a Periodical"
           (("editor") ("title") ("year" nil nil 0) ("date" nil nil 0))
           nil
           (("editora") ("editorb") ("editorc") ("subtitle") ("issuetitle")
            ("issuesubtitle") ("language") ("series") ("volume") ("number") ("issue")
            ("date") ("month") ("year") ("note") ("issn") ("addendum") ("pubstate")
            ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
          ("SuppPeriodical" "Supplemental Material in a Periodical" ; same as @article
           (("author") ("title") ("journaltitle")
            ("year" nil nil 0) ("date" nil nil 0))
           nil
           (("translator") ("annotator") ("commentator") ("subtitle") ("titleaddon")
            ("editor") ("editora") ("editorb") ("editorc")
            ("journalsubtitle") ("issuetitle") ("issuesubtitle")
            ("language") ("origlanguage") ("series") ("volume") ("number") ("eid")
            ("issue") ("month") ("pages") ("version") ("note") ("issn")
            ("addendum") ("pubstate") ("doi") ("eprint") ("eprintclass")
            ("eprinttype") ("url") ("urldate")))
          ("Proceedings" "Single-Volume Conference Proceedings"
           (("title") ("year" nil nil 0) ("date" nil nil 0))
           nil
           (("subtitle") ("titleaddon") ("maintitle") ("mainsubtitle")
            ("maintitleaddon") ("eventtitle") ("eventdate") ("venue") ("language")
            ("editor")
            ("volume") ("part") ("volumes") ("series") ("number") ("note")
            ("organization") ("publisher") ("location") ("month")
            ("isbn") ("chapter") ("pages") ("pagetotal") ("addendum") ("pubstate")
            ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
          ("MVProceedings" "Multi-Volume Conference Proceedings"
           (("editor") ("title") ("year" nil nil 0) ("date" nil nil 0))
           nil
           (("subtitle") ("titleaddon") ("eventtitle") ("eventdate") ("venue")
            ("language") ("volumes") ("series") ("number") ("note")
            ("organization") ("publisher") ("location") ("month")
            ("isbn") ("pagetotal") ("addendum") ("pubstate")
            ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
          ("InProceedings" "Article in Conference Proceedings"
           (("author") ("title") ("year" nil nil 0) ("date" nil nil 0))
           (("booktitle"))
           (("editor") ("subtitle") ("titleaddon") ("maintitle") ("mainsubtitle")
            ("maintitleaddon") ("booksubtitle") ("booktitleaddon")
            ("eventtitle") ("eventdate") ("venue") ("language")
            ("volume") ("part") ("volumes") ("series") ("number") ("note")
            ("organization") ("publisher") ("location") ("month") ("isbn")
            ("chapter") ("pages") ("addendum") ("pubstate")
            ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
          ("Reference" "Single-Volume Work of Reference" ; same as @collection
           (("editor") ("title") ("year" nil nil 0) ("date" nil nil 0))
           nil
           (("editora") ("editorb") ("editorc") ("translator") ("annotator")
            ("commentator") ("introduction") ("foreword") ("afterword")
            ("subtitle") ("titleaddon") ("maintitle") ("mainsubtitle")
            ("maintitleaddon") ("language") ("origlanguage") ("volume")
            ("part") ("edition") ("volumes") ("series") ("number") ("note")
            ("publisher") ("location") ("isbn") ("chapter") ("pages") ("pagetotal")
            ("addendum") ("pubstate") ("doi") ("eprint") ("eprintclass")
            ("eprinttype") ("url") ("urldate")))
          ("MVReference" "Multi-Volume Work of Reference" ; same as @mvcollection
           (("editor") ("title") ("year" nil nil 0) ("date" nil nil 0))
           nil
           (("editora") ("editorb") ("editorc") ("translator") ("annotator")
            ("commentator") ("introduction") ("foreword") ("afterword")
            ("subtitle") ("titleaddon") ("language") ("origlanguage") ("edition")
            ("volumes") ("series") ("number") ("note") ("publisher")
            ("location") ("isbn") ("pagetotal") ("addendum") ("pubstate") ("doi")
            ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
          ("InReference" "Article in a Work of Reference" ; same as @incollection
           (("author") ("editor") ("title") ("year" nil nil 0) ("date" nil nil 0))
           (("booktitle"))
           (("editora") ("editorb") ("editorc") ("translator") ("annotator")
            ("commentator") ("introduction") ("foreword") ("afterword")
            ("subtitle") ("titleaddon") ("maintitle") ("mainsubtitle")
            ("maintitleaddon") ("booksubtitle") ("booktitleaddon")
            ("language") ("origlanguage") ("volume") ("part") ("edition")
            ("volumes") ("series") ("number") ("note") ("publisher") ("location")
            ("isbn") ("chapter") ("pages") ("addendum") ("pubstate") ("doi")
            ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
          ("Report" "Technical or Research Report"
           (("author") ("title") ("type") ("institution")
            ("year" nil nil 0) ("date" nil nil 0))
           nil
           (("subtitle") ("titleaddon") ("language") ("number") ("version") ("note")
            ("location") ("month") ("isrn") ("chapter") ("pages") ("pagetotal")
            ("addendum") ("pubstate")
            ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
          ("Thesis" "PhD. or Master's Thesis"
           (("author") ("title") ("type") ("institution")
            ("year" nil nil 0) ("date" nil nil 0))
           nil
           (("subtitle") ("titleaddon") ("language") ("note") ("location")
            ("month") ("isbn") ("chapter") ("pages") ("pagetotal")
            ("addendum") ("pubstate")
            ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
          ("Unpublished" "Unpublished"
           (("author") ("title") ("year" nil nil 0) ("date" nil nil 0))
           nil
           (("subtitle") ("titleaddon") ("language") ("howpublished")
            ("note") ("location") ("isbn") ("date") ("month") ("year")
            ("addendum") ("pubstate") ("url") ("urldate")))))
  )

(use-package bibtex-utils
  :defer 2
  :after (bibtex)
  :config
  (setq bu-bibtex-fields-ignore-list '("")))

(use-package blimp
  :straight (blimp :type git
                   :host github
                   :repo "walseb/blimp")
  :hook (image-mode-hook . blimp-mode))

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
         ("C-M-s" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("M-y" . counsel-yank-pop)
         ("M-x" . counsel-M-x)
         ("C-x C-m" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-SPC" . counsel-mark-ring)
         ("C-c o" . counsel-outline)
         ("C-c n" . counsel-org-goto)))

(use-package counsel-notmuch
  :bind ("C-c u" . counsel-notmuch))

(use-package counsel-org-clock
  :bind ("C-c C-x c" . counsel-org-clock-history)
  :config
  (setq counsel-org-clock-default-action 'counsel-org-clock-clock-dwim-action))

(use-package csv-mode
  :defer t
  :config (setq csv-separators '("," ";"))
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
         (TeX-mode . LaTeX-math-mode))
  :config
  (eval-after-load 'tex-mode
    '(bind-key "C-:" 'reftex-citation LaTeX-mode-map)))

(use-package dot-defun
  :straight nil
  :defer 2
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
              ("P" . peep-dired)
              ("," .  job-dired-cp-mv-files-to-destinations))
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
(use-package ediff
  :config (set 'ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package elmacro
  :defer 2
  :diminish elmacro-mode
  :config
  (elmacro-mode))

(use-package engine-mode
  :defer 2
  :config
  (engine-mode t)
  (defengine google
    "http://www.google.de/search?ie=utf-8&oe=utf-8&q=%s")
  (defengine google-images
    "http://www.google.de/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")
  (defengine google-scholar
    "https://scholar.google.de/scholar?hl=de&q=%s")
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s")
  (defengine fu-katalog
    "http://aleph-www.ub.fu-berlin.de/F/?func=find-e&request=%s")
  (defengine jstor
    "http://www.jstor.org/action/doBasicSearch?acc=on&wc=on&fc=off&group=none&Query=%s")
  (defengine sowiport
    "http://sowiport.gesis.org/Search/Results?type=AllFields&lookfor=%s")
  (defengine pons-de-en
    "http://de.pons.com/übersetzung?l=deen&in=&lf=de&q=%s")
  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s")
  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=de&go=Go&search=%s"))

(use-package epa-file
  :straight nil
  :config
  (epa-file-enable)
  ;; Symmetric Encryption.
  (setq epa-file-select-keys nil))

(use-package expand-region
  :bind (("C-c m" . er/expand-region)))

;;; F
(use-package filetags
  :straight (filetags :type git
                      :host github
                      :repo "DerBeutlin/filetags.el")
  :bind (:map dired-mode-map
              ("#" . filetags-dired-update-tags))
  :init
  (setq filetags-load-controlled-vocabulary-from-file t))

(use-package flyspell
  :diminish flyspell-mode
  :hook (text-mode . flyspell-mode)
  :init
  :config
  (eval-after-load "flyspell"
    '(define-key flyspell-mode-map (kbd "C-.") nil))
  (eval-after-load "flyspell"
    '(define-key flyspell-mode-map (kbd "C-,") nil))

  (setq flyspell-tex-command-regexp
        (concat
         "\\("                          ;1
         "\\(begin\\|end\\)"
         "[ \t]*{\\|"
         "\\("                          ;2
         "cite[.*]*\\|"
         "autocite[.*]*\\|autocites[.*]*\\|"
         "textcite[.*]*\\|textcites[.*]*\\|"
         "label\\|ref\\|eqref\\|"
         "documentclass\\|KOMAoptions\\|setkomafont\\|usepackage\\|"
         "addbibresource\\|newclassic\\|"
         "pagestyle\\|"
         "printbibliography"
         "\\)"                          ;2
         "[ \t]*"
         "\\(\\[[^]]*\\]\\)"
         "?{[^{}]*"
         "\\)"                          ;1
         ))

  ;; run flyspell-buffer after saving dict
  (defun flyspell-buffer-after-pdict-save (&rest _)
    (flyspell-buffer))

  (advice-add 'ispell-pdict-save :after #'flyspell-buffer-after-pdict-save))

(use-package flyspell-correct
  :bind (("C-," . flyspell-correct-wrapper))
  :config
  (setq flyspell-correct-interface 'flyspell-correct-ivy))

;;; G
(use-package german-holidays
  :defer 2
  :config
  (setq holiday-other-holidays holiday-german-holidays))

(use-package gnorb
  :defer 2
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

(use-package gnuplot
  :straight (gnuplot :type git
                     :host github
                     :repo "bruceravel/gnuplot-mode"))

(use-package gnus
  :commands gnus
  :straight nil
  :init
  (setq gnus-init-file (no-littering-expand-etc-file-name "gnus-config.el")))

(use-package gnus-dired
  :defer 2
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

(use-package google-translate
  :commands google-translate-smooth-translate
  :config
  (require 'google-translate-smooth-ui)
  (setq google-translate-translation-directions-alist
        '(("de" . "en") ("en" . "de") ("de" . "fr") ("fr" . "de")))
  (setq google-translate-output-destination nil))

(use-package goto-addr
  :straight nil
  :hook ((compilation-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)
         (eshell-mode . goto-address-mode)
         (shell-mode . goto-address-mode))
  :bind (:map goto-address-highlight-keymap
              ("C-c C-o" . goto-address-at-point))
  :commands (goto-address-prog-mode
             goto-address-mode))

(use-package gscholar-bibtex
  :commands gscholar-bibtex
  :config
  (setq gscholar-bibtex-database-file (expand-file-name (concat db-dir "biblio.bib")))
  (setq gscholar-bibtex-default-source "Google Scholar")
  (gscholar-bibtex-source-on-off :off "IEEE Xplore")
  (gscholar-bibtex-source-on-off :off "DBLP")
  (gscholar-bibtex-source-on-off :off "ACM Digital Library"))

;;; H
(use-package helpful
  :bind (:map help-map
              ("f" . helpful-callable)
              ("v" . helpful-variable)
              ("k" . helpful-key)))

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

(use-package ivy
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-h" . backward-delete-char)
         ("C-w" . backward-kill-word)
         ("<menu>" . zettelkasten-name-of-the-file)
         ("M-y" . ivy-next-line)
         ("M-ü" . ivy-next-line))
  :config
  (ivy-mode 1)
  (setq ivy-height 13)
  ;;     (setq ivy-fixed-height-minibuffer t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-wrap t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy)
  (setq ivy-use-selectable-prompt t)
  ;;     (setq ivy-re-builders-alist
  ;;               '((counsel-ag . ivy--regex-ignore-order)
  ;;                 (t . ivy--regex-plus)))

  (setq ivy-switch-buffer-faces-alist
        '((emacs-lisp-mode . swiper-match-face-1)
          (dired-mode . ivy-subdir)
          (org-mode . org-level-4)))

  (setq ivy-views
        '((",archive"
           (horz
            (file "~/archive/")
            (file "~/archive/.nav-date-description")))
          (",mail"
           (horz
            (buffer "*Group*")
            (buffer "*OfflineIMAP*")))
          (",todo"
           (horz
            (buffer "*pomidor*")
            (buffer "*Org Agenda*"))))))

(use-package ivy-bibtex
  :bind (("C-." . ivy-bibtex)
         ("C-<f5>" . ivy-resume))
  :config
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

  (ivy-bibtex-ivify-action bibtex-completion-edit-logs ivy-bibtex-edit-logs)
  (ivy-add-actions
   'ivy-bibtex
   '(("E" ivy-bibtex-edit-logs "Edit log")))


  (setq bibtex-completion-bibliography (expand-file-name job/bibliography-file))
  (setq bibtex-completion-library-path (expand-file-name texte-dir))
  (setq bibtex-completion-pdf-field "Files")
  (setq bibtex-completion-notes-path (expand-file-name zettel-dir))
  (setq bibtex-completion-notes-extension ".txt")
  (setq bibtex-completion-additional-search-fields '("subtitle"
                                                     "date"
                                                     "keywords"))

  (advice-add 'bibtex-completion-candidates
              :filter-return 'reverse)

  (setq bibtex-completion-cite-default-command "autocite")

  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (start-process "evince" "*bibtex-evince*" "/usr/bin/evince" fpath)))

  (setq bibtex-completion-notes-template-multiple-files
        "#+TITLE: ${author} ${date}: ${title}
#+DATE: [${timestamp}]

* Schlagwörter
tags: §${=key=}, §txt, ${keywords},

* Inhalt

* Literatur

* Links & Files

* Data
** misc
#+begin_src csv :tangle zettel-txt-references-path.csv :padline no
${source},${=key=}
#+end_src")

  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-org-ref-autocite)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default)))

  (defun bibtex-completion-format-citation-org-ref-autocite (keys)
    "Formatter for org-ref references."
    (let* ((prenote (if bibtex-completion-cite-prompt-for-optional-arguments
                        (read-from-minibuffer "Prenote: ") ""))
           (postnote (if bibtex-completion-cite-prompt-for-optional-arguments
                         (read-from-minibuffer "Postnote: ") ""))
           (prenote (if (string= "" prenote) "" (concat prenote "::")))
           (notes (if (string= "" postnote) "" (concat "[" prenote postnote "]"))))
      (format "[[autocite:%s]%s]" (s-join ", " keys) notes)))

  (defun bibtex-completion-apa-get-value (field entry &optional default)
    "Return FIELD or ENTRY formatted following the APA
   guidelines.  Return DEFAULT if FIELD is not present in ENTRY."
    (let ((value (bibtex-completion-get-value field entry))
          (entry-type (bibtex-completion-get-value "=type=" entry)))
      (if value
          (pcase field
            ;; https://owl.english.purdue.edu/owl/resource/560/06/
            ("author" (bibtex-completion-apa-format-authors value))
            ("editor"
             (if (string= entry-type "proceedings")
                 (bibtex-completion-apa-format-editors value)
               (bibtex-completion-apa-format-editors value)))
            ;; When referring to books, chapters, articles, or Web pages,
            ;; capitalize only the first letter of the first word of a
            ;; title and subtitle, the first word after a colon or a dash
            ;; in the title, and proper nouns. Do not capitalize the first
            ;; letter of the second word in a hyphenated compound word.
            ("title" (replace-regexp-in-string ; remove braces
                      "[{}]"
                      "" value))
            ("booktitle" value)
            ;; Maintain the punctuation and capitalization that is used by
            ;; the journal in its title.
            ("pages" (s-join "--" (s-split "[^0-9]+" value t)))
            ("doi" (s-concat " http://dx.doi.org/" value))
            (_ value))
        "")))

  (defun bibtex-completion-format-entry (entry width)
    "Formats a BibTeX entry for display in results list."
    (let* ((fields (list (if (assoc-string "author" entry 'case-fold) "author" "editor")
                         "title" "date" "=has-pdf=" "=has-note=" "=type="))
           (fields (-map (lambda (it)
                           (bibtex-completion-clean-string
                            (bibtex-completion-get-value it entry " ")))
                         fields))
           (fields (-update-at 0 'bibtex-completion-shorten-authors fields)))
      (s-format "$0 $1 $2 $3$4 $5" 'elt
                (-zip-with (lambda (f w) (truncate-string-to-width f w 0 ?\s))
                           fields (list 36 (- width 53) 4 1 1 7)))))

  (defun bibtex-completion-insert-reference (keys)
    "Insert a reference for each selected entry."
    (let* ((refs (--map
                  (s-word-wrap 10000
                               (concat "\n- " (bibtex-completion-apa-format-reference it)))
                  keys)))
      (insert "\n" (s-join "\n" refs) "\n")))

  (defun bibtex-completion-apa-format-reference (key)
    "Returns a plain text reference in APA format for the publication specified by KEY."
    (let*
        ((entry (bibtex-completion-get-entry key))
         (ref (pcase (downcase (bibtex-completion-get-value "=type=" entry))
                ("article"
                 (s-format
                  "${author} ${date}: ${title}. ${subtitle}. In: ${journaltitle}, ${volume}(${number}), ${pages}. ([[file:${=key=}.txt][Zettel]])"
                  'bibtex-completion-apa-get-value entry))
                ("inproceedings"
                 (s-format
                  "${author} ${date}: ${title}. ${subtitle}. In: ${editor} (Hg.): [${crossref}] ${location}: ${publisher}, ${pages}. ([[file:${=key=}.txt][Zettel]])"
                  'bibtex-completion-apa-get-value entry))
                ("book"
                 (s-format
                  "${author} ${date}: ${title}. ${subtitle}. ${location}: ${publisher}. ([[file:${=key=}.txt][Zettel]])"
                  'bibtex-completion-apa-get-value entry))
                ("collection"
                 (s-format
                  "${editor} (Hg.) ${date}: ${title}. ${subtitle}. ${location}: ${publisher}. ([[file:${=key=}.txt][Zettel]])"
                  'bibtex-completion-apa-get-value entry))
                ("mvcollection"
                 (s-format
                  "${editor} (Hg.) ${date}: ${title}. ${subtitle}. ${location}: ${publisher}. ([[file:${=key=}.txt][Zettel]])"
                  'bibtex-completion-apa-get-value entry))
                ("phdthesis"
                 (s-format
                  "${author} ${year}: ${title}. ${subtitle}. (Doctoral dissertation). ${school}, ${location}. ([[file:${=key=}.txt][Zettel]])"
                  'bibtex-completion-apa-get-value entry))
                ("inbook"
                 (s-format
                  "${author} ${date}: ${title}. ${subtitle}. In: [${crossref}] ${location}: ${publisher}, ${pages}. ([[file:${=key=}.txt][Zettel]])"
                  'bibtex-completion-apa-get-value entry))
                ("incollection"
                 (s-format
                  "${author} ${date}: ${title}. ${subtitle}. In: ${editor} (Hg.): [${crossref}] ${location}: ${publisher}, ${pages}. ([[file:${=key=}.txt][Zettel]])"
                  'bibtex-completion-apa-get-value entry))
                ("proceedings"
                 (s-format
                  "${editor} (Hg.) ${date}: ${title}. ${location}: ${publisher}. ([[file:${=key=}.txt][Zettel]])"
                  'bibtex-completion-apa-get-value entry))
                ("unpublished"
                 (s-format
                  "${author} ${date}: ${title}. ${subtitle}. Unpublished manuscript. ([[file:${=key=}.txt][Zettel]])"
                  'bibtex-completion-apa-get-value entry))
                ("online"
                 (s-format
                  "${author} ${date}: ${title}. ${subtitle}. , ${url}. ([[file:${=key=}.txt][Zettel]])"
                  'bibtex-completion-apa-get-value entry))
                (_
                 (s-format
                  "${author} ${date}: ${title}. ${subtitle}. ([[file:${=key=}.txt][Zettel]])"
                  'bibtex-completion-apa-get-value entry)))))
      (replace-regexp-in-string "\\([ .?!]\\)\\." "\\1" ref))) ; Avoid sequences of punctuation marks.


  ;; Eigene Aktion für Logs
  (defcustom bibtex-completion-logs-extension "--log.txt"
    "The extension of the files containing notes.  This is only
used when `bibtex-completion-notes-path' is a directory (not a file)."
    :group 'bibtex-completion
    :type 'string)

  (defcustom bibtex-completion-logs-template-multiple-files
    "#+TITLE: Log: ${author} ${date}: ${title}\n#+DATE: [${timestamp}]\n\n* ${author} ${date}: ${title}\n:PROPERTIES:\n:CATEGORY: wiss\n:END:\n[[autocite:${=key=}]]\n[[file:~/Dropbox/db/zk/zettel/${=key=}.txt][zettel]]\n"
    "Template used to create a new log when each log is stored in
a separate file.  '${field-name}' can be used to insert the value
of a BibTeX field into the template. Fork."
    :group 'bibtex-completion
    :type 'string)

  (defun bibtex-completion-edit-logs (keys)
    "Open the log  associated with the selected entries using `find-file'. Fork from edit-notes"
    (dolist (key keys)
      (if (and bibtex-completion-notes-path
               (f-directory? bibtex-completion-notes-path))
                                        ; One log file per publication:
          (let* ((path (f-join bibtex-completion-notes-path
                               (s-concat key bibtex-completion-logs-extension))))
            (find-file path)
            (unless (f-exists? path)
              (insert (s-format bibtex-completion-logs-template-multiple-files
                                'bibtex-completion-apa-get-value
                                (bibtex-completion-get-entry key)))))
                                        ; One file for all logs:
        (unless (and buffer-file-name
                     (f-same? bibtex-completion-notes-path buffer-file-name))
          (find-file-other-window bibtex-completion-notes-path))
        (widen)
        (outline-show-all)
        (goto-char (point-min))
        (if (re-search-forward (format bibtex-completion-notes-key-pattern (regexp-quote key)) nil t)
                                        ; Existing entry found:
            (when (eq major-mode 'org-mode)
              (org-narrow-to-subtree)
              (re-search-backward "^\*+ " nil t)
              (org-cycle-hide-drawers nil)
              (bibtex-completion-notes-mode 1))
                                        ; Create a new entry:
          (let ((entry (bibtex-completion-get-entry key)))
            (goto-char (point-max))
            (insert (s-format bibtex-completion-notes-template-one-file
                              'bibtex-completion-apa-get-value
                              entry)))
          (when (eq major-mode 'org-mode)
            (org-narrow-to-subtree)
            (re-search-backward "^\*+ " nil t)
            (org-cycle-hide-drawers nil)
            (goto-char (point-max))
            (bibtex-completion-notes-mode 1))))))
  )



;;; K
(use-package key-chord
  :defer 2
  :after (avy ace-window)
  :init
  (progn
    (setq key-chord-two-keys-delay 0.15)
    (setq key-chord-one-key-delay 0.25)
    (key-chord-mode 1)
    (key-chord-define-global "jk" 'avy-goto-char-timer)
    (key-chord-define-global "jl" 'avy-goto-line)
    (key-chord-define-global "jf" 'ace-window)))

(use-package keyfreq
  :init
  (keyfreq-autosave-mode 1)
  (keyfreq-mode 1)
  (setq keyfreq-file (no-littering-expand-var-file-name "keyfreq.el"))
  (setq keyfreq-file-lock (no-littering-expand-var-file-name "keyfreq.lock"))
  (setq keyfreq-excluded-commands
        '(backward-char
          delete-backward-char
          forward-char
          handle-switch-frame
          left-char
          left-word
          mouse-drag-region
          mouse-set-point
          mwheel-scroll
          next-line
          previous-line
          right-char
          right-word
          self-insert-command
          ;;isearch
          isearch-printing-char
          ;;org
          org-self-insert-command
          org-delete-backward-char
          org-return
          org-agenda-next-line
          org-agenda-previous-line
          org-ref-next-key
          ;;Ivy
          ivy-done
          ivy-next-line
          ivy-previous-line
          magit-invoke-popup-action)))

;;; L
(use-package langtool
  :defer t
  :init
  (setq langtool-language-tool-jar "~/programme/LanguageTool-3.1/languagetool-commandline.jar"))

(use-package latex-extra
  :diminish latex-extra-mode
  :hook (TeX-mode . latex-extra-mode))

(use-package ledger-mode
  :mode "\\.dat\\'"
  :config
  (setq ledger-binary-path "~/src/ledger/ledger"))


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

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package messages-are-flowing
  :commands (messages-are-flowing-use-and-mark-hard-newlines)
  :after (message)
  :hook (message-mode . messages-are-flowing-use-and-mark-hard-newlines))

(use-package modalka
  :bind ("<return>" . modalka-mode)
  :config
  (modalka-define-kbd "a" "C-a")
  (modalka-define-kbd "b" "C-b")
  (modalka-define-kbd "c a" "C-c a")
  (modalka-define-kbd "c c" "C-c C-c")
  (modalka-define-kbd "c m" "C-c m")
  (modalka-define-kbd "c z" "C-c z")
  (modalka-define-kbd "d" "C-d")
  (modalka-define-kbd "e" "C-e")
  (modalka-define-kbd "f" "C-f")
  (modalka-define-kbd "g" "C-g")
  (modalka-define-kbd "h" "C-h")
  (modalka-define-kbd "k" "C-k")
  (modalka-define-kbd "l" "C-l")
  (modalka-define-kbd "m" "C-m")
  (modalka-define-kbd "n" "C-n")
  (modalka-define-kbd "o" "C-o")
  (modalka-define-kbd "p" "C-p")
  (modalka-define-kbd "r" "M-r")
  (modalka-define-kbd "s" "C-s")
  (modalka-define-kbd "w" "C-w")
  (modalka-define-kbd "x s" "C-x s")
  (modalka-define-kbd "y" "C-y")
  (modalka-define-kbd "z" "C-z")
  (modalka-define-kbd "W" "M-w")
  (modalka-define-kbd "Y" "M-y")
  (modalka-define-kbd "," "C-,")
  (modalka-define-kbd "<SPC>" "C-<SPC>"))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-<" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :diminish multiple-cursors)

;;; O
(use-package olivetti
  :hook (TeX-mode . olivetti-mode))

(use-package org-autolist
  :commands org-autolist-mode
  :diminish org-autolist-mode
  :init
  (progn
    (add-hook 'org-mode-hook (lambda () (org-autolist-mode)))))

(use-package org-checklist
  :defer 2
  :straight org
  :load-path "~/.emacs.d/straight/repos/org/contrib/lisp")

(use-package org-clock-convenience
  :bind (:map org-agenda-mode-map
              ("<C-S-up>" . org-clock-convenience-timestamp-up)
              ("<C-S-down>" . org-clock-convenience-timestamp-down)
              ("ö" . org-clock-convenience-fill-gap)
              ("ä" . org-clock-convenience-fill-gap-both)))

(use-package org-clock-csv
  :defer 2
  :config
  (defun my/org-clock-csv-calc ()
    "Ruft script auf und verarbeitet die "
    (interactive)
    (shell-command "source ~/script/clock-entries.sh"))

  (defun my/org-clock-csv-write-calc ()
    (interactive)
    (org-clock-csv)
    (my/org-clock-csv-calc)))

(use-package org-collector
  :defer 3
  :straight org
  :load-path "~/.emacs.d/straight/repos/org/contrib/lisp")

(use-package org-contacts
  :defer 2
  :straight org
  :load-path "~/.emacs.d/straight/repos/org/contrib/lisp"
  :config
  (setq org-contacts-files '("~/Dropbox/db/contacts.org"))
  (setq org-contacts-icon-use-gravatar nil)
  (setq org-contacts-birthday-format "%l (%y)"))

(use-package org-indent
  :straight org
  :load-path "~/.emacs.d/straight/repos/org/contrib/lisp"
  :commands org-indent-mode
  :diminish org-indent-mode
  :init
  (progn
    (setq org-indent-mode-turns-on-hiding-stars t)))

(use-package org-gcal
  :straight (org-gcal :type git
                    :host github
                    :repo "kidd/org-gcal.el")
  :defer 2
  :config
  (setq org-gcal-auto-archive t)
  (setq org-gcal-down-days 365)
  (setq org-gcal-client-id "553301842275-clecdgmr7i8741e3ck5iltlgfk3qf79r.apps.googleusercontent.com")
  (setq org-gcal-client-secret "4zyEbm_F_BMuJsA7rZZmgFBm")
  (setq org-gcal-file-alist '(("jobangen@googlemail.com" . "~/Dropbox/db/org/calender.org"))))

(use-package org-listcruncher
  :straight (org-listcruncher :type git
                              :host github
                              :repo "dfeich/org-listcruncher"))

(use-package org-noter
  :config
  (setq org-noter-property-doc-file "INTERLEAVE_PDF")
  (setq org-noter-property-note-location "INTERLEAVE_PAGE_NOTE"))

(use-package org-ref
  :defer 2
  :init
  (bind-key "C-c )" 'org-autocite-complete-link org-mode-map)
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  :config
  (progn
    (require 'org-ref)
    (setq org-ref-notes-directory (expand-file-name zettel-dir))
    (setq org-ref-default-bibliography '("~/Dropbox/db/biblio.bib"))
    (setq org-ref-pdf-directory (expand-file-name texte-dir))
    (setq orhc-bibtex-cache-file (no-littering-expand-var-file-name "org/ref/bibtex-cache.el"))
    (setq org-ref-default-citation-link "autocite")))

(use-package ox-extra
  :defer 3
  :straight org
  :load-path "~/.emacs.d/straight/repos/org/contrib/lisp"
  :config
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(use-package ox-latex
  :defer 2
  :straight org
  :config
  (setq org-latex-listings t)
  (add-to-list 'org-latex-packages-alist '("" "booktabs" t))
  (add-to-list 'org-latex-packages-alist '("" "ellipsis" t))
  (add-to-list 'org-latex-packages-alist '("" "csquotes" t))
  (add-to-list 'org-latex-packages-alist '("" "lmodern" t))
  (add-to-list 'org-latex-packages-alist '("onehalfspacing" "setspace" t))
  (add-to-list 'org-latex-packages-alist '("" "microtype" t))
  (add-to-list 'org-latex-packages-alist '("english, ngerman" "babel" t))
  (add-to-list 'org-latex-packages-alist '("T1" "fontenc" t))
  (add-to-list 'org-latex-packages-alist '("utf8" "inputenc" t))

  (add-to-list 'org-latex-classes
               '("scrartcl"
                 "\\RequirePackage[l2tabu, orthodox]{nag}
          \\documentclass[DIV12, a4paper, 12pt]{scrartcl}
         [NO-DEFAULT-PACKAGES]
         [PACKAGES]
         [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (add-to-list 'org-latex-classes
               '("scrbook"
                 "\\RequirePackage[l2tabu, orthodox]{nag}
          \\documentclass[DIV=12, a4paper, 12pt]{scrbook}
         [NO-DEFAULT-PACKAGES]
         [NO-PACKAGES]
         [EXTRA]"
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("abrechnung"
                 "\\documentclass[DIV=12, a4paper, 12pt]{scrartcl}
          \\usepackage{job-abrechnung-ba}
         [NO-DEFAULT-PACKAGES]
         [PACKAGES]
         [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (add-to-list 'org-latex-classes
               '("zettel"
                 "\\documentclass[DIV=12, a4paper, 12pt, headings=normal]{scrartcl}
          \\usepackage{enumitem}
          \\setlist[itemize]{itemsep=-0.5ex}
         \\makeatletter
         \\def\\maketitle{{\\centering%
         \\par{\\large\\bfseries\\@title\\par\\bigskip}%
         \\noindent}}
         \\makeatother
         [NO-DEFAULT-PACKAGES]
         [PACKAGES]
         [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection*{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection*{%s}" . "\\subsubsection*{%s}")))
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass{beamer}\n
\\usepackage{microtype}
\\usepackage{lmodern}
\\usepackage{csquotes}
\\usepackage{ellipsis}
\\usepackage{booktabs}\n
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\section{%s}")

                 ("\\begin{frame}[fragile]\\frametitle{%s}"
                  "\\end{frame}"
                  "\\begin{frame}[fragile]\\frametitle{%s}"
                  "\\end{frame}")))

  (setq org-latex-default-class "zettel")
  (setq org-export-with-author t)
  (setq org-export-with-date t)
  (setq org-export-with-toc nil)
  (setq org-latex-hyperref-template nil)
  (setq org-latex-tables-booktabs t)
  (setq org-export-default-language "en")
  (setq org-export-with-smart-quotes t)
  (add-to-list 'org-export-smart-quotes-alist
               '("en"
                 (opening-double-quote :utf-8 "“" :html "&ldquo;" :latex "\\enquote{" :texinfo "``")
                 (closing-double-quote :utf-8 "”" :html "&rdquo;" :latex "}" :texinfo "''")
                 (opening-single-quote :utf-8 "‘" :html "&lsquo;" :latex "\\enquote*{" :texinfo "`")
                 (closing-single-quote :utf-8 "’" :html "&rsquo;" :latex "}" :texinfo "'")
                 (apostrophe :utf-8 "’" :html "&rsquo;"))) ;; Export von "" und '' zu csquotes
  )

(use-package ox-reveal
  :straight (org-reveal :type git
                        :host github
                        :repo "lechten/org-reveal")
  :config
  (setq org-reveal-root "file:///home/job/programme/reveal.js"))

;;; P
(use-package paperless
  :defer t
  :config
  (bind-key "C-m" 'paperless-display paperless-mode-map)
  (setq paperless-capture-directory "~/archive/texte/texteingang")
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
  (defun job/save-buffer-no-args ()
  "Save buffer ignoring arguments"
  (save-buffer))

  (advice-add 'pdf-annot-edit-contents-commit :after 'job/save-buffer-no-args)

  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . org-pdfview-open))
  (add-to-list 'org-file-apps
               '("\\.pdf::\\([[:digit:]]+\\)\\'" . org-pdfview-open))
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link)))))

(use-package pomodoro
  :defer t
  :straight (pomodoro :local-repo "~/.emacs.d/lisp/pomodoro"))

(use-package pos-tip)                   ;for sdcv

(use-package projectile
  :defer 2
  :diminish projectile-mode
  :config
  (progn
    (projectile-mode)
    (setq projectile-completion-system 'ivy)
    (setq projectile-enable-caching t)
    (setq projectile-switch-project-action 'projectile-dired)))

;;; R
(use-package reftex
  :hook (TeX-mode . reftex-mode)
  :diminish reftex-mode
  :config
  (progn
    (setq reftex-plug-into-AUCTeX t)
    (setq reftex-sort-bibtex-matches "author")
    (setq reftex-external-file-finders
          '(("tex" . "kpsewhich -format=.tex %f")
            ("bib" . "kpsewhich -format=.bib %f")))
    (setq reftex-default-bibliography '("~/Dropbox/db/biblio.bib"))
    (eval-after-load 'reftex-vars
      '(progn
         (setq reftex-cite-format
               '((?\C-m . "\\autocite[][]{%l}")
                 (?c . "\\cite[][]{%l}")
                 (?t . "\\textcite[][]{%l}")
                 (?y . "\\autocite*[][]{%l}")
                 (?n . "\\nocite{%l}")
                 (?f . "\\footcite[][]{%l}")
                 (?T . "\\textcquote[][]{%l}[]{}")
                 (?B . "\\blockcquote[][]{%l}[]{}")))))

    (setq reftex-cite-prompt-optional-args t)
    (setq reftex-cite-cleanup-optional-arg t)))

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
(use-package sdcv
  :straight (sdcv :type git
                  :host github
                  :repo "stardiviner/sdcv.el")
  :bind (:map sdcv-mode-map
              ("M-p" . sdcv-previous-dictionary)
              ("M-n" . sdcv-next-dictionary))
  :config
  (setq sdcv-dictionary-simple-list
        '("Duden"
          "German - English"
          "English - German")))


(use-package sensitive-mode
  :straight (sensitive-mode :local-repo "~/.emacs.d/lisp/sensitive-mode")
  :mode ("\\.gpg\\'" . sensitive-mode)
  :config
  (setq epg-gpg-program "gpg2")
  ;; fragt in emacs nach pw; braucht "allow-loopback-pinentry" in gpg-agent.conf
  (setq epa-pinentry-mode 'loopback))

(use-package shell-interaction
  :defer 2
  :straight (shell-interaction :local-repo "~/.emacs.d/lisp/shell-interaction")
  :init
    (eval-after-load 'shell-interaction
    `(make-directory ,(concat user-emacs-directory "var/shell-interaction") t))
)

(use-package showtip)                   ; for sdcv

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
  (add-to-list 'sml/replacer-regexp-list '("^~/archive/texts/" ":TXT:") t))

(use-package smartparens
  :defer 2
  :diminish smartparens-mode
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (add-hook 'yas-before-expand-snippet-hook (lambda () (smartparens-mode -1)))
  (add-hook 'yas-after-exit-snippet-hook (lambda () (smartparens-mode 1))))

(use-package shell-pop
  :bind (("C-c j" . shell-pop))
  :config
  (setq shell-pop-universal-key "C-c j")
  (setq shell-pop-default-directory "~/")
  (setq shell-pop-shell-type
        (quote ("ansi-term" "*ansi-term*"
                (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/usr/bin/zsh")
  (setq shell-pop-window-size 45)
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(use-package swiper)

;;; T
(use-package tramp
  :straight nil
  :init
  (setq tramp-persistency-file-name (no-littering-expand-var-file-name "tramp-history.el")))


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
          '("Ding" "Sache" "auch" "bisschen" "dabei" "dann"
            "einfache" "extrem" "gemacht" "halt" "hervorragend"
            "immerhin" "man" "massenhaft" "möglicherweise" "paar" "recht" "schon"
            "sehr" "unglaublich" "viel" "vielleicht" "wenig" "wichtig" "wichtige"
            "wohl" "ziemlich" "überhaupt"))
    (setq writegood-passive-voice-irregulars
          '("gemacht" "geworden" "vorgenommen" "durchgeführt"))

    (defun writegood-passive-voice-font-lock-keywords-regexp ()
      "Generate font-lock keywords regexp for passive-voice"
      (concat "\\b\\(wurde\\)\\b\\([[:space:]]\\|\\s<\\|\\s>\\)+\\(ge[[:word:]]+\\|"
              (regexp-opt writegood-passive-voice-irregulars)
              "\\)\\b"))))

(use-package www-synonyms
  :commands www-synonyms-insert-synonym
  :config
  (setq www-synonyms-key "gaGF6dLppnG6whJVPKFg")
  (setq www-synonyms-lang "de_DE"))

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
  :custom
  (zettelkasten-main-directory "~/Dropbox/db/zk/")
  (zettelkasten-temp-directory "~/.emacs.d/var/zettelkasten/")
  (zettelkasten-bibliography-file job/bibliography-file)
  (zettelkasten-texts-directory "~/archive/texts/")

  :preface
  ;; (add-hook 'after-init-hook 'zettelkasten-parse-values-combined)

  (defun zettelkasten-txt-query ()
    (interactive)
    (counsel-ag nil "~/.custom-temp/pdfs-extracted" nil))

  (defun job/open-at-point ()
    (interactive)
    (if (equal major-mode 'dired-mode)
        (dired-find-file))
    (if (equal major-mode 'org-mode)
        (org-open-at-point)))

  :config
  (bind-key "C-c z" 'hydra-zettelkasten/body))

;;; Hydras
(defhydra hydra-system (:color red
                               :columns 2)
  "System"
  ("w" nmcli-show-short "nmcli-show-short")
  ("t" wlan-toggle "wlan-toggle")
  ("M" mount-lsblk "mount-lsblk")
  ("m" mount-mount-device "mount-mount-device" :color blue)
  ("u" mount-unmount-device "mount-unmount-device" :color blue)
  ("n" neato-graph-bar "neato-graph-bar" :color blue)
  ("S" job/clean-kill-emacs-shutdown "Shutdown" :color blue)
  ("v" vpn-zedat-shell "vpn-zedat" :color blue)
  ("b" battery "battery")
  ("q" nil "Quit" :color blue))
(bind-key "<f1>" 'hydra-system/body)

;;;
(desktop-save-mode 1) ;; Erinnert die zuletzt geöffneten Dateien
(setq desktop-restore-eager 5)
(setq desktop-files-not-to-save "\\(^/[^/:]*:\\|(ftp)$\\|KILL\\)")
(setq desktop-restore-frames nil)

(defhydra hydra-projectile-other-window (:color teal)
  "projectile-other-window"
  ("f"  projectile-find-file-other-window        "file")
  ("g"  projectile-find-file-dwim-other-window   "file dwim")
  ("d"  projectile-find-dir-other-window         "dir")
  ("b"  projectile-switch-to-buffer-other-window "buffer")
  ("q"  nil                                      "cancel" :color blue))

(defhydra hydra-projectile (:color teal
                            :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir
"
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("s-f" projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("g"   ggtags-update-tags)
  ("s-g" ggtags-update-tags)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("s-k" projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("p"   projectile-switch-project)
  ("s"   projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("q"   nil :color blue))
(bind-key* "C-c p" 'hydra-projectile/body)
