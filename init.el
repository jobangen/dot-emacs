;; -*- lexical-binding: t; -*-
(setq package-enable-at-startup nil)
(server-start)

(defvar linux-p
  (string= "gnu/linux" system-type))

(defvar windows-p
  (string= "windows-nt" system-type))

;;; Config
(setq split-width-threshold 110)
(setq split-height-threshold nil)
(setq inhibit-splash-screen t) ;;Remove splash screen
(fset 'yes-or-no-p 'y-or-n-p)
(setq large-file-warning-threshold nil)
(setq sentence-end-double-space nil)
(setq visible-bell t) ;; blinken bei error

(transient-mark-mode 1) ;; No region when it is not highlighted
(global-font-lock-mode 1) ;;syntax highlighting everywhere
(global-visual-line-mode 1) ;;Add proper word wrapping
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(global-auto-revert-mode t) ;;aktualisiert buffer automatisch
(setq auto-revert-interval 3) ;; Prüfinterval in Sek.
(setq auto-revert-verbose nil)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(if windows-p
    (set-clipboard-coding-system 'utf-16le))

(setq-default indent-tabs-mode nil ;; Insert tabs as spaces (not tabs)
              tab-width 4
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
      time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S"
      time-stamp-start "#\\+DATE:[ \t]+\\\\?[\[\"<]+"
      time-stamp-end "\\\\?[\]\">]")
(add-hook 'write-file-hooks 'time-stamp)

(setq reb-re-syntax 'rx)


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
(setq use-package-enable-imenu-support t)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/etc"))

(use-package diminish)

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

(use-package dot-exwm
  :unless windows-p
  :straight exwm)

  (if windows-p
      (setq job/sans-serif-font "Arial")
    (setq job/sans-serif-font "Helvetica Neue LT Std"))

;;; Setup
;; https://sigquit.wordpress.com/2008/09/28/single-dot-emacs-file/

;; Dir
(defvar backup-dir
  (expand-file-name (convert-standard-filename "backups/") user-emacs-directory))

(defvar autosave-dir
   (expand-file-name (convert-standard-filename "autosave/")  user-emacs-directory))

(defvar custom-temp
   (expand-file-name "~/.custom-temp/"))

(defun job/custom-temp-file-name (file)
    (expand-file-name (convert-standard-filename file) custom-temp))

(defvar texte-dir
  (if windows-p
      "c:/Users/jba054/OneDrive - University of Bergen/archive/txt-docs/"
    (expand-file-name "~/archive/texts/")))

(defvar dropbox-dir
  (if windows-p
      nil
    (expand-file-name "~/Dropbox/")))

(defvar db-dir
  (expand-file-name (convert-standard-filename "db/") dropbox-dir))

(setq org-directory
      (if windows-p
          ""
        (expand-file-name (convert-standard-filename "db/org/") dropbox-dir)))

(defvar zettel-dir
  (if windows-p
      "O:/archive/proj/"
    (expand-file-name (convert-standard-filename "db/zk/zettel/") dropbox-dir)))

(defvar zettel-txt-dir
  (if windows-p
      "c:/Users/jba054/OneDrive - University of Bergen/archive/zettel/txt/"
    (expand-file-name (convert-standard-filename "db/zk/zettel/txt/") dropbox-dir)))

(use-package no-littering)


(defvar job/bibliography-file
  (if windows-p
      "c:/Users/jba054/src/bibliography/biblio.bib"
    (expand-file-name (convert-standard-filename "db/biblio.bib") dropbox-dir)))

(global-set-key (kbd "s-<tab>") 'other-window)

;;; Fonts


(setq browse-url-browser-function 'browse-url-generic)
(if windows-p
    (setq browse-url-generic-program "c:/Program Files/Mozilla Firefox/firefox.exe")
  (setq browse-url-generic-program "firefox"))

;;; org
(use-package dot-org
  :demand t
  :straight org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c i" . org-clock-in)
         ("C-c l" . org-store-link)
         ("C-c C-j" . org-journal-new-entry)
         :map org-agenda-mode-map
         ("C-c d" . job/org-agenda-add-tags-today)
         ("C-c w" . job/add-tag-this-week-dwim))
  :init
  (if windows-p
      (progn
        (setq org-default-notes-file "c:/Users/jba054/OneDrive - University of Bergen/archive/zettel/2022-03-16-0946-inbox.org"))
    (setq org-default-notes-file "inbox.org"))
  (setq org-agenda-time-grid '((daily today require-timed)
                               (800 1000 1200 1400 1600 1800 2000)
                               "" "-------------"))
  (setq org-agenda-current-time-string "<-------- now")
  (setq org-agenda-block-separator "")

  )
(if windows-p
    (setq org-agenda-category-icon-alist
          '(("mobile" "/home/job/.emacs.d/icons/arrow-right-hollow-12.png" nil nil :ascent center)
            ("inbox" "~/.emacs.d/icons/fast-forward-16.png" nil nil :ascent center)
            ("appt" "/home/job/.emacs.d/icons/appt-16.png" nil nil :ascent center)
            ("mail" "/home/job/.emacs.d/icons/mail-16.png" nil nil :ascent center)
            ("diss" "/home/job/.emacs.d/icons/phd-16.png" nil nil :ascent center)
            ("wiss" "/home/job/.emacs.d/icons/search.png" nil nil :ascent center)
            ("pers" "/home/job/.emacs.d/icons/user-16.png" nil nil :ascent center)
            ("zkt" "/home/job/.emacs.d/icons/network-16.png" nil nil :ascent center)
            ("arbeit" "~/.emacs.d/icons/briefcase-16.png" nil nil :ascent center)))
  (setq org-agenda-category-icon-alist
        '(("mobile" "/home/job/.emacs.d/icons/arrow-right-hollow-12.png" nil nil :ascent center)
          ("inbox" "/home/job/.emacs.d/icons/arrow-right-hollow-12.png" nil nil :ascent center)
          ("appt" "/home/job/.emacs.d/icons/appt-16.png" nil nil :ascent center)
          ("mail" "/home/job/.emacs.d/icons/mail-16.png" nil nil :ascent center)
          ("diss" "/home/job/.emacs.d/icons/phd-16.png" nil nil :ascent center)
          ("wiss" "/home/job/.emacs.d/icons/search.png" nil nil :ascent center)
          ("pers" "/home/job/.emacs.d/icons/user-16.png" nil nil :ascent center)
          ("zkt" "/home/job/.emacs.d/icons/network-16.png" nil nil :ascent center)
          ("arbeit" "/home/job/.emacs.d/icons/briefcase-16.png" nil nil :ascent center))))

;; Backup
(setq backup-directory-alist
      `(("." . ,backup-dir)))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq vc-follow-symlinks t)
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms
      `((".*" ,autosave-dir t)))

;;; Libraries
(use-package counsel-projectile   :defer 3)
(use-package define-word
  :unless windows-p
  :commands define-word define-word-at-point)
;; (use-package ess                  :commands R)
(use-package emacsql :straight (emacsql :type git
                                        :host github
                                        :repo "magit/emacsql"
                                        :files ("*.el")))

(emacsql-fix-vector-indentation)
(use-package flyspell-correct-ivy :after (flyspell-correct ivy flyspell))
(use-package git-timemachine :disabled      :defer t)
(use-package goldendict :disabled           :commands goldendict-dwim)
(use-package hydra)
(use-package iso-transl           :defer 2 :straight nil)
(use-package ivy-hydra            :after (ivy hydra))
(use-package ivy-pass :unless windows-p             :defer t :after (ivy pass))
(use-package neato-graph-bar :unless windows-p      :defer t)
(use-package neotree :disabled              :defer t)
(use-package nov :disabled                  :mode ("\\.epub\\'" . nov-mode))
(use-package orglink :unless windows-p :hook (TeX-mode . orglink-mode))
(use-package pass :unless windows-p                 :defer t)
(use-package peep-dired :disabled           :defer t)
(use-package ttl-mode             :defer t)
(use-package wgrep                :defer 3)
(use-package ob-mermaid)

(org-babel-load-file "~/.emacs.d/myinit.org")

;;; Keybindings
(bind-keys
 ("C-x C-r" . revert-buffer)
 ("M-n" . forward-paragraph)
 ("M-p" . backward-paragraph))


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
  :disabled
  :defer t
  :straight (academic-phrases :type git
                              :host github
                              :repo "nashamri/academic-phrases"))

(use-package ace-window
  :after (avy)
  :bind ("M-o" . ace-window)
  :config
  (progn
    (setq aw-scope 'frame)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    (setq aw-dispatch-always nil)))

(use-package add-node-modules-path      ;; javascript
  :defer t
  :hook (((js2-mode rjsx-mode) . add-node-modules-path)))

(use-package auto-yasnippet
  :disabled
  :bind (("H-w" . aya-create)
         ("H-y" . aya-expand)))

(use-package avy
  :bind (("M-s" . avy-goto-char-timer)
         ("M-j". avy-goto-char-timer)
         )
  :config
  (defun avy-action-org-todo (pt)
    (save-excursion
      (goto-char pt)
      (if (string-equal major-mode "org-agenda-mode")
          (org-agenda-todo)
        (org-todo)
        )
      )
    t)

  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)


  (progn
    (setq avy-all-windows t)
    ;; (setq avy-keys '(?w ?e ?r ?u ?i ?o ?a ?s ?d ?f ?g ?h ?j ?k ?l ?ö ?v ?b ?n ?m))
    (setq avy-keys '(?a ?s ?d ?f ?j ?k ?l ?ø))
    (setf (alist-get ?t avy-dispatch-alist) 'avy-action-org-todo)
    (define-key input-decode-map (kbd "C-i") (kbd "H-i"))))

;;; B
(use-package bash-completion
  :unless windows-p
  :init
  (bash-completion-setup))

(use-package beacon
  :defer 2
  :disabled
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

(use-package blacken ;; python
  :disabled
  :after elpy
  :hook (python-mode-hook . blacken-mode)
  )

(use-package blimp
  :disabled
  :straight (blimp :type git
                   :host github
                   :repo "walseb/blimp")
  :hook (image-mode-hook . blimp-mode))

(use-package bookmark+
  :straight (bookmark-plus)
  :init
  (setq bmkp-bmenu-state-file
        (no-littering-expand-var-file-name "bmkp/bmenu-state.el"))
  (setq bookmark-save-flag 1))


;;; C
;; (use-package calendar
;;   :bind (("C-c b" . calendar)
;;          :map calendar-mode-map
;;          ("i d" . job/diary-insert-entry)
;;          ("n" . calendar-forward-week)
;;          ("p" . calendar-backward-week)
;;          ("f" . calendar-forward-day)
;;          ("b" . calendar-backward-day))
;;   :config
;;   (setq calendar-date-style 'iso)
;;   (setq calendar-set-date-style 'iso)
;;   (setq calendar-time-display-form '(24-hours ":" minutes))
;;   (setq calendar-mark-diary-entries-flag t) ;; markiert Tage mit Eintrag automatisch
;;   (setq calendar-view-diary-initially-flag t) ;; Öffnet Diary-window automatisch
;;   (setq calendar-location-name "Berlin")

;;   (add-hook 'calendar-today-visible-hook 'calendar-mark-today))

(use-package calendar
  :bind (("C-c b" . calendar))
  :config
  (setq calendar-week-start-day 1)
  (setq calendar-latitude 52.450894)
  (setq calendar-longitude 13.30857)
  (setq calendar-location-name "Berlin")
  (setq calendar-week-start-day 1
        calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch"
                                 "Donnerstag" "Freitag" "Samstag"]
        calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai"
                                   "Juni" "Juli" "August" "September"
                                   "Oktober" "November" "Dezember"]
        calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'font-lock-function-name-face))
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today))

(use-package calfw
  :disabled
  ;; :bind (("C-c f" . job/open-org-calendar))
  :config
  (setq calendar-week-start-day 1)
  ;;
  (defun job/open-org-calendar ()
    (interactive)
    (delete-other-windows)
    (cfw:open-org-calendar)))

(use-package calfw-org
  :disabled
  :after (calfw)
  :config
  (setq cfw:org-agenda-schedule-args '(:sexp :timestamp))
  (setq cfw:render-line-breaker 'cfw:render-line-breaker-simple)
  (setq cfw:render-line-breaker-none t))

(use-package char-menu
  :defer t
  :bind ("<f2>" . char-menu)
  :config
  (setq char-menu
        '("–" "—" "„“" "‚‘" "“”" "‘’" "»«" "›‹" "«»" "‹ ›" "…"
          ("deutsch" "„“" "»«" "…")
          ("Typography" "•" "©" "†" "‡" "°" "·" "§" "№" "★")
          ("Math" "≈" "≡" "≠" "∞" "×" "±" "∓" "÷" "√" "⊂" "⊃")
          ("Arrows" "←" "→" "↑" "↓" "⇐" "⇒" "⇑" "⇓")
          ("Greek" "α" "β" "Y" "δ" "ε" "ζ" "η" "θ" "ι" "κ" "λ" "μ" "ν" "ξ" "ο" "π" "ρ" "σ" "τ" "υ" "φ" "χ" "ψ" "ω")
          ("Other Languages" "Œ"))))


(use-package shell-maker
  :straight (shell-maker
             :type git
             :host github
             :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package chatgpt-shell
  :requires shell-maker
  :straight (chatgpt-shell
             :type git
             :host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el"))
  :bind (;; ("C-c g" . job/chatgpt-shell-dwim)
         ("C-c G" . chatgpt-shell-prompt))
  :init
  (setq chatgpt-shell-model-version "gpt-3.5-turbo-0125")
  (setq chatgpt-shell-openai-key
        (auth-source-pick-first-password :host "api.openai.com"))
  (defun job/chatgpt-shell-dwim ()
    (interactive)
    (if (string-equal major-mode "chatgpt-shell-mode")
        (progn
          (bury-buffer)
          (other-window 1))
      (other-window 1)
      (chatgpt-shell)))

  (setq chatgpt-shell-system-prompts
        `(
          ("General" . "You use markdown liberally to structure responses. Always show code snippets in markdown blocks with language labels.")
          ("Language" . "You are a language teacher and assist with translation, correcting texts and other language related tasks.
                        You comment briefly on the task and explain changes in translation and explain important word choices.")
          ("Python" . "The user is a programmer with very limited time.
                        You treat their time as precious. You do not repeat obvious things, including their query.
                        You are as concise as possible in responses.
                        You never apologize for confusions because it would waste their time.
                        You use markdown liberally to structure responses.
                        Always show code snippets in markdown blocks with language labels.
                        Don't explain code snippets.
                        Whenever you output updated code for the user, only show diffs, instead of entire snippets.
                        The user uses python 3 and you adapt your answers to the programming language.")
          ("Frontend" . "The user is a programmer with very limited time.
                        You treat their time as precious. You do not repeat obvious things, including their query.
                        You are as concise as possible in responses.
                        You never apologize for confusions because it would waste their time.
                        You use markdown liberally to structure responses.
                        Always show code snippets in markdown blocks with language labels.
                        Don't explain code snippets.
                        Whenever you output updated code for the user, only show diffs, instead of entire snippets.
                        The user uses nuxt 3 with typescript and you adapt your answers to this framework if relevant for the answer.")
          ("eLisp" . "The user is a programmer with very limited time.
                        You treat their time as precious. You do not repeat obvious things, including their query.
                        You are as concise as possible in responses.
                        You never apologize for confusions because it would waste their time.
                        You use markdown liberally to structure responses.
                        Always show code snippets in markdown blocks with language labels.
                        Don't explain code snippets.
                        Whenever you output updated code for the user, only show diffs, instead of entire snippets.
                        The user uses eLisp and emacs 29 and you adapt your answers to the programming language and emacs version.")
          ("Linked Data" . "The user is a programmer with very limited time.
                        You treat their time as precious. You do not repeat obvious things, including their query.
                        You are as concise as possible in responses.
                        You never apologize for confusions because it would waste their time.
                        You use markdown liberally to structure responses.
                        Always show code snippets in markdown blocks with language labels.
                        Don't explain code snippets.
                        Whenever you output updated code for the user, only show diffs, instead of entire snippets.
                        The user work with linked data and uses RDF, OWL, SKOS and SPARQL-queries. You adapt your answers to these technologies if relevant")))

  )

(use-package claude-shell
  :straight (claude-shell :type git :host github :repo "arminfriedl/claude-shell")
  :bind (("C-c g" . job/claude-shell-dwim))
  :config
  (setq claude-shell-api-token (lambda () (auth-source-pick-first-password :host "api.anthropic.com")))
  (setq claude-shell-streaming t)
  (setq claude-shell-system-prompts
        `(
          ("General" . "You use markdown liberally to structure responses. Always show code snippets in markdown blocks with language labels.")
          ;; Based on https://github.com/benjamin-asdf/dotfiles/blob/8fd18ff6bd2a1ed2379e53e26282f01dcc397e44/mememacs/.emacs-mememacs.d/init.el#L768
          ("Programming" . "The user is a programmer with very limited time.
                        You treat their time as precious. You do not repeat obvious things, including their query.
                        You are as concise as possible in responses.
                        You never apologize for confusions because it would waste their time.
                        You use markdown liberally to structure responses.
                        Always show code snippets in markdown blocks with language labels.
                        Don't explain code snippets.
                        Whenever you output updated code for the user, only show diffs, instead of entire snippets.")
          ("Language" . "You are a language teacher and assist with translation, correcting texts and other language related tasks.
                        You comment briefly on the task and explain changes in translation and explain important word choices. If the prompt starts with a language code like nb you translate the following text to that language.")
          ("Python" . "The user is a programmer with very limited time.
                        You treat their time as precious. You do not repeat obvious things, including their query.
                        You are as concise as possible in responses.
                        You never apologize for confusions because it would waste their time.
                        You use markdown liberally to structure responses.
                        Always show code snippets in markdown blocks with language labels.
                        Don't explain code snippets.
                        Whenever you output updated code for the user, only show diffs, instead of entire snippets.
                        The user uses python 3 and you adapt your answers to the programming language.")
          ("Frontend" . "The user is a programmer with very limited time.
                        You treat their time as precious. You do not repeat obvious things, including their query.
                        You are as concise as possible in responses.
                        You never apologize for confusions because it would waste their time.
                        You use markdown liberally to structure responses.
                        Always show code snippets in markdown blocks with language labels.
                        Don't explain code snippets.
                        Whenever you output updated code for the user, only show diffs, instead of entire snippets.
                        The user uses nuxt 3 with typescript and you adapt your answers to this framework if relevant for the answer.")
          ("eLisp" . "The user is a programmer with very limited time.
                        You treat their time as precious. You do not repeat obvious things, including their query.
                        You are as concise as possible in responses.
                        You never apologize for confusions because it would waste their time.
                        You use markdown liberally to structure responses.
                        Always show code snippets in markdown blocks with language labels.
                        Don't explain code snippets.
                        Whenever you output updated code for the user, only show diffs, instead of entire snippets.
                        The user uses eLisp and emacs 29 and you adapt your answers to the programming language and emacs version.")
          ("Linked Data" . "The user is a programmer with very limited time.
                        You treat their time as precious. You do not repeat obvious things, including their query.
                        You are as concise as possible in responses.
                        You never apologize for confusions because it would waste their time.
                        You use markdown liberally to structure responses.
                        Always show code snippets in markdown blocks with language labels.
                        Don't explain code snippets.
                        Whenever you output updated code for the user, only show diffs, instead of entire snippets.
                        The user work with linked data and uses RDF, OWL, SKOS and SPARQL-queries. You adapt your answers to these technologies if relevant.")))

  (defun job/claude-shell-dwim ()
    (interactive)
    (if (string-equal major-mode "claude-shell-mode")
        (progn
          (bury-buffer)
          (other-window 1))
      (other-window 1)
      (claude-shell))))


(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq company-show-numbers 't)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  )

(use-package contacts
  :disabled
  :straight (scimax :type git
                    :host github
                    :repo "jkitchin/scimax"
                    :files ("contacts.el"))
  ;; :bind (("C-c g" . ivy-contacts))
  :config
  (setq contacts-files '("~/Dropbox/db/contacts.org"))
  (setq contacts-cache-file (no-littering-expand-var-file-name "contacts-cache.el")))

(use-package counsel
  :bind (("C-c o" . counsel-outline)
         ("C-M-s" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-x f" . counsel-flycheck)
         ("M-y" . counsel-yank-pop)
         ("M-x" . counsel-M-x)
         ("C-x C-m" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-SPC" . counsel-mark-ring)
         ("C-x H-i" . counsel-imenu)))

(use-package counsel-notmuch
  :disabled
  :bind ("C-c u" . counsel-notmuch))

(use-package counsel-org-clock
  :bind ("C-c C-x c" . counsel-org-clock-history)
  :config
  (setq counsel-org-clock-default-action 'counsel-org-clock-clock-dwim-action))

(use-package csv-mode
  :defer t
  :config
  (setq csv-separators '("," ";"))
  (setq csv-align-padding 2))

(use-package ctrlf
  :straight (ctrlf :type git
                   :host github
                   :repo "raxod502/ctrlf")
  :config
  (setq ctrlf-mode-bindings '(("C-s" . ctrlf-forward-literal)
                              ("C-r" . ctrlf-backward-literal)))
  (ctrlf-mode 1))

;;; D
(use-package diary-lib
  :disabled
  :config
  (setq diary-file "~/Dropbox/db/diary")
  (setq diary-date-forms diary-iso-date-forms)
  (setq diary-list-include-blanks t)
  (setq diary-number-of-entries [1 5 4 3 2 2 1])

  (defun job/diary-insert-entry (arg)
    "See `insert-diary-entry'."
    (interactive "P")
    (let ((calendar-date-display-form
           '(year "-" month "-" day)))
      (diary-make-entry (calendar-date-string (calendar-cursor-to-date t) t)
                        arg)))

  (add-hook 'diary-list-entries-hook 'diary-sort-entries t))

(use-package deadgrep
  :unless windows-p
  :commands deadgrep
  :straight (deadgrep :type git
                      :host github
                      :repo "Wilfred/deadgrep"))

(use-package deft
  :disabled
  :defer t
  :bind (:map deft-mode-map
              ("C-h" . deft-filter-decrement)
              ("C-w" . deft-filter-decrement-word))
  :config
  (setq deft-directory "/home/job/Dropbox/db/zk/zettel")
  (setq deft-file-limit 200)
  (setq deft-new-file-format "%Y-%m-%d-%H%M"))

(use-package dot-auctex :straight auctex
  :demand t
  :mode ("\\.tex$" . TeX-latex-mode)
  :hook ((TeX-mode . TeX-fold-mode)
         (TeX-mode . variable-pitch-mode)
         (TeX-mode . LaTeX-math-mode))
  :config
  (eval-after-load 'tex-mode
    '(bind-key "C-:" 'reftex-citation LaTeX-mode-map))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  (defun job/latex-notes ()
    (interactive)
    (let ((note
           (read-string "Notiz: ")))
      (save-excursion
        (search-forward-regexp "% # Stand" nil t)
        (backward-paragraph)
        (insert (concat note "%"))
        (TeX-newline))))

  (eval-after-load 'latex-mode
    '(bind-key "C-c n" 'job/latex-notes latex-mode-map)))

(use-package dot-defun
  :straight nil
  :demand t
  :bind (("C-a" . job/beginning-of-line-or-indentation)
         ("C-k" . job/kill-line)
         ("C-w" . job/kill-word-or-region)
         ("C-c d" . job/insert-date)
         ("C-c f" . job-dired-goto-my-dirs)
         ("C-x C-v" . job/find-file-as-sudo)
         ("M-c" . capitalize-word)
         ("M-l" . downcase-word)
         ("C-c n". job-navigate-date-description)))

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
        trash-directory "~/.local/share/Trash/files")
  (setq dired-listing-switches "--group-directories-first -alh1v")
  (put 'dired-find-alternate-file 'disabled nil)
  (add-hook 'dired-after-readin-hook 'hl-line-mode)
  )

(use-package dired-collapse
  :disabled
  :hook (dired-mode . dired-collapse-mode))

(use-package dired-hide-details
  :straight nil
  :hook (dired-mode .  dired-hide-details-mode))

(use-package dired-narrow
  :bind (:map dired-mode-map
              ("C-s" . dired-narrow)))

;; (use-package dired-
;;   :hook (dired-mode . dired-filter-group-mode)
;;   :config
;;   (setq dired-filter-group-saved-groups
;;         '(("default"
;;            ("DIR"
;;             (directory))
;;            ("PDF"
;;             (extension "pdf"))
;;            ("LaTeX"
;;             (extension "tex" "bib"))
;;            ("Text & Data"
;;             (extension "org" "txt" "doc" "docx" "csv" "odt"))
;;            ("Media"
;;             (extension "JPG" "jpg" "PNG" "png" "gif" "bmp" "svg"))
;;            ("Archives"
;;             (extension "zip" "rar" "gz" "bz2" "tar" "org_archive"))))))

(use-package dired-launch
  :hook (dired-mode . dired-launch-mode)
  :bind (:map dired-mode-map
              ("J" . dired-launch-command)
              ("K" . dired-launch-with-prompt-command))
  :diminish dired-launch-mode
  :init
  (if windows-p
      (setf dired-launch-extensions-map '())
    (setf dired-launch-extensions-map
          '(;;Archives
            ("gz" ("file-roller"))
            ;; Office
            ("odt" ("libreoffice"))
            ("doc" ("libreoffice"))
            ("docx" ("libreoffice"))
            ("csv" ("libreoffice"))
            ("ppt" ("libreoffice"))
            ("pptx" ("libreoffice"))
            ("pdf" ("evince" "gimp-2.10"))
            ("PDF" ("evince " "gimp-2.10"))
            ;; Web
            ("html" ("firefox"))
            ;; Pictures
            ("jpg" ("eog" "gimp-2.10"))
            ("png" ("feh" "eog" "gimp-2.10"))
            ("svg" ("eog" "gimp-2.10"))
            ;; Audio
            ("wav" ("rhythmbox"))
            ("WAV" ("rhythmbox"))
            ("mp3" ("rhythmbox"))
            ;; Video
            ("mov" ("totem" "vlc"))
            ;; gpx
            ("gpx" ("viking"))
            ;; Other
            ("mm" ("freeplane")))))

  )

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
              ("<tab>" . dired-subtree-toggle)))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g q" . dumb-jump-quick-look)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;; E
(use-package edbi :disabled)

(use-package ediff
  :config (set 'ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package elfeed
  :commands elfeed
  :bind (:map elfeed-show-mode-map
              ("Z" . zettelkasten-elfeed-new-zettel)
              ("z" . zettelkasten-elfeed-new-bib)
              ("d" . doi-utils-add-entry-from-elfeed-entry)
              ("n" . zettelkasten-elfeed-skip)))

(use-package elfeed-score
  :unless windows-p
  :config
  (progn
    (elfeed-score-enable)
    (setq elfeed-use-curl t)
    (setq elfeed-curl-max-connections 8)
    (setq elfeed-curl-timeout 20)
    (define-key elfeed-search-mode-map "=" elfeed-score-map)
    (setq elfeed-search-print-entry-function #'elfeed-score-print-entry)
    (setq elfeed-score-serde-score-file (no-littering-expand-var-file-name "elfeed/elfeed-score"))))


(use-package elfeed-org
  :after elfeed
  :init
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/etc/elfeed/elfeed.org")))


(use-package elmacro
  :disabled
  :defer 2
  :diminish elmacro-mode
  :config
  (elmacro-mode))

(use-package elpy
  :ensure t
  :bind (:map elpy-mode-map
              ("C-c C-c" . job/elpy-shell-dwim))
  :init
  (setq elpy-test-runner 'elpy-test-pytest-runner)
  (setq python-indent-offset 4)
  ;; Use the working environment as the RPC environment
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))
  
  (elpy-enable)

  (defun job/elpy-shell-dwim ()
    (interactive)
    (when (get-buffer "*Python*")
      (let ((process (get-buffer-process "*Python*")))
        (when process
          (set-process-query-on-exit-flag process nil)))
      (kill-buffer "*Python*"))
    (elpy-shell-send-buffer)
    ;; (switch-to-buffer-other-window "*Python*")
    (end-of-buffer)
    (other-window 1)
    (when (get-buffer "*projectile-files-errors*")
      (kill-buffer "*projectile-files-errors*")
      )
    ))

(use-package flycheck-eglot
  :disabled
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package eglot
  :disabled
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-events-buffer-size 0)
  (setq eglot-sync-connect nil))


(use-package engine-mode
  :unless windows-p
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

(use-package evil
  :config
  (setq evil-default-state 'emacs)
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil)
  (evil-mode 1))

(use-package expand-region
  :bind (("C-c m" . er/expand-region)))

;;; F
(use-package filetags
  :straight (filetags :type git
                      :host github
                      :repo "DerBeutlin/filetags.el")
  :commands filetags-extract-filetags
  :bind (:map dired-mode-map
              ("#" . filetags-dired-update-tags))
  :init
  (setq filetags-load-controlled-vocabulary-from-file t))


(use-package flycheck
  ;; :defer 2
  :diminish
  ;; :init (global-flycheck-mode)
  :hook (python-mode . flycheck-mode)
  :config
  (setq flycheck-display-errors-delay 0.3)
  (custom-set-variables
   '(flycheck-python-flake8-executable "python")
   '(flycheck-python-pycompile-executable "python")
   '(flycheck-python-pylint-executable "python"))
  
  (flycheck-def-config-file-var flycheck-python-ruff-config python-ruff
                                '("pyproject.toml" "ruff.toml" ".ruff.toml"))

  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using the ruff.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.

See URL `https://beta.ruff.rs/docs/'."
    :command ("ruff"
              "check"
              (config-file "--config" flycheck-python-ruff-config)
              "--output-format=concise"
              "--stdin-filename" source-original
              "-")
    :standard-input t
    :error-filter (lambda (errors)
                    (let ((errors (flycheck-sanitize-errors errors)))
                      (seq-map #'flycheck-flake8-fix-error-level errors)))
    :error-patterns
    ((warning line-start
              (file-name) ":" line ":" (optional column ":") " "
              (id (one-or-more (any alpha)) (one-or-more digit)) " "
              (message (one-or-more not-newline))
              line-end))
    :modes (python-mode python-ts-mode)
    :next-checkers ((warning . python-pyright)))

  (defun python-flycheck-setup ()
    (progn
      (flycheck-select-checker 'python-ruff)
      (flycheck-add-next-checker 'python-ruff 'python-pyright)))

  (add-to-list 'flycheck-checkers 'python-ruff)
  (add-hook 'python-mode-local-vars-hook #'python-flycheck-setup 'append)

  (if windows-p
      (setq flycheck-lua-luacheck-executable "c:/Users/jba054/src/luacheck/luacheck.exe")))

(use-package flyspell
  :unless windows-p
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
  ;; https://www.reddit.com/r/emacs/comments/4oc7pg/spellcheck_flyspellmode_underlines_disappear_when/
  ;; läuft leider immer nach der Prüfung; auch wenn nicht gesichert wurde
  ;; (defun flyspell-buffer-after-pdict-save (&rest _)
  ;;   (flyspell-buffer))

  ;; (advice-add 'ispell-pdict-save :after #'flyspell-buffer-after-pdict-save)

  (defun ispell-pdict-save (&optional no-query force-save)
    "Check to see if the personal dictionary has been modified.
If so, ask if it needs to be saved."
    (interactive (list ispell-silently-savep t))
    (if (and ispell-pdict-modified-p (listp ispell-pdict-modified-p))
        (setq ispell-pdict-modified-p (car ispell-pdict-modified-p)))
    (when (and (or ispell-pdict-modified-p force-save)
               (or no-query
                   (y-or-n-p "Personal dictionary modified.  Save? ")))
      (ispell-send-string "#\n")        ; save dictionary
      (message "Personal dictionary saved.")
      (when flyspell-mode
        (flyspell-mode 0)
        (flyspell-mode 1)
        (flyspell-buffer)))
    ;; unassert variable, even if not saved to avoid questioning.
    (setq ispell-pdict-modified-p nil)))

(use-package flyspell-correct
  :bind (("C-," . flyspell-correct-wrapper))
  :config
  (setq flyspell-correct-interface 'flyspell-correct-ivy))

(use-package forge
  :straight (forge :type git
                   :host github
                   :repo "magit/forge")
  :after magit
  :config
  (setq auth-sources '("~/.authinfo.gpg"))
  (add-to-list 'forge-alist '("git.app.uib.no" "git.app.uib.no/api/v4" "git.app.uib.no" forge-gitlab-repository))
  (add-to-list 'forge-alist '("itgit.app.uib.no" "itgit.app.uib.no/api/v4" "itgit.app.uib.no" forge-gitlab-repository))
  (use-package orgit)
  (use-package orgit-forge))



;;; g
(use-package german-holidays
  :defer 2
  :config
  (setq holiday-other-holidays holiday-german-holidays))

(use-package gnorb
  :disabled
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
  :disabled
  :straight (gnuplot-mode :type git
                          :host github
                          :repo "bruceravel/gnuplot-mode")
  :mode ("\\.plot\\'" . gnuplot-mode))

(use-package gnus
  :disabled
  :commands gnus
  :straight nil
  :init
  (setq gnus-init-file (no-littering-expand-etc-file-name "gnus-config.el")))

(use-package gnus-recent
  :disabled
  :after gnus
  :straight (gnus-recent :type git
                         :host github
                         :repo "unhammer/gnus-recent")
  :config
  (define-key gnus-summary-mode-map (kbd "l") #'gnus-recent-goto-previous)
  (define-key gnus-group-mode-map (kbd "C-c L") #'gnus-recent-goto-previous))

(use-package gnus-dired
  :disabled
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

(use-package go-translate
  :config
  (setq go-translate-buffer-follow-p t)
  (setq go-translate-local-language "de")
  (setq go-translate-target-language "en")
  (setq go-translate-extra-directions '(("de" . "no") ("en" . "no")))
  (setq go-translate-token-current (cons 430675 2721866130)))

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
  :bind (("C-c C-?" . help)
         (:map help-map
               ("f" . helpful-callable)
               ("v" . helpful-variable)
               ("k" . helpful-key))))

(use-package hl-line+
  :hook
  (focus-in . hl-line-flash)
  :custom
  (global-hl-line-mode nil)
  (hl-line-flash-show-period 0.5)
  (hl-line-inhibit-highlighting-for-modes '(dired-mode))
  (hl-line-overlay-priority -100) ;; sadly, seems not observed by diredfl
  )

(use-package highlight-indentation
  :init
  (progn
    (defun set-hl-indent-color ()
      (set-face-background 'highlight-indentation-face "#e3e3d3")
      (set-face-background 'highlight-indentation-current-column-face "#c3b3b3"))
    (add-hook 'python-mode-hook 'highlight-indentation-mode)
    (add-hook 'python-mode-hook 'set-hl-indent-color)))

(use-package hippie-expand
  :straight nil
  :bind ( ("M-<tab>" . hippie-expand)
          ("C-<tab>" .  hippie-expand))
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
  :bind (("C-." . job/ivy-bibtex-try-last-citation)
         ("C-<f5>" . ivy-resume))
  :config
  (setq bibtex-completion-find-additional-pdfs t)
  (setq bibtex-completion-pdf-extension '(".pdf" ".txt")) ;; Solution for listing add files in buffer
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

  (setq ivy-re-builders-alist
        '((ivy-bibtex . ivy--regex-ignore-order)
          (t . ivy--regex-plus)))

  (ivy-bibtex-ivify-action bibtex-completion-edit-logs ivy-bibtex-edit-logs)
  (ivy-add-actions
   'ivy-bibtex
   '(("E" ivy-bibtex-edit-logs "Edit log")))


  (setq bibtex-completion-bibliography (expand-file-name job/bibliography-file))
  (setq bibtex-completion-library-path (expand-file-name texte-dir))
  (setq bibtex-completion-pdf-field "File")
  (setq bibtex-completion-notes-path (expand-file-name zettel-txt-dir))
  (setq bibtex-completion-notes-extension ".org")
  (setq bibtex-completion-additional-search-fields '("subtitle"
                                                     "date"
                                                     "keywords"))

  (advice-add 'bibtex-completion-candidates
              :filter-return 'reverse)

  (setq bibtex-completion-cite-default-command "autocite")

  (defun bibtex-completion-open-pdf-external (keys &optional fallback-action)
    (let ((bibtex-completion-pdf-open-function
           (lambda (fpath)

             (if windows-p
                 (start-process "sumatra" "*bibtex-sumatra*" "C:/Users/jba054/AppData/Local/SumatraPDF/SumatraPDF.exe" fpath)
               (start-process "evince" "*bibtex-evince*" "/usr/bin/evince" fpath)))))
      (bibtex-completion-open-pdf keys fallback-action)))

  (ivy-bibtex-ivify-action bibtex-completion-open-pdf-external ivy-bibtex-open-pdf-external)


  (ivy-add-actions
   'ivy-bibtex
   '(("P" ivy-bibtex-open-pdf-external "Open PDF file in external Viewer")))


  (setq bibtex-completion-notes-template-multiple-files
        "#+TITLE: ${author} ${date}: ${title}
#+DATE: [${timestamp}]
#+RDF_TYPE: zktb:${=type=}
#+DESCRIPTOR: ${keywords}

* Meta
- Auth:
- Date: [[${=key=}::zk:dct:issued::${date}][${date}]]
- Lang: [[${=key=}::zk:dct:language::${language}][${language}]]

* Inhalt

* Literatur

* Links & Files")

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
    (let* ((fields (list "=has-pdf=" (if (assoc-string "author" entry 'case-fold) "author" "editor")
                         "date" "title" "=key=" "=type=" "=has-note="))
           (fields (-map (lambda (it)
                           (bibtex-completion-clean-string
                            (bibtex-completion-get-value it entry "  ")))
                         fields))
           (fields (-update-at 1 'bibtex-completion-shorten-authors fields)))
      (s-format "$0 $1  $2  $3 $4 $5 $6" 'elt
                (-zip-with (lambda (f w) (truncate-string-to-width f w 0 ?\s))
                           fields (list 1 25 4 (- width 64) 18 6 1)))))

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
                  "${author} ${date}: ${title}. ${subtitle}. In: ${journaltitle}, ${volume}(${number}), ${pages}. [[zk:${=key=}][Zettel]]"
                  'bibtex-completion-apa-get-value entry))
                ("inproceedings"
                 (s-format
                  "${author} ${date}: ${title}. ${subtitle}. In: ${editor} (Hg.): [${crossref}] ${location}: ${publisher}, ${pages}. [[zk:${=key=}][Zettel]]"
                  'bibtex-completion-apa-get-value entry))
                ("book"
                 (s-format
                  "${author} ${date}: ${title}. ${subtitle}. ${location}: ${publisher}. [[zk:${=key=}][Zettel]]"
                  'bibtex-completion-apa-get-value entry))
                ("collection"
                 (s-format
                  "${editor} (Hg.) ${date}: ${title}. ${subtitle}. ${location}: ${publisher}. [[zk:${=key=}][Zettel]]"
                  'bibtex-completion-apa-get-value entry))
                ("mvcollection"
                 (s-format
                  "${editor} (Hg.) ${date}: ${title}. ${subtitle}. ${location}: ${publisher}. [[zk:${=key=}][Zettel]]"
                  'bibtex-completion-apa-get-value entry))
                ("phdthesis"
                 (s-format
                  "${author} ${year}: ${title}. ${subtitle}. (Doctoral dissertation). ${school}, ${location}. [[zk:${=key=}][Zettel]]"
                  'bibtex-completion-apa-get-value entry))
                ("inbook"
                 (s-format
                  "${author} ${date}: ${title}. ${subtitle}. In: [${crossref}] ${location}: ${publisher}, ${pages}. [[zk:${=key=}][Zettel]]"
                  'bibtex-completion-apa-get-value entry))
                ("incollection"
                 (s-format
                  "${author} ${date}: ${title}. ${subtitle}. In: ${editor} (Hg.): [${crossref}] ${location}: ${publisher}, ${pages}. [[zk:${=key=}][Zettel]]"
                  'bibtex-completion-apa-get-value entry))
                ("proceedings"
                 (s-format
                  "${editor} (Hg.) ${date}: ${title}. ${location}: ${publisher}. [[zk:${=key=}][Zettel]]"
                  'bibtex-completion-apa-get-value entry))
                ("unpublished"
                 (s-format
                  "${author} ${date}: ${title}. ${subtitle}. Unpublished manuscript. [[zk:${=key=}][Zettel]]"
                  'bibtex-completion-apa-get-value entry))
                ("online"
                 (s-format
                  "${author} ${date}: ${title}. ${subtitle}. , ${url}. [[zk:${=key=}][Zettel]]"
                  'bibtex-completion-apa-get-value entry))
                (_
                 (s-format
                  "${author} ${date}: ${title}. ${subtitle}. [[zk:${=key=}][Zettel]]"
                  'bibtex-completion-apa-get-value entry)))))
      (replace-regexp-in-string "\\([ .?!]\\)\\." "\\1" ref))) ; Avoid sequences of punctuation marks.


  ;; Eigene Aktion für Logs
  (defcustom bibtex-completion-logs-extension "--log.org"
    "The extension of the files containing notes.  This is only
used when `bibtex-completion-notes-path' is a directory (not a file)."
    :group 'bibtex-completion
    :type 'string)

  (defcustom bibtex-completion-logs-template-multiple-files
    "#+TITLE: Log: ${author} ${date}: ${title}\n#+DATE: [${timestamp}]\n\n* ${author} ${date}: ${title}\n:PROPERTIES:\n:CATEGORY: wiss\n:END:\n[[autocite:${=key=}]]\n[[file:~/Dropbox/db/zk/zettel/${=key=}.org][zettel]]\n"
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

  (defun job/ivy-bibtex-try-last-citation ()
    (interactive)
    (save-excursion
      (search-backward-regexp "autocite" nil t)
      (search-forward "{" nil t)
      (kill-ring-save (point) (search-forward-regexp "[a-z0-9-]*" nil t)))
    (ivy-bibtex)))

(use-package ivy-posframe
  :disabled
  :after (ivy posframe)
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper-isearch . nil)
          (flyspell-correct-ivy . ivy-posframe-display-at-point)
          (zettelkasten-zettel-add-descriptor . ivy-posframe-display-at-point)
          (t . ivy-posframe-display-at-frame-center)))

  (setq ivy-posframe-height-alist '((counsel-ag . 30)
                                    (t . 20)))

  (setq ivy-posframe-size-function 'ivy-posframe-get-size+)


  (defun ivy-posframe-get-size+ ()
    (if (eq ivy--display-function
            'ivy-posframe-display-at-point)
        (list
         :height ivy-posframe-height
         ;; FIXME: dynamically determine length: how to take completion
         ;; annotations into account?
         :width 1000
         :min-height 10
         :min-width 33)
      (list
       :height ivy-posframe-height
       :width (+ 2 (frame-width))
       :min-height (or ivy-posframe-min-height (+ ivy-height 1))
       :min-width (or ivy-posframe-min-width (round (* (frame-width) 0.62))))))

  (ivy-posframe-mode 1)

  (set-face-attribute 'ivy-posframe nil :background "#f3f3f3" :foreground "#333333")
  (set-face-attribute 'ivy-posframe-border nil :background "#000000"))

(use-package ivy-prescient
  ;; :disabled
  :after (counsel)
  :config
  (ivy-prescient-mode)
  (prescient-persist-mode))


(use-package ivy-rich
  :unless windows-p
  :init (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :disabled
  :straight (all-the-icons-ivy-rich :type git
                                    :host github
                                    :repo "seagle0128/all-the-icons-ivy-rich"
                                    )
  :init
  (all-the-icons-ivy-rich-mode 1))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :disabled
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))



;;; K
(use-package key-chord
  :disabled
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
  :disabled
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
  :disabled
  :defer t
  :init
  (setq langtool-language-tool-jar "~/programme/LanguageTool-3.1/languagetool-commandline.jar"))

(use-package latex-extra
  :unless windows-p
  :diminish latex-extra-mode
  :hook (TeX-mode . latex-extra-mode))

(use-package ledger-mode
  :mode "\\.ledger\\'"
  :config
  (if windows-p
      (progn
        (setq ledger-binary-path "c:/Users/jba054/src/ledger.exe")
        (setq ledger-reconcile-default-commodity "NOK")
        (setq ledger-reports
              '(("Assets vs. Liabilities" "%(binary) -f \"c:/Users/jba054/OneDrive - University of Bergen/archive/ledger/main.ledger\" --decimal-comma bal -X NOK --real -d \"l<=5\" (Assets or Liabilities or Equity:Transfer:BrkToFelles) and not Felleskonto")
                ("Account" "%(binary) -f \"c:/Users/jba054/OneDrive - University of Bergen/archive/ledger/main.ledger\" --decimal-comma -X NOK reg %(account)")
                ("Account monthly average" "%(binary) -f \"c:/Users/jba054/OneDrive - University of Bergen/archive/ledger/main.ledger\" --decimal-comma -X NOK reg %(account) -M --average")
                ("Income vs. Expenses/Transfer" "%(binary) -f \"c:/Users/jba054/OneDrive - University of Bergen/archive/ledger/sb1.ledger\" --decimal-comma -X NOK --real --invert bal Income Expenses Equity:Transfer:BrkToFelles")
                ("Payee" "%(binary) -f  \"c:/Users/jba054/OneDrive - University of Bergen/archive/ledger/main.ledger\" --decimal-comma -X NOK reg @%(payee)")
                ("Expenses vs. Transfer (shared)" "%(binary) -f \"~/OneDrive - University of Bergen/archive/ledger/sb1-felles.ledger\" --decimal-comma -X NOK --real --invert bal Expenses Equity:Transfer")
                ("Budget (shared)" "%(binary) -f \"~/OneDrive - University of Bergen/archive/ledger/sb1-felles.ledger\" --decimal-comma -X NOK bal Budget")
                ("Assets vs. Liabilities (shared)" "%(binary) -f \"c:/Users/jba054/OneDrive - University of Bergen/archive/ledger/sb1-felles.ledger\" --decimal-comma bal -X NOK --real -d \"l<=5\" (Assets or Liabilities)")))
        )
    (setq ledger-binary-path "/home/job/src/ledger/ledger")
    (setq ledger-reconcile-default-commodity "€")
    (setq ledger-schedule-file "/home/job/proj/ledger-data/schedule.ledger")
    (setq ledger-reports
          '(("Dashboard" "%(binary) python /home/job/proj/ledger-data/ledger-dashboard.py")
            ("Budget" "%(binary) -f /home/job/proj/ledger-data/main.ledger --decimal-comma -X € bal ^Budget:Funds ^Budget:Savings")
            ("Assets vs. Liabilities" "%(binary) -f /home/job/proj/ledger-data/main.ledger --decimal-comma  bal -X € --real -d \"l<=3\" Assets Liabilities ")
            ("Income vs. Expenses" "%(binary) -f /home/job/proj/ledger-data/main.ledger --decimal-comma bal -X € --real Income Expenses ")
            ("reg" "%(binary) -f /home/job/proj/ledger-data/main.ledger -p \"this year\" --decimal-comma -X € reg %(account)")
            ("payee" "%(binary) -f %(ledger-file) --decimal-comma -X € reg @%(payee)")
            ("account" "%(binary) -f %(ledger-file) --decimal-comma -X € reg %(account)")
            ("Budgeting" "%(binary) python /home/job/proj/ledger-data/ledger-budgeting.py")
            ("Tag: Location" "%(binary) -f /home/job/proj/ledger-data/main.ledger --decimal-comma bal -X € --limit 'tag(\"Location\")' '--account=tag(\"Location\")' --real -d \"l<=2\" Expenses Income"))))
  )

(use-package ledger-capture
  :straight (ledger-capture :type git
                            :host github
                            :repo "jobangen/soa2ledger")
  :config
  (defun job/soa2ledger ()
    (interactive)
    (let* ((account (completing-read "Account: " '("sb1-personal-csv" "sb1-felles-csv")))
           (file (read-file-name "File: " "~/Downloads/" nil t))
           (dryrun (if (yes-or-no-p "Dryrun?") "--dryrun" "")))
      (async-shell-command (format "cd c:/Users/jba054/src/soa2ledger/ && python soa2ledger.py --account=%s --import_file=%s %s" account file dryrun) "*soa2ledger*"))))

(use-package ledger-ext
  :straight (ledger-ext :type git
                            :host github
                            :repo "jobangen/ledger-ext")
  :bind (:map ledger-mode-map
              ("C-c C-o C-n" . ledger-ext-plot-wrapper)))


(use-package ledger-job
  :disabled
  :unless windows-p
  :straight (ledger-job :local-repo "~/.emacs.d/lisp/ledger-job")
  :after (ledger-mode)
  :bind (:map ledger-mode-map
              ("C-c C-o C-n" . ledger-job-gnuplot-wrapper)))

(use-package link-hint
  :after (avy)
  :bind (("C-c h" . link-hint-open-link)
         ("C-<" . link-hint-open-link)))

(use-package linkmarks
  :disabled
  :straight (linkmarks :type git
                       :host github
                       :repo "dustinlacewell/linkmarks")
  :config
  (setq linkmarks-file "~/Dropbox/db/zk/zettel/zettelkasten-index.org")

  (cl-defun job/linkmarks-select ()
    (interactive)
    (-let* ((targets (linkmarks--in-file))
            (choices (mapcar 'car targets))
            (choice (completing-read "Entry: " choices))
            ((_ link) (-first (lambda (i) (equal (car i) choice)) targets)))
      (org-open-link-from-string link)))

  (defun job/linkmarks-capture ()
    (interactive)
    (let ((org-capture-entry '("t" "Zk Index" entry (file linkmarks-file)
                               "* %?\n%a\nadded: %U\n\n" :kill-buffer t)))
      (linkmarks--setup)
      (org-capture))))

(use-package lispy
  :hook (emacs-lisp-mode . lispy-mode)
  :diminish lispy-mode)

(use-package lsp-mode
  :disabled
  :hook (((python-mode ;; pylsp
           js-mode)    ;; ts-ls
          . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-idle-delay 0.5)

  (setq lsp-pyls-plugins-flake8-enabled t)

  (lsp-register-custom-settings
   '(("pylsp.plugins.pyls_mypy.enabled" t t)
     ("pylsp.plugins.pyls_mypy.live_mode" nil t)
     ("pylsp.plugins.pyls_black.enabled" t t)
     ("pylsp.plugins.pyls_isort.enabled" t t)

     ;; disable the following: dublicates by flake8
     ("pylsp.plugins.pycodestyle.enabled" nil t)
     ("pylsp.plugins.mccabe.enabled" nil t)
     ("pylsp.plugins.pyflakes.enabled" nil t)
     ))
  )

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-delay 0.2)
  (setq lsp-ui-doc-use-childframe t)
  (setq lsp-ui-doc-header nil)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-max-width 100)
  (setq lsp-ui-doc-max-height 35)
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-doc-alignment 'frame)

  (setq lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-delay 0.05)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-show-symbol t)
  (setq lsp-ui-sideline-show-diagnostics t)

  (setq lsp-ui-doc-border (face-foreground 'default)))

(use-package lua-mode)

;;; M
(use-package magit
  :bind (("C-x g" . magit-status))
  :commands magit-list-repositories
  :init
  (if windows-p
      (setq magit-repository-directories '(("c:/Users/jba054/src" . 2)
                                           ("c:/Users/jba054/.emacs.d" . 1))))
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-diff-refine-hunk 'all))

(use-package magit-gitflow
  :unless windows-p
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package messages-are-flowing
  :unless windows-p
  :commands (messages-are-flowing-use-and-mark-hard-newlines)
  :after (message)
  :hook (message-mode . messages-are-flowing-use-and-mark-hard-newlines))

(use-package modalka
  :disabled
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

(unless windows-p
 (use-package mu4e
   :straight (mu4e :type git
                   :host github
                   :repo "djcb/mu"
                   :files ("mu4e/*.el"))
   :config
   (require 'smtpmail)
   (require 'org-mu4e)
   (setq org-mu4e-link-query-in-headers-mode nil)

   (setq user-full-name "Jan Ole Bangen")
   (setq mail-user-agent 'mu4e-user-agent)
   (setq mu4e-root-maildir "/home/job/.mail/")
   (setq mu4e-get-mail-command "mbsync -a")
   (setq mu4e-update-interval 1200)
   (setq mu4e-completing-read-function 'completing-read)
   (setq mu4e-use-fancy-chars nil)

   (setq mu4e-context-policy 'pick-first)
   (setq mu4e-compose-context-policy nil)
   (setq mu4e-compose-format-flowed t)
   ;; (add-hook 'mu4e-compose-mode-hook 'visual-clean)
   (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
   ;; (add-hook 'mu4e-compose-mode-hook (lambda () (use-hard-newlines 1)))
   (setq mu4e-confirm-quit nil)
   (setq mu4e-attachment-dir "~/tmp/2del")
   (setq mu4e-change-filenames-when-moving t)
   (setq mu4e-headers-include-related nil)
   (setq mu4e-view-show-addresses 't)
   (setq mu4e-view-use-gnus t)
   (add-hook 'mu4e-view-mode-hook 'visual-line-mode)
   (setq message-kill-buffer-on-exit t)
   (setq mu4e-headers-results-limit 100)
   (setq mu4e-view-show-images t
         mu4e-show-images t
         mu4e-view-image-max-width 800)
   (setq mu4e-user-mail-address-list '("jobangen@gmail.com"
                                       "jobangen@zedat.fu-berlin.de"))

   (setq send-mail-function 'smtpmail-send-it
         message-send-mail-function 'smtpmail-send-it)

   (setq mu4e-compose-complete-only-after (format-time-string
                                           "%Y-%m-%d"
                                           (time-subtract
                                            (current-time) (days-to-time 365))))

   (setq mu4e-index-cleanup nil)  ;; nil speeds up
   (setq mu4e-index-lazy-check t) ;; t speeds up

   (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
   (add-to-list 'mu4e-view-actions '("attachmentActions" . mu4e-view-attachment-action) t)

   (setq mu4e-contexts
         `(,(make-mu4e-context
             :name "gmail"
             :match-func
             (lambda (msg)
               (when msg
                 (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
             :vars '((mu4e-drafts-folder . "/gmail/drafts")
                     (mu4e-sent-folder . "/gmail/sent")
                     (mu4e-trash-folder . "/gmail/trash")
                     (mu4e-refile-folder . "/gmail/arch")
                     (mu4e-sent-messages-behavior . delete)
                     (user-mail-address . "jobangen@gmail.com")
                     (smtpmail-smtp-server . "smtp.gmail.com")
                     (smtpmail-smtp-service . 587)))
           ,(make-mu4e-context
             :name "zedat"
             :match-func
             (lambda (msg)
               (when msg
                 (string-prefix-p "/zedat" (mu4e-message-field msg :maildir))))
             :vars '((mu4e-drafts-folder . "/zedat/drafts")
                     (mu4e-sent-folder . "/zedat/sent")
                     (mu4e-trash-folder . "/zedat/Trash")
                     (mu4e-refile-folder . "/zedat/2021")
                     (mu4e-sent-messages-behavior . sent)
                     (user-mail-address . "jobangen@zedat.fu-berlin.de")
                     (smtpmail-smtp-server . "mail.zedat.fu-berlin.de")
                     (smtpmail-smtp-service . 587)))))

   (setq mu4e-bookmarks
         `(,(make-mu4e-bookmark
             :name "Inboxes"
             :query "maildir:/gmail/inbox OR maildir:/zedat/inbox"
             :key ?i)
           ,(make-mu4e-bookmark
             :name "Unread messages"
             :query "flag:unread AND NOT flag:trashed"
             :key ?u)
           ,(make-mu4e-bookmark
             :name "Flagged messages"
             :query "flag:flagged"
             :key ?f)
           ,(make-mu4e-bookmark
             :name "Today's messages"
             :query "date:today..now AND NOT flag:trashed"
             :key ?t)
           ,(make-mu4e-bookmark
             :name "Yesterday's messages"
             :query (lambda ()
                      (concat "NOT flag:trashed AND date:"
                              (format-time-string
                               "%Y%m%d"
                               (subtract-time (current-time) (days-to-time 1)))))
             :key ?y)
           ,(make-mu4e-bookmark
             :name "Last 7 days"
             :query "date:7d..now AND NOT flag:trashed"
             :key ?w)))

   ;; https://github.com/sje30/emacs/blob/master/mu4e-view-save-all-attachments.el
   (defvar bulk-saved-attachments-dir (expand-file-name "~/tmp/2del"))

   (defun cleanse-subject (sub)
     (replace-regexp-in-string
      "[^A-Z0-9]+"
      "-"
      (downcase sub)))

   (defun mu4e-view-save-all-attachments (&optional arg)
     "Save all MIME parts from current mu4e gnus view buffer."
     ;; Copied from mu4e-view-save-attachments
     (interactive "P")
     (cl-assert (and (eq major-mode 'mu4e-view-mode)
                     (derived-mode-p 'gnus-article-mode)))
     (let* ((msg (mu4e-message-at-point))
            (id (cleanse-subject (mu4e-message-field msg :subject)))
            (attachdir (concat bulk-saved-attachments-dir "/" id))
	    (parts (mu4e~view-gather-mime-parts))
            (handles '())
            (files '())
            dir)
       (mkdir attachdir t)
       (dolist (part parts)
         (let ((fname (or
		       (cdr (assoc 'filename (assoc "attachment" (cdr part))))
                       (seq-find #'stringp
                                 (mapcar (lambda (item) (cdr (assoc 'name item)))
                                         (seq-filter 'listp (cdr part)))))))
           (when fname
             (push `(,fname . ,(cdr part)) handles)
             (push fname files))))
       (if files
           (progn
             (setq dir
		   (if arg (read-directory-name "Save to directory: ")
		     attachdir))
             (cl-loop for (f . h) in handles
                      when (member f files)
                      do (mm-save-part-to-file h (expand-file-name f dir))))
         (mu4e-message "No attached files found"))))
   ))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ;; ("C-<" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :diminish multiple-cursors)

;;; O
(use-package olivetti
  :commands (olivetti-mode))

;;; P
(use-package plantuml-mode
  :init
  (if windows-p
      (setq org-plantuml-jar-path (expand-file-name "c:/Users/jba054/src/plantuml/plantuml-1.2022.4.jar")))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

  (add-to-list 'org-babel-default-header-args:plantuml
               '(:cmdline . "-charset utf-8")))

(use-package pdf-tools
  :unless windows-p
  :demand t
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
  (setq pdf-annot-activate-created-annotations nil) ;problem with org-pdftools
  (setq pdf-view-resize-factor 1.1)
  (add-hook 'pdf-isearch-minor-mode-hook (lambda () (ctrlf-local-mode -1)))

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

  (defvar job/pdfview-selected-pages '())

  (defun job/pdfview-select-page ()
    "Add current page to list of selected pages."
    (interactive)
    (add-to-list 'job/pdfview-selected-pages (pdf-view-current-page) t))

  (defun job/pdfview-extract-selected-pages (file)
    "Save selected pages to FILE."
    (interactive "FSave as: ")
    (setq job/pdfview-selected-pages (sort job/pdfview-selected-pages #'<))
    (start-process "pdfjam" "*pdfjam*"
                   "pdfjam"
                   (buffer-file-name)
                   (mapconcat #'number-to-string
                              job/pdfview-selected-pages
                              ",")
                   "-o"
                   (expand-file-name file)))

  (define-key pdf-view-mode-map "S" #'job/pdfview-select-page)


  (advice-add 'pdf-annot-edit-contents-commit :after 'job/save-buffer-no-args))

(use-package pet ;; python
  :disabled
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package pomodoro
  :disabled
  :defer t
  :straight (:type git
                   :host github
                   :repo "vderyagin/pomodoro.el"))

(use-package pos-tip
  :unless windows-p)                   ;for sdcv

(use-package posframe
  :disabled
  :defer t)

(use-package prettier-js ;; javascript
  :defer t
  :diminish prettier-js-mode
  :hook (((js2-mode rjsx-mode vue-mode) . prettier-js-mode))
  :config
  (setq prettier-js-args '("--parser vue")))

(use-package projectile
  :defer 2
  :diminish projectile-mode
  :config
  (progn
    (projectile-mode)
    (setq projectile-indexing-method 'alien)
    (setq projectile-completion-system 'ivy)
    (setq projectile-enable-caching t)
    (setq projectile-switch-project-action 'projectile-dired)))

(use-package py-autopep8
  :config
  (setq py-autopep8-options '("--max-line-length=79"))
  ;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  )

(use-package python
  ;; :bind (:map python-mode-map
  ;;             ("C-c C-c" . job/python-shell-send-buffer-dwim))
  ;; :hook (python-mode . eglot-ensure)
  :config
  (setq python-shell-interpreter "python")
  (setq python-shell-interpreter-args "-m IPython --simple-prompt -i")
  
  (use-package python-isort
    :config
    (setq python-isort-arguments '("--profile" "black" "--stdout" "--atomic" "-"))
    (add-hook 'python-mode-hook 'python-isort-on-save-mode))
  

  (defun job/python-shell-send-buffer-dwim ()
    (interactive)
    (when (get-buffer "*Python*")
      (kill-buffer "*Python*"))
    (run-python)
    (other-window 1)
    (python-shell-send-buffer)

    
    ;; (job/python-shell-send-buffer-dwim)
    ;; (if (and (get-buffer "*Python*") (get-buffer-process "*Python*"))
    ;;     (progn
    ;;       (switch-to-buffer-other-window "*Python*")
    ;;       (end-of-buffer)
    ;;       (other-window 1)
    ;;       (python-shell-send-buffer))
    ;;   (run-python)
    ;;   (other-window 1)
    ;;   (job/python-shell-send-buffer-dwim))
    ))

(use-package pyvenv ;; python
  :config
  ;; (setq pyvenv-workon "emacs")  ; Default venv
  (pyvenv-tracking-mode 1)

  (defun job/create-venv-here ()
    "Create a new Python virtual environment in VENV-DIR."
    (interactive)
    (shell-command "python -m venv .venv")
    )
  )

;;; R
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
            :foreground "red"
            :inherit 'error
            :box t)
  )

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
    (setq reftex-default-bibliography `(,job/bibliography-file))
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

(use-package restclient)

(use-package rjsx-mode ;; javascript
  :mode ("\\.js\\'"
         "\\.jsx\\'")
  :config
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-basic-offset 2
        js-indent-level 2)
  (setq-local flycheck-disabled-checkers (cl-union flycheck-disabled-checkers
                                                   '(javascript-jshint))) ; jshint doesn't work for JSX
  )


(use-package ruff-format
  :straight (ruff-format :type git
                         :host github
                         :repo "scop/emacs-ruff-format"))

;;; S
(use-package sdcv
  :disabled
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


(unless windows-p
  (use-package sensitive-mode
    :straight (sensitive-mode :local-repo "~/.emacs.d/lisp/sensitive-mode")
    :mode ("\\.gpg\\'" . sensitive-mode)
    :config
    (setq epg-gpg-program "gpg2")
    ;; fragt in emacs nach pw; braucht "allow-loopback-pinentry" in gpg-agent.conf
    (setq epa-pinentry-mode 'loopback)))

(use-package shell
  :bind (:map shell-mode-map
              ("C-r" . counsel-shell-history))
  :init
  (defun job/goto-shell ()
    (interactive)
    (if (string= (buffer-name) "*shell*")
        (shell (generate-new-buffer-name "*shell*"))
      (if (get-buffer "*shell*")
          (switch-to-buffer "*shell*")
        (shell))))

  (defun my-shell-mode-hook ()
    (setq comint-input-ring-file-name "~/.zsh_history") ;; or bash_history
    (comint-read-input-ring t))
  (add-hook 'shell-mode-hook 'my-shell-mode-hook))

(unless windows-p
 (use-package shell-interaction
   :defer 2
   :straight (shell-interaction :local-repo "~/.emacs.d/lisp/shell-interaction")
   :init
   (eval-after-load 'shell-interaction
     `(make-directory ,(concat user-emacs-directory "var/shell-interaction") t))
   ))

(use-package showtip
  :disabled)                   ; for sdcv

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

(use-package smart-shift
  :config
  (global-smart-shift-mode 1))

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
  (setq shell-pop-default-directory nil)
  (if windows-p
      (setq shell-pop-shell-type
            '("eshell" "*eshell*"
              (lambda nil (eshell shell-pop-term-shell))))
    (setq shell-pop-shell-type
          '("ansi-term" "*ansi-term*"
            (lambda nil (ansi-term shell-pop-term-shell)))))

  (when linux-p
    (setq shell-pop-term-shell "/usr/bin/zsh"))
  (setq shell-pop-window-size 45)
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(use-package swiper
  :bind ("C-S-s" . swiper-isearch))

;;; T
(use-package telephone-line
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (setq telephone-line-height 20
        telephone-line-evil-use-short-tag t)

  (telephone-line-mode 1))

(use-package tile
  :bind ("C-c w" . imalison:hydra-tile/body)
  :config
  (progn
    (defvar imalison:tall-tile-strategy (tile-split-n-tall 3))
    (defvar imalison:wide-tile-strategy tile-wide)
    (defvar imalison:master-tile-strategy (tile-argument-buffer-fetcher
                                           :layout tile-master-left))
    (require 'hydra)
    (defhydra imalison:hydra-tile
      nil
      "tile"
      ("t" (tile :strategy imalison:tall-tile-strategy))
      ("w" (tile :strategy imalison:wide-tile-strategy))
      ("m" (tile :strategy imalison:master-tile-strategy))
      ("s" tile-select)
      ("0" (tile :strategy tile-one))
      ("n" tile)
      ;; ("l" winner-undo)
      )
    (setq tile-cycler
          (tile-strategies :strategies
                           (list imalison:tall-tile-strategy
                                 imalison:master-tile-strategy
                                 imalison:wide-tile-strategy
                                 tile-one)))))

(use-package tramp
  :straight nil
  :init
  (setq tramp-default-method "ssh")
  (setq tramp-persistency-file-name (no-littering-expand-var-file-name "tramp-history.el")))

(use-package ts
  :straight (ts :type git
                :host github
                :repo "alphapapa/ts.el"))

;;; U
(use-package undo-tree
  :bind (("C-x u" . undo-tree-visualize))
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode 1)
  :config
  (progn
    (setq undo-tree-visualizer-timestamps nil)
    (setq undo-tree-visualizer-diff t)
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/var/undo")))
    ))

;;; V
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :init
  (volatile-highlights-mode t))

(use-package vue-mode
  :mode "\\.vue\\'")


;;; W
(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode))


(use-package writegood-mode
  :unless windows-p
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

(use-package wwg
  :unless windows-p
  :straight (wwg :type git
                 :host github
                 :repo "ag91/writer-word-goals"))

(use-package www-synonyms
  :unless windows-p
  :commands www-synonyms-insert-synonym
  :config
  (setq www-synonyms-key "gaGF6dLppnG6whJVPKFg")
  (setq www-synonyms-lang "de_DE"))

;;; Y
(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook 'highlight-indentation-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (progn
    (add-hook 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
    (yas-global-mode 1)
    (setq require-final-newline nil)))

;;; Z
(use-package zetteldeft
  :disabled
  :straight (zetteldeft :type git
                        :host github
                        :repo "EFLS/zetteldeft")
  :config
  (setq zd-tag-regex "[#@]+[A-Za-z0-9:>-]+")
  (setq zd-string-after-title
        (concat "\n#+date: [" (format-time-string "%Y-%m-%d-%H%M") "]

* Schlagwörter
tags:

* Inhalt

* Literatur

* Links & Files

* Data"))

  (defun job/zd-follow-loop ()
    (interactive)
    (goto-char (point-min))
    (ignore-errors
      (while t
        (zetteldeft-follow-link)))))

(use-package zettelkasten
  :straight (zettelkasten :type git
                          :host github
                          :repo "jobangen/zettelkasten"
                          :files ("*.el"))
  :commands zettelkasten-insert-link-at-point
  :bind (("C->" . zettelkasten-open-backlink)
         ;; :map org-mode-map
         ;; ("C-c C-w" . zettelkasten-org-refile-wrapper)
         )
  :init
  (defun zettelkasten-org-refile-wrapper (&optional arg)
    (interactive "P")
    (if (s-starts-with?
         zettelkasten-zettel-directory
         (buffer-file-name))
        (zettelkasten-refile arg)
      (org-refile arg)))

  (if windows-p
      (progn
        (setq zettelkasten-db-emacsql-lib 'emacsql-sqlite-builtin)
        (setq zettelkasten-main-directory "c:/Users/jba054/OneDrive - University of Bergen/archive/zettel/")
        (setq zettelkasten-zettel-directory "c:/Users/jba054/OneDrive - University of Bergen/archive/zettel/")
        (setq zettelkasten-inbox-file (concat zettelkasten-zettel-directory "2022-03-16-0946-inbox.org"))
        (setq zettelkasten-texts-directory "c:/Users/jba054/OneDrive - University of Bergen/archive/txt-docs/"))
    (setq zettelkasten-main-directory "~/Dropbox/db/zk/")
    (setq zettelkasten-zettel-directory "/home/job/Dropbox/db/zk/zettel/")
    (setq zettelkasten-texts-directory "~/archive/texts/"))

  (setq zettelkasten-temp-directory "~/.emacs.d/var/zettelkasten/")
  (setq zettelkasten-bibliography-file job/bibliography-file)
  (setq zettelkasten-db-update-method 'when-idle)
  (setq zettelkasten-org-agenda-integration t)
  (setq zettelkasten-collection-predicate "prov:wasMemberOf")
  (setq zettelkasten-filename-to-id-func 'job/zettelkasten-fname-to-id)

  (defun job/zettelkasten-fname-to-id (filename)
    (cond ((s-prefix? "txt" filename)
           (s-chop-prefix "txt/" filename))
          ((s-prefix? "jr" filename)
           (s-chop-prefix "jr/" filename))
          ((s-prefix? "rdf" filename)
           (s-chop-prefix "rdf/" filename))
          ((s-prefix? "eph" filename)
           (s-left 15 (s-chop-prefix "eph/" filename)))
          (t (s-left
              (length (format-time-string zettelkasten-file-id-format))
              filename))))

  (defun org-add-invalidated-property ()
    (when (or (string= (org-get-todo-state) "DONE")
              (string= (org-get-todo-state) "CANCELLED")
              (string= (org-get-todo-state) "DELEGATED"))
      (org-set-property
       "INVALIDATED"
       (concat
        (format-time-string "%Y-%m-%dT%H:%M:%S+")
        (job/current-timezone-offset-hours)))))

  (defun org-delete-invalidated-property ()
    (when (string= (org-get-todo-state) "TODO")
      (org-entry-delete (point) "INVALIDATED")))

  (add-hook 'org-after-todo-state-change-hook 'org-add-invalidated-property)
  (add-hook 'org-after-todo-state-change-hook 'org-delete-invalidated-property)

  (setq zettelkasten-classes
        '(("owl:Class")
          ("owl:Thing"
           ("time:ProperInterval"
            ("time:DateTimeInterval"))
           ;;
           ("prov:InstantaneousEvent"
            ("prov:Start"
             "prov:End"))
           ("prov:Role")
           ;; Event, Procedure, Relationship
           ("prov:Activity"
            ("zkt:Experience") ;; TODO
            ("zkt:Procedure"
             ("zkt:Project"
              ("zkt:PhD"))))
           ;;
           ("prov:Entity"
            ("zkt:LinguisticForm")
            ("skos:Concept")
            ("skos:Concept zkt:Zettel")
            ("prov:Bundle")
            ("prov:Collection")
            ("prov:Plan"
             ("zkt:Task")
             ("dct:LinguisticSystem")
             ("skos:Concept prov:Plan")
             ("zkt:Rezept")
             ("zktm:Vaccine"))
            ("zkt:RealObject"
             ("dct:Software")
             ("dct:BibliographicResource"
              ("zktb:ProperBibliographicResource"
               ("zktb:Article"
                ("zktb:Review"))
               ("zktb:Book")
               ("zktb:InBook")
               ("zktb:Collection")
               ("zktb:Lexikon")
               ("zktb:InCollection")
               ("zktb:Journal")
               ("zktb:Issue")
               ("zktb:Thesis")
               ("zktb:ClassicalText")
               ("zktb:Report"))
              ("zkt:BibliographicEphemera"
               ("zkt:DocumentPart"         ;; paragraph etc
                ("zkt:FormalDocumentPart") ;; with headline? Section, Chapter
                ("zkt:Quote") ;; cites other text, is part of document
                ("zkt:JobPosting")
                ("zkt:CoverLetter"))
               ("zkt:Draft")
               ("zkt:SlideShow")
               ("zkt:Excerpt")
               ("zkt:Letter")
               ("zkt:Contract")
               ("zkt:Mitschrift")
               ("zkt:Note")
               ("zkt:Zettel")))))
           ;;
           ("prov:Agent"
            ("foaf:Agent"
             ("foaf:Person")
             ("foaf:Organization")
             ("foaf:Group"))
            ("prov:Person"
             ("foaf:Person")
             ("prov:Person foaf:Person"))
            ("prov:SoftwareAgent")
            ("prov:Organization"
             ("foaf:Organization")))
           ;;
           ("prov:Location"
            ("zkt:RealObject prov:Location geo:Point"
             ("zkt:TrainStation")
             ("zkt:Airport")))
           ("time:DateTimeDescription")
           ("dct:Collection")
           ("dct:PhysicalResource")
           ("skos:ConceptScheme")
           ("skos:Collection")
           ("prov:Influence"
            ("prov:AgentInfluence"
             ("prov:Association"))
            ("zkt:SemanticRelation")
            ("prov:EntityInfluence"
             ("prov:Usage")
             ("zkt:Perception")))
           ("geo:Point"))))

  (setq zettelkasten-predicates
        '(nil ("rdf:type")
              (("prov:hadRole")
               ("prov:wasInfluencedBy"
                ;;  zettelkasten
                ("zkt:followedBy")      ; zettel-zettel
                ("zkt:follows")
                ("zkt:hasBranch")
                ("zkt:branchesOffFrom")
                ("zkt:crossreferences")
                ("zkt:crossreferencedBy")

                ("zkt:symbolizes")      ;word-concept
                ("zkt:wasSymbolizedBy")
                ("zkt:refersTo")        ;concept-thing
                ("zkt:wasReferedToBy")  ;thing-concept
                ("zkt:standsFor")       ;word-thing
                ("prov:wasAttributedTo" ;; entity to agent
                 ("zkt:hadAdressat")
                 ("zktb:wasAuthoredBy")
                 ("zktb:wasEditedBy")
                 ("zktb:introductionBy")
                 ("zktb:wasTranslatedBy")
                 ("zkt:wasAnnotatedBy")
                 ("zkt:wasCoinedBy"))

                ("prov:wasAssociatedWith" ;; activity with agent
                 ("zkt:hadParticipant"    ;; event -- agent
                  ("zkt:wasPerformedBy"   ;; part of, active
                   ("zkt:wasLedBy"))
                  ("zkt:wasPerformedWith" ;; part of, passive?
                   ("zktm:wasPerformedOn")))
                 ("zkt:hadNonParticipant"
                  ("zkt:wasOrganizedBy")
                  ("zkt:wasDirectedAt"))
                 ("zkt:hadActiveAssociate"
                  ("zkt:wasPerformedBy" ;; part of, active
                   ("zkt:wasLedBy"))
                  ("zkt:wasOrganizedBy"))
                 ("zkt:hadPassiveAssociate"
                  ("zkt:wasPerformedWith" ;; part of, passive?
                   ("zktm:wasPerformedOn"))
                  ("zkt:wasDirectedAt"))
                 ("zkt:hadResponsibleParty") ;; activity -- agent
                 ("zkt:applicant")
                 ("zkt:employer")
                 ("zkt:employee")
                 ("zkt:sentBy")
                 ("zkt:sentTo"))

                ("prov:wasDerivedFrom" ;;entity -- entity
                 ("prov:hadPrimarySource")
                 ("prov:wasQuotedFrom")
                 ("prov:wasRevisionOf"))
                ("prov:wasGeneratedBy") ;;entity by activity
                ("prov:wasInvalidatedBy")
                ("prov:used"
                 ("zkt:perceptionOf"))
                ("prov:actedOnBehalfOf")
                ("prov:wasInformedBy") ;;activity by activity
                ("prov:wasStartedBy")
                ("prov:wasEndedBy"))
               ("prov:alternateOf"
                ("prov:specializationOf" ;;entity -- entity
                 ))
               ("prov:hadMember")
               ("prov:wasMemberOf")
               ("prov:atLocation"
                )                       ;; ... at Location
               ("prov:generatedAtTime") ;; entity at instant
               ("prov:qualifiedInfluence"
                ("prov:qualifiedAssociation")
                ("prov:qualifiedUsage"
                 ("zkt:qualifiedPerception")))
               ("skos:semanticRelation"
                ("skos:related"
                 ("skos:relatedMatch"))
                ("skos:broaderTransitive"
                 ("skos:broader"
                  ("skos:broadMatch"))
                 ("zkt:isTypeOf")) ;;
                ("skos:narrowerTransitive"
                 ("skos:narrower"
                  ("skos:narrowMatch"))
                 ("zkt:hasType"))
                ("skos:mappingRelation"
                 ("skos:closeMatch"
                  ("skos:exactMatch")
                  ("skos:relatedMatch")
                  ("skos:broadMatch")
                  ("skos:narrowMatch")))))

              ;; SKOS
              (
               ("skos:subject")
               ("skos:isSubjectOf")
               ("skos:member")
               ("skos:memberOf")
               ("skos:inScheme")
               ("skos:definition"))
              ;; owl-time
              ("time:intervalStartedBy" "time:intervalStarts"
               "time:intervalFinishedBy" "time:intervalFinishes"
               "time:after" "time:before"
               "time:intervalMetBy" "time:intervalMeets"
               "time:intervalContains" "time:intervalDuring"
               "time:minutes"
               "time:hours"
               "time:days"
               "time:hasDateTimeDescription")
              (("dct:issued")
               ("dct:date")
               ("dct:hasPart")
               ("dct:isPartOf")
               ("dct:language"))
              "zkt:addressee" ;; addressee of entity, letter etc
              ;; descriptive meta data
              ("zkt:distanceKM")
              ("zkt:wasEvidencedBy") ;; Concept -- ...
              ("zkt:result")
              ("foaf:member") ("foaf:memberOf")


              ("zktb:wasDedicatedTo")

              ("zktm:atBodilyLocation")
              ("zkt:hadQualia")
              ("zkt:dosage")
              ("prov:entity")
              ("prov:agent")
              ))

  (setq zettelkasten-db-predicate-data
        '([nil "rdf:type" nil "owl:Class" "zkr:isInstanceOf"]
          ;; prov attributes
          [nil "prov:hadRole" "owl:Thing" "prov:Role" "zkp:wasFilledBy"]
          [nil "prov:agent" "owl:Thing" "prov:Agent" "zkp:wasAgentIn"]
          [nil "zkt:symbolized" "zkt:LinguisticForm" "skos:Concept" "zkt:wasSymbolizedBy"]
          [nil "zkt:wasSymbolizedBy" "skos:Concept" "zkt:LinguisticForm" "zkt:symbolized"]
          [nil "zkt:refersTo" "skos:Concept" "owl:Thing" "zkt:wasReferedToBy"]
          [nil "zkt:wasReferedToBy" "owl:Thing" "skos:Concept" "zkt:refersTo"]
          [nil "zkt:standsFor" "zkt:LinguisticForm" "owl:Thing" nil]
          [nil "prov:wasAttributedTo" "prov:Entity" "prov:Agent" "prov:contributed"]
          ;; Zettelkasten
          [nil "zkt:followedBy" "zkt:Zettel" "zkt:Zettel" "zkt:follows"]
          [nil "zkt:follows" "zkt:Zettel" "zkt:Zettel" "zkt:followedBy"]
          [nil "zkt:crossreferences" "zkt:Zettel" "zkt:Zettel" "zkt:crossreferencedBy"]
          [nil "zkt:crossreferencedBy" "zkt:Zettel" "zkt:Zettel" "zkt:crossreferences"]
          [nil "zkt:hasBranch" "zkt:Zettel" "zkt:Zettel" "zkt:branchesOffFrom"]
          [nil "zkt:branchesOffFrom" "zkt:Zettel" "zkt:Zettel" "zkt:hasBranch"]
          ;;
          [nil "zkt:hadAdressat" "prov:Entity" "prov:Agent" "zkt:wasAdressatOf"]
          [nil "zktb:wasAuthoredBy" "prov:Entity" "prov:Agent" "zktb:authored"]
          [nil "zktb:wasEditedBy" "prov:Entity" "prov:Agent" "zktb:edited"]
          [nil "zkt:wasCoinedBy" "prov:Entity" "prov:Agent" "zktb:coined"]
          ;; 
          [nil "prov:wasAssociatedWith" "prov:Activity" "prov:Agent" "prov:wasAssociateFor"]
          [nil "zkt:hadParticipant" "prov:Activity" "prov:Agent" "zkt:participatedIn"]
          [nil "zkt:hadNonParticipant" "prov:Activity" "prov:Agent" "zkt:participatednotin"]
          [nil "zkt:hadActiveAssociate" "prov:Activity" "prov:Agent" "zkt:wasActiveAssociateFor"]
          [nil "zkt:hadPassiveAssociate" "prov:Activity" "prov:Agent" "zkt:wasPassiveAssociateFor"]
          [nil "zkt:hadResponsibleParty" "prov:Activity" "prov:Agent" "zkt:responsibleFor"]
          ;; 
          [nil "zkt:wasPerformedBy" "prov:Activity" "prov:Agent" "zkt:performed"]
          [nil "zkt:wasPerformedWith" "prov:Activity" "prov:Agent" "zkt:hadP"]
          [nil "zkt:wasOrganizedBy" "prov:Activity" "prov:Agent" "zkt:organized"]
          [nil "zkt:wasDirectedAt" "prov:Activity" "prov:Agent" "zkt:wasTargetOf"]
          [nil "zkt:wasLedBy" "prov:Activity" "prov:Agent" "zkt:led" ]
          [nil "zkt:wasPerformedOn" "prov:Activity" "prov:Agent" "zkt:hadPerformedOn"]
          ;;
          [nil "prov:wasGeneratedBy" "prov:Entity" "prov:Activity" "prov:generated"]
          [nil "prov:used" "prov:Activity" "prov:Entity" "prov:wasUsedBy"]
          [nil "prov:wasInformedBy" "prov:Activity" "prov:Activity" "prov:informed"]
          ;;
          [nil "prov:wasDerivedFrom" "prov:Entity" "prov:Entity" "prov:hadDerivation"]
          [nil "prov:wasRevisionOf" "prov:Entity" "prov:Entity" "prov:hadRevision"]
          [nil "prov:hadPrimarySource" "prov:Entity" "prov:Entity" "prov:wasPrimarySourceOf"]
          ;;
          [nil "prov:atLocation" "owl:Thing" "prov:Location" "prov:locationOf"]
          [nil "prov:wasMemberOf" "prov:Entity" "prov:Collection" "prov:hadMember"]
          ;; Qualified
          [nil "prov:qualifiedInfluence" "owl:Thing" "prov:Influence" nil]
          [nil "prov:qualifiedAssociation" "prov:Activity" "prov:Association" nil]
          ;;
          [nil "skos:broaderTransitive" "skos:Concept" "skos:Concept" "skos:narrowerTransitive"]
          [nil "skos:broader" "skos:Concept" "skos:Concept" "skos:narrower"]
          [nil "skos:narrowerTransitive" "skos:Concept" "skos:Concept" "skos:broaderTransitive"]
          [nil "skos:narrower" "skos:Concept" "skos:Concept" "skos:broader"]
          [nil "skos:narrowMatch" "skos:Concept" "skos:Concept" "skos:broadMatch"]
          [nil "skos:broadMatch" "skos:Concept" "skos:Concept" "skos:narrowMatch"]
          [nil "skos:related" "skos:Concept" "skos:Concept" "skos:related"]
          [nil "skos:subject" "owl:Thing" "owl:Thing" "skos:isSubjectOf"]
          [nil "skos:isSubjectOf" "owl:Thing" "owl:Thing" "skos:subject"]
          ;;
          [nil "dct:issued" "prov:Entity" "time:DateTimeInterval" nil]
          [nil "dct:date" "owl:Thing" "time:DateTimeInterval" nil]
          [nil "dct:language" "owl:Thing" "dct:LinguisticSystem" nil]
          [nil "dct:isPartOf" "owl:Thing" "owl:Thing" "dct:hasPart"]
          [nil "dct:hasPart" "owl:Thing" "owl:Thing" "dct:isPartOf"]
          [nil "zkt:result" "owl:Thing" "owl:Thing" nil]
          [nil "zkt:dosage" "zkt:Event" "owl::Class" nil]
          ;; owl-time
          [nil "time:intervalStartedBy" "time:ProperInterval" "time:ProperInterval" "time:intervalStarts"]
          [nil "time:intervalStarts" "time:ProperInterval" "time:ProperInterval" "time:intervalStartedBy"]
          [nil "time:intervalFinishes" "time:ProperInterval" "time:ProperInterval" "time:intervalFinishedBy"]
          [nil "time:intervalFinishedBy" "time:ProperInterval" "time:ProperInterval" "time:intervalFinishes"]
          ;;
          [nil "foaf:memberOf" "foaf:Person" "foaf:Group" "foaf:member"]
          [nil "foaf:member" "foaf:Group" "foaf:Person" "foaf:memberOf"]
          ;;
          [nil "time:hasDateTimeDescription" "owl:Thing" "time:DateTimeDescription" nil]
          [nil "time:minutes" "prov:Activity" "df:value"  nil]
          [nil "time:hours" "prov:Activity" "df:value" nil]
          [nil "time:days" "prov:Activity" "df:value" nil]
          ;; 
          [nil "zkt:distanceKM" "prov:Activity" "df:value" nil]
          ))

  (defun zettelkasten-txt-query ()
    (interactive)
    (counsel-ag nil "~/.custom-temp/pdfs-extracted" nil))

  (defun job/open-at-point ()
    (interactive)
    (if (equal major-mode 'dired-mode)
        (dired-find-file))
    (if (equal major-mode 'org-mode)
        (org-open-at-point)))

  (zettelkasten-update-org-agenda-files)
  )

(use-package zettelkasten-ext
  :straight (zettelkasten-ext :type git
                              :host github
                              :repo "jobangen/zettelkasten-ext")
  :bind (("C-ä" . hydra-zettelkasten/body)
         ("C-æ" . hydra-zettelkasten/body))
)


;;; Hydras
(defhydra hydra-system (:color red)

  ("w" nmcli-show-short "nmcli-show-short" :column "Wlan")
  ("t" wlan-toggle "wlan-toggle")

  ("M" mount-lsblk "mount-lsblk" :column "Mount")
  ("m" mount-mount-device "mount-mount-device" :color blue)
  ("u" mount-unmount-device "mount-unmount-device" :color blue)

  ("v" vpn-zedat-shell "vpn-zedat" :color blue :column "Programs")
  ("e" elfeed "elfeed" :color blue)
  ("p" pass "pass" :color blue)

  ("n" neato-graph-bar "neato-graph-bar" :color blue :column "System")
  ("x" job/xrandr "xrandr")
  ("b" battery "battery")
  ("S" job/clean-kill-emacs-shutdown "Shutdown" :color blue)

  ("q" nil "Quit" :color blue))
(bind-key "<f1>" 'hydra-system/body)

;;;
;; (desktop-save-mode 1)
;; Erinnert die zuletzt geöffneten Dateien
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

(if (window-system)
    (progn
      (menu-bar-mode -1)
      (tooltip-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (set-fringe-mode '(1 . 1))))

(if windows-p
    (progn
      (set-face-attribute 'default nil :height 135)
      (add-to-list 'default-frame-alist '(font . "Consolas")) )
  (add-to-list 'default-frame-alist '(font . "Inconsolata-12")))

(defun job/set-face-attr-height (&optional height)
  "Set char height."
  (interactive)
  (set-face-attribute 'default nil :height (or height (string-to-number (read-string "Height: ")))))

(defun job/toggle-line-spacing ()
  "Toggle line spacing of current buffer between nil and 0.5."
  (interactive)
  (if line-spacing
      (setq line-spacing nil)
    (setq line-spacing 0.5))
  (redraw-frame (selected-frame)))


(defun job/set-uib ()
  (interactive)
  (setq split-width-threshold 160)
  (job/set-face-attr-height 145))


(unless windows-p
  (setq initial-buffer-choice
        (lambda ()
          (delete-other-windows)
          (org-journal-new-entry t)
          (split-window-right)
          (other-window 1)
          (org-agenda-list 7)
          (get-buffer "*Org Agenda*"))))
