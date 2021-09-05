;;;_,dot-org

(require 'org)


(setq org-image-actual-width 600)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;;; aesthetics
(setq org-startup-folded nil)
(setq org-startup-indented t)
(setq org-ellipsis "⬎")
(setq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◇" "◇" "◇" "◇" "◇" "◇" "◇" "◇" "◇" "◇")))
;;  "◆" "᳃" "⚬" "∘" "∘" "∘" "∘" "∘" "∘" "▹"

;;; refile
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path 'file)
(setq org-refile-use-outline-path t)
(setq org-refile-allow-creating-parent-nodes (quote confirm))
(setq org-refile-targets
      '((("~/Dropbox/db/org/pers.org") :maxlevel . 3)
        (("~/Dropbox/db/org/wiss.org") :maxlevel . 6)
        (("~/Dropbox/db/org/irw.org") :maxlevel . 4)
        (("~/proj/diss/diss.org") :maxlevel . 4)
        (("~/Dropbox/db/org/antiq.org") :maxlevel . 2)
        (("~/Dropbox/db/contacts.org") :maxlevel . 2)
        (("~/Dropbox/db/org/goals.org") :maxlevel . 2)
        (("~/Dropbox/db/org/projects.org") :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))


;;; archive
;; https://fuco1.github.io/2017-04-20-Archive-subtrees-under-the-same-hierarchy-as-original-in-the-archive-files.html
;; (defadvice org-archive-subtree (around fix-hierarchy activate)
;;   (let* ((fix-archive-p (and (not current-prefix-arg)
;;                              (not (use-region-p))))
;;          (afile (org-extract-archive-file (org-get-local-archive-location)))
;;          (buffer (or (find-buffer-visiting afile) (find-file-noselect afile))))
;;     ad-do-it
;;     (when fix-archive-p
;;       (with-current-buffer buffer
;;         (goto-char (point-max))
;;         (while (org-up-heading-safe))
;;         (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
;;                (path (and olpath (split-string olpath "/")))
;;                (level 1)
;;                tree-text)
;;           (when olpath
;;             (org-mark-subtree)
;;             (setq tree-text (buffer-substring (region-beginning) (region-end)))
;;             (let (this-command) (org-cut-subtree))
;;             (goto-char (point-min))
;;             (save-restriction
;;               (widen)
;;               (-each path
;;                 (lambda (heading)
;;                   (if (re-search-forward
;;                        (rx-to-string
;;                         `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
;;                       (org-narrow-to-subtree)
;;                     (goto-char (point-max))
;;                     (unless (looking-at "^")
;;                       (insert "\n"))
;;                     (insert (make-string level ?*)
;;                             " "
;;                             heading
;;                             "\n"))
;;                   (cl-incf level)))
;;               (widen)
;;               (org-end-of-subtree t t)
;;               (org-paste-subtree level tree-text))))))))


;;; tags
(setq org-tags-exclude-from-inheritance
      '("project" "txt"
        "KBbacklog" "KBtodo" "KBstarted" "KBblocked" "KBreview"))

; Tags with fast selection keys
(setq org-tag-alist '((:startgroup)
                       ("arbeit"    . ?a)
                       ("pers"      . ?p)
                       ("wiss"      . ?w)(:endgroup)
                      (:startgroup)
                       ("@home"     . ?h)
                       ("@irw"      . ?i)
                       ("@mail"     . ?m)
                       ("@topoi"    . ?o)(:endgroup)
                      (:startgroup)
                       ("today"     . ?t)
                       ("someday"   . ?s)(:endgroup)
                      (:startgroup)
                      ("KBbacklog"  . ?1)
                      ("KBtodo"     . ?2)
                      ("KBstarted"  . ?3)
                      ("KBblocked"  . ?4)
                      ("KBreview"   . ?5)(:endgroup)
                      ("computer"   . ?c)
                      ("verwaltung" . ?v)
                      ("lehre")
                      ("lesen")
))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key t)

;;; Projects
(setq org-stuck-projects
           '("+project/-DONE" ("TODO" "NEXT" "STARTED") ("longterm")))


;;; org-agenda

;; (setq org-agenda-diary-file "journal.org")
(setq org-agenda-include-diary t)


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
  :straight org
  :load-path "~/.emacs.d/straight/repos/org/contrib/lisp"
  :config
  (setq org-contacts-files '("~/Dropbox/db/contacts.org"))
  (setq org-contacts-icon-use-gravatar nil)
  (setq org-contacts-birthday-format "%l (%y)"))

(use-package org-drill
  :disabled
  :config
  (setq org-drill-hide-item-headings-p t)
  (setq org-drill-maximum-items-per-session 50)
  (setq org-drill-maximum-duration 20)
  (setq org-drill-spaced-repetition-algorithm 'sm5)
  (setq org-drill-add-random-noise-to-intervals-p t)
  (setq org-drill-adjust-intervals-for-early-and-late-repetitions-p t))

(use-package org-el-cache
  :straight (org-el-cache :type git
                    :host github
                    :repo "l3kn/org-el-cache"))

(use-package org-gcal
  :straight (org-gcal :type git
                      :host github
                      :repo "kidd/org-gcal.el")
  :disabled
  :defer 2
  :config
  (setq org-gcal-auto-archive t)
  (setq org-gcal-down-days 365)
  (setq org-gcal-client-id "553301842275-clecdgmr7i8741e3ck5iltlgfk3qf79r.apps.googleusercontent.com")
  (setq org-gcal-client-secret "4zyEbm_F_BMuJsA7rZZmgFBm")
  (setq org-gcal-file-alist '(("jobangen@googlemail.com" . "~/Dropbox/db/org/calender.org"))))


(use-package org-indent
  :straight org
  :load-path "~/.emacs.d/straight/repos/org/contrib/lisp"
  :commands org-indent-mode
  :diminish org-indent-mode
  :init
  (setq org-indent-mode-turns-on-hiding-stars t))

(use-package org-journal
  :init
  (setq org-journal-file-type 'daily)
  (setq org-journal-dir "/home/job/Dropbox/db/zk/zettel/jr/")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-date-format "%Y-%m-%d, %A")
  (setq org-journal-enable-agenda-integration t)
  (setq org-journal-carryover-items "")
  (setq org-journal-date-prefix "* ")
  (setq org-journal-file-header "#+TITLE: jr: %Y-%m-%d, %A, W%W \n#+COLLECTION: journal\n#+DESCRIPTOR: @journal\n\n")
  (setq org-journal-time-prefix "** ")
  :config
  (set-face-attribute
   'org-journal-calendar-entry-face nil :foreground "#dd0000" :slant 'italic)
  (set-face-attribute
   'org-journal-calendar-scheduled-face nil :foreground "#c40000" :slant 'italic)
  )

(use-package org-listcruncher
  :defer 3
  :straight (org-listcruncher :type git
                              :host github
                              :repo "dfeich/org-listcruncher"))

(use-package org-noter
  :config
  (setq org-noter-notes-search-path '("~/Dropbox/db/zk/zettel"))
  (setq org-noter-arrow-delay 0.1)
  (setq org-noter-property-doc-file "NOTER_DOCUMENT")
  (setq org-noter-property-note-location "NOTER_PAGE")
  (setq org-noter-always-create-frame nil)
  (setq org-noter-kill-frame-at-session-end nil)
  (setq org-noter-doc-property-in-notes t)
  (add-hook 'org-noter-insert-heading-hook #'zettelkasten-heading-to-node)
  )

(use-package org-noter-pdftools
  ;; :disabled ;; funktioniert nicht, warten?
  :after org-noter
  :straight
  (org-noter-pdftools :type git
                      :host github
                      :repo "fuxialexander/org-pdftools")

  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package org-notmuch
  :disabled
  :straight org
  :load-path "~/.emacs.d/straight/repos/org/contrib/lisp")

(use-package org-pdftools
  ;; :hook (org-load . org-pdftools-setup-link)
  ;; :init
  ;; (add-hook 'org-store-link-functions 'org-pdftools-store-link)
  :config
  (with-eval-after-load 'org
    (org-pdftools-setup-link))
  (setq org-pdftools-link-prefix "pdf")
  (setq org-pdftools-use-freepointer-annot t) 
  (setq org-pdftools-use-isearch-link nil)

  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package org-recoll
  :straight (org-recoll :type git
                    :host github
                    :repo "alraban/org-recoll"))

(use-package org-ref
  :defer 2
  :init
  (bind-key "C-c )" 'org-autocite-complete-link org-mode-map)
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  :config
  (progn
    (setq org-ref-notes-function #'org-ref-notes-function-many-files)
    (setq org-ref-notes-directory (expand-file-name zettel-dir))
    (setq org-ref-default-bibliography '("~/Dropbox/db/biblio.bib"))
    (setq org-ref-pdf-directory (expand-file-name texte-dir))
    (setq orhc-bibtex-cache-file (no-littering-expand-var-file-name "org/ref/bibtex-cache.el"))
    (setq org-ref-default-citation-link "autocite")))

(use-package org-secretary
  :straight (org-secretary :local-repo "~/.emacs.d/lisp/org-secretary/"))



;;; org-babel
(setq org-babel-python-command "python3")
(org-babel-lob-ingest "/home/job/proj/2018-11-06 lilli-diss/org/variablen.org")
(org-babel-lob-ingest "/home/job/proj/2018-11-06 lilli-diss/org/forschungsfragen.org")
(setq org-babel-default-header-args:python '((:noweb . "yes")
                                             (:results . "output wrap")))

(use-package ob-async)


;;; org-export
(setq org-export-use-babel nil)

(use-package org-mind-map
  :init
  (require 'ox-org)
  ;; Uncomment the below if 'ensure-system-packages` is installed
  ;;:ensure-system-package (gvgen . graphviz)
  :config
  (setq org-mind-map-engine "dot")      ; Default. Directed Graph
  (setq org-mind-map-dot-output '("png"))
  (setq org-mind-map-include-text t)
  (setq org-mind-map-default-node-attribs '(("shape" . "plain")))
  (setq org-mind-map-default-graph-attribs '(("autosize" . "false")
                                             ("size" . "10,30")
                                             ("resolution" . "900")
                                             ("nodesep" . "0.75")
                                             ("ranksep" . "0.1")
                                             ("overlap" . "false")
                                             ("spline" . "true")
                                             ("rankdir" . "LR")))
  )

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
\\usepackage{booktabs}
\\usepackage{graphicx}\n
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
  (setq org-reveal-root "file:///home/job/src/reveal.js"))

(use-package ox-publish
  :straight
  :config
  (setq org-publish-use-timestamps-flag nil)
  (setq org-publish-project-alist
        '(
          ("innovati-notes"
           :base-directory "/home/job/proj/2018-11-06 lilli-diss/org/"
           :base-extension "org"
           :publishing-directory "/home/job/proj/2018-11-06 lilli-diss/html/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4     ; Just the default for this project.
           :auto-preamble t)

          ("innovati-static"
           :base-directory "/home/job/proj/2018-11-06 lilli-diss/org/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|eps\\|pdf\\|mp3\\|ogg\\|swf\\|csv\\|txt\\|sh\\|py"
           :publishing-directory "/home/job/proj/2018-11-06 lilli-diss/html/"
           :recursive t
           :publishing-function org-publish-attachment)
          ("innovati" :components ("innovati-notes" "innovati-static")))))





(provide 'dot-org)
;;; dot-org.el ends here
