;;;_,dot-org

(require 'org)


(setq org-image-actual-width 900)



;;; archive
;; https://fuco1.github.io/2017-04-20-Archive-subtrees-under-the-same-hierarchy-as-original-in-the-archive-files.html
(defadvice org-archive-subtree (around fix-hierarchy activate)
  (let* ((fix-archive-p (and (not current-prefix-arg)
                             (not (use-region-p))))
         (afile (org-extract-archive-file (org-get-local-archive-location)))
         (buffer (or (find-buffer-visiting afile) (find-file-noselect afile))))
    ad-do-it
    (when fix-archive-p
      (with-current-buffer buffer
        (goto-char (point-max))
        (while (org-up-heading-safe))
        (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
               (path (and olpath (split-string olpath "/")))
               (level 1)
               tree-text)
          (when olpath
            (org-mark-subtree)
            (setq tree-text (buffer-substring (region-beginning) (region-end)))
            (let (this-command) (org-cut-subtree))
            (goto-char (point-min))
            (save-restriction
              (widen)
              (-each path
                (lambda (heading)
                  (if (re-search-forward
                       (rx-to-string
                        `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
                      (org-narrow-to-subtree)
                    (goto-char (point-max))
                    (unless (looking-at "^")
                      (insert "\n"))
                    (insert (make-string level ?*)
                            " "
                            heading
                            "\n"))
                  (cl-incf level)))
              (widen)
              (org-end-of-subtree t t)
              (org-paste-subtree level tree-text))))))))

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

(use-package org-journal
  :init
  (setq org-journal-dir "~/Dropbox/db/journal")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-date-format "%Y-%m-%d, %A")
  (setq org-journal-enable-agenda-integration t)
  :config
  (set-face-attribute
   'org-journal-calendar-entry-face nil :foreground "#dd0000" :slant 'italic)
  (set-face-attribute
   'org-journal-calendar-scheduled-face nil :foreground "#c40000" :slant 'italic))

(use-package org-listcruncher
  :straight (org-listcruncher :type git
                              :host github
                              :repo "dfeich/org-listcruncher"))

(use-package org-noter
  :config
  (setq org-noter-property-doc-file "INTERLEAVE_PDF")
  (setq org-noter-property-note-location "INTERLEAVE_PAGE_NOTE"))

(use-package org-notmuch
  :defer 3
  :straight org
  :load-path "~/.emacs.d/straight/repos/org/contrib/lisp")

(use-package org-pdfview
  :after (pdf-tools))

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
    (require 'org-ref)
    (setq org-ref-notes-directory (expand-file-name zettel-dir))
    (setq org-ref-default-bibliography '("~/Dropbox/db/biblio.bib"))
    (setq org-ref-pdf-directory (expand-file-name texte-dir))
    (setq orhc-bibtex-cache-file (no-littering-expand-var-file-name "org/ref/bibtex-cache.el"))
    (setq org-ref-default-citation-link "autocite")))


;; org-babel
(setq org-babel-python-command "python3")
(org-babel-lob-ingest "/home/job/proj/2018-11-06 lilli-diss/org/variablen.org")

(use-package ob-async)


;; org-export
(setq org-export-use-babel nil)

(use-package org-mind-map
  :init
  (require 'ox-org)
  ;; Uncomment the below if 'ensure-system-packages` is installed
  ;;:ensure-system-package (gvgen . graphviz)
  :config
  (setq org-mind-map-engine "dot")      ; Default. Directed Graph
  (setq org-mind-map-dot-output '("dot")))

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
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|csv\\|txt\\|sh\\|py"
           :publishing-directory "/home/job/proj/2018-11-06 lilli-diss/html/"
           :recursive t
           :publishing-function org-publish-attachment)
          ("innovati" :components ("innovati-notes" "innovati-static")))))

(provide 'dot-org)
;;; dot-org.el ends here
