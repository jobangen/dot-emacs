;;;,_dot-auctex
(require 'tex-site)

(setq TeX-auto-save t)
(setq TeX-electric-escape t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)


;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

(setq LaTeX-csquotes-open-quote "\\enquote{")
(setq LaTeX-csquotes-close-quote "}")
(setq LaTeX-paragraph-commands '("…")) ;; Befehle, bei 'fill' eigene paragraphen bilden

(setq LaTeX-babel-hyphen nil) ; Disable language-specific hyphen insertion.

(custom-set-variables
 '(TeX-fold-macro-spec-list
   '(("[f]" ("sidenote"))
     ("[fn]" ("footnote" "marginpar"))
     ("[ac]" ("autocite" "avolcite")) ;; new
     ("[c]" ("cite"))
     ("[l]" ("label"))
     ("[r]" ("ref" "pageref" "eqref"))
     ("[i]" ("index" "glossary"))
     ("[1]:||--" ("item"))
     ("…" ("dots"))
     ("(C)" ("copyright"))
     ("(R)" ("textregistered"))
     ("TM" ("texttrademark"))
     ("\"{1}\"" ("enquote"))
     ("'{1}'" ("enquote*"))
     ("\"{2}\"" ("blockcquote"))
     ("\"{2}\"" ("textcquote"))
     ("\"{1}\"" ("chapname"))
     ("'{1}'" ("uneigtl"))
     (1 ("part" "part*" "chapter" "chapter*"
         "section" "section*" "subsection" "subsection*"
         "subsubsection" "subsubsection*"
         "paragraph" "paragraph*" "subparagraph" "subparagraph*"
         "emph" "textit" "textsl" "textmd" "textrm"
         "textsf" "texttt" "textbf" "textsc" "textup")))))

;;SyncTeX
(setq TeX-source-correlate-mode 'synctex)

(add-hook 'TeX-mode-hook
          (lambda ()
            (TeX-add-symbols
             '("enquote")
             '("enquote*")
             '("textcquote" [ "pre-note" ] [ "post-note" ] TeX-arg-cite  [ "Punctuation" ] t ignore ignore)
             '("blockcquote" [ "pre-note" ] [ "post-note" ] TeX-arg-cite  [ "Punctuation" ] t ignore ignore)
             '("noindent" ignore)
             '("fxnote" "Note")
             '("fxwarning" "Note")
             '("fxerror" "Note")
             '("fxfatal" "Note")
             '("fxnote*" t "Note")
             '("fxwarning*" t "Note")
             '("fxerror*" t "Note")
             '("fxfatal*" t "Note")
             '("test" "string" ignore))))



;;(add-hook 'TeX-after-compilation-finished-functions
;;  #'TeX-revert-document-buffer)

(add-hook 'TeX-mode-hook
          (lambda ()
            (add-to-list
             'TeX-command-list
             '("Latexmk" "%`latexmk -pdf %t"
               TeX-run-TeX nil t))
            (setq TeX-save-query nil)
            (setq TeX-show-compilation t)))

(add-hook 'TeX-mode-hook
          (lambda ()
            (add-to-list
             'TeX-command-list
             '("Latex -se" "%`pdflatex --synctex=1 -shell-escape %t"
               TeX-run-TeX nil t))
            (setq TeX-save-query nil)
            (setq TeX-show-compilation t)))

(add-hook 'TeX-mode-hook
          (lambda ()
            (add-to-list
             'TeX-command-list
             '("XeLaTeX" "%`xelatex --synctex=1 %(mode)%' %t"
               TeX-run-TeX nil t))
            (setq TeX-save-query nil)
            (setq TeX-show-compilation t)))

;; http://mbork.pl/2015-10-31_Smart_comma_and_other_punctuation
(defun job/smart-self-insert-punctuation (count)
  "If COUNT=1 and the point is after a space, insert the relevant
      character before any spaces."
  (interactive "p")
  (if (and (= count 1)
           (eq (char-before) ?\s))
      (save-excursion
        (skip-chars-backward " ")
        (self-insert-command 1))
    (self-insert-command count)))

(eval-after-load 'tex
  '(define-key TeX-mode-map ","
     'job/smart-self-insert-punctuation))

;;;###autoload
(defun job/insert-enquote ()
  (interactive)
  (insert "\\enquote{} "))

;;;###autoload
(defun job/insert-enquote* ()
  (interactive)
  (insert "\\enquote*{} "))

;;;###autoload
(defun job/enquote-wrapper ()
  (interactive)
  (if (not (eq last-command 'job/enquote-wrapper))
      (job/insert-enquote)
    (setq this-command nil)
    (forward-char 2)
    (delete-backward-char 11)
    (call-interactively 'job/insert-enquote*))
  (backward-char 2))

(eval-after-load 'tex
  '(define-key TeX-mode-map "\""
     'job/enquote-wrapper))

;; https://florian.adamsky.it/2018/03/09/emacs-add-acronyms.html
;;;###autoload
(defun fa/add-latex-acronym (region-beg region-end)
  "This function reads the written out form of an acronym via the
minibuffer and adds it to the acronym list in a latex
document. Addtionally, it sorts all acronyms in the list."
  (interactive "r")
  (save-excursion
    (let ((acronym
           (if (region-active-p)
               (buffer-substring region-beg region-end)
             (read-from-minibuffer "Acronym: ")))
          (full-name (read-from-minibuffer "Full Name: ")))
      (beginning-of-buffer)
      (if (search-forward "\\begin{acronym}" nil t)
          (progn
            (deactivate-mark)
            (open-line 1)
            (forward-line 1)
            (insert (concat "  \\acro{" acronym "}{" full-name "}"))
            (beginning-of-line)
            (sort-lines nil (point) (search-forward "\\end{acronym}" nil nil)))
        (user-error "No acronym environment found")))))

(provide 'dot-auctex)
;;; dot-auctex.el ends here
