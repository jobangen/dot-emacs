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
     ("[fx]" ("fxwarning" "fxnote" "fxerror" "fxfatal"))
     ("[ac]" ("autocite" "avolcite")) ;; new
     ("[acs]" ("autocites")) ;; new
     ("[c]" ("cite"))
     ("[cs]" ("cites"))
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
     ("/{1}/" ("begriff"))
     ("/{1}/" ("lang"))
     (1 ("part" "part*" "chapter" "chapter*"
         "section" "section*" "subsection" "subsection*"
         "subsubsection" "subsubsection*"
         "paragraph" "paragraph*" "subparagraph" "subparagraph*"
         "emph" "textit" "textsl" "textmd" "textrm"
         "textsf" "texttt" "textbf" "textsc" "textup")))))

;;SyncTeX
(setq TeX-source-correlate-mode 'synctex)

;; aus fixme.el kopiert
(defun LaTeX-fixme-targeted-annotation-hack (optional)
  "Hook for targeted FiXme annotations.
This function inserts a first couple of braces and a second one,
potentially with the active region's contents within it. The point is left
inside the first couple of braces."
  (TeX-argument-insert "" nil)
  (TeX-parse-argument optional nil))

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
             '("mwg" t ignore)
             '("fxnote*" LaTeX-fixme-targeted-annotation-hack)
             '("fxwarning*" LaTeX-fixme-targeted-annotation-hack)
             '("fxerror*" LaTeX-fixme-targeted-annotation-hack)
             '("clnote" "Note")
             '("clwarning" "Note")
             '("clnote*" LaTeX-fixme-targeted-annotation-hack)
             '("clwarning*" LaTeX-fixme-targeted-annotation-hack)
             '("abrnote" "Note")
             '("abrwarning" "Note")
             '("abrnote*" LaTeX-fixme-targeted-annotation-hack)
             '("abrwarning*" LaTeX-fixme-targeted-annotation-hack))))



;;(add-hook 'TeX-after-compilation-finished-functions
;;  #'TeX-revert-document-buffer)

(add-hook 'TeX-mode-hook
          (lambda ()
            (add-to-list
             'TeX-command-list
             '("Latexmk-f-pvc" "%`latexmk -f -pvc -pdf %t"
               TeX-run-TeX nil t))
            (setq TeX-save-query nil)
            (setq TeX-show-compilation t)))

(add-hook 'TeX-mode-hook
          (lambda ()
            (add-to-list
             'TeX-command-list
             '("Latexmk-custom-sync" "%`latexmk -new-viewer- -file-line-error -interaction=nonstopmode -shell-escape --synctex=1 -pvc -pdf %t"
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
