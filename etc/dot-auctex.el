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

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (TeX-add-symbols "enquote")
            (TeX-add-symbols "enquote*")
            (TeX-add-symbols "blockcquote")
            (TeX-add-symbols "textquote")
            (TeX-add-symbols "textcquote")))

;; upd pdf-buffer after comp: https://github.com/politza/pdf-tools
(add-hook 'TeX-after-compilation-finished-functions 
          #'TeX-revert-document-buffer)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-to-list
             'TeX-command-list
             '("Latexmk" "%`latexmk -pdf %t"
               TeX-run-TeX nil t))
            (setq TeX-save-query nil)
            (setq TeX-show-compilation t)))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-to-list
             'TeX-command-list
             '("Latex -se" "%`pdflatex --synctex=1 -shell-escape %t"
               TeX-run-TeX nil t))
            (setq TeX-save-query nil)
            (setq TeX-show-compilation t)))

(add-hook 'LaTeX-mode-hook
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


(provide 'dot-auctex)
;;; dot-auctex.el ends here
