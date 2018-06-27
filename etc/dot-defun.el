;;;_,dot-defun


;;;###autoload
(defun job/beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

;;;###autoload
(defun job/berlinantiquariat-bill-format ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward-regexp "\\\\textit" nil t)
    (replace-match "" t nil))
  (goto-char (point-min))
  (while (search-forward-regexp "{\\[" nil t)
    (replace-match "" t nil))
  (goto-char (point-min))
  (while (search-forward-regexp "\\]}" nil t)
    (replace-match "" t nil))
  (goto-char (point-min))
  (while (search-forward-regexp "\"" nil t)
    (replace-match "" t nil))
  (goto-char (point-min))
  (while (search-forward-regexp "€" nil t)
    (replace-match "\\\\EURdig" t nil)))

;; https://github.com/baron42bba/.emacs.d/blob/master/bba.org#copy-buffer-file-name-to-kill-ring
;;;###autoload
(defun job/buffer-file-name-to-kill-ring ()
  (interactive)
  (kill-new (buffer-file-name))
  (message (format "stored '%s' in kill-ring" buffer-file-name)))

;;;###autoload
(defun job/capitalize-last-word ()
  (interactive)
  (left-word)
  (capitalize-word 1))

;;;###autoload
(defun job/clean-kill-emacs ()
  (interactive)
  (save-some-buffers)
  (kill-matching-buffers "\.gpg$")
  (kill-matching-buffers "\.tex$")
  (kill-matching-buffers "\.pdf$")
  (kill-matching-buffers "\.dat$")
  (kill-matching-buffers "\.csv$")
  (kill-matching-buffers "\.synctex\.gz$")
  (kill-matching-buffers "\.sh$")
  (kill-matching-buffers "zettel-combined\.txt$")
  (kill-emacs))

;;;###autoload
(defun job/clean-kill-emacs-shutdown ()
  (interactive)
  (save-some-buffers)
  (add-hook 'kill-emacs-hook '(lambda () (shell-command "shutdown now")) t)
  (kill-matching-buffers "\.gpg$")
  (kill-matching-buffers "\.tex$")
  (kill-matching-buffers "\.pdf$")
  (kill-matching-buffers "\.dat$")
  (kill-matching-buffers "\.csv$")
  (kill-matching-buffers "\.synctex\.gz$")
  (kill-matching-buffers "\.sh$")
  (kill-matching-buffers "zettel-combined\.txt$")
  (kill-emacs))

;;;###autoload
(defun job/downcase-last-word ()
  (interactive)
  (left-word)
  (downcase-word 1))

;;;###autoload
(defun job/find-file-as-sudo ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (find-alternate-file (concat "/sudo::" file-name)))))

;;;###autoload
(defun job/insert-date (prefix)
  "Insert the current date ISO-format; With prefix-argument: insert current date ISO-format with time. With two prefix arguments, insert date."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "%Y-%m-%d-%H%M")
                 ((equal prefix '(16)) "%Y%m%d.%H%M")
                 ((equal prefix '(64)) "%d.%m.%Y")))
        (system-time-locale "de_DE"))
    (insert (format-time-string format))))

;;;###autoload
(defun job/kill-line ()
  "kill-line; If point at end of line, kill whole line."
  (interactive)
  (if (equal (point)
             (line-end-position))
      (kill-whole-line)
    (call-interactively 'kill-line)))

;; http://emacs.stackexchange.com/questions/28543/smartparens-strict-mode-c-w-kill-line-if-no-active-region/29927
;;;###autoload
(defun job/kill-word-or-region (&optional arg)
  "Kill active region or one word backward."
  (interactive "p")
  (if (use-region-p)
      (kill-region
       (region-beginning)
       (region-end))
    (backward-kill-word arg)))

;;;###autoload
(defun job/just-one-space-in-region (beg end)
  "replace all whitespace in the region with single spaces"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))

;; https://www.emacswiki.org/emacs/SortWords
;;;###autoload
(defun sort-words (reverse beg end)
 "Sort words in region alphabetically, in REVERSE if negative.
  Prefixed with negative \\[universal-argument], sorts in reverse.

  The variable `sort-fold-case' determines whether alphabetic case
  affects the sort order.

  See `sort-regexp-fields'."
    (interactive "*P\nr")
    (sort-regexp-fields reverse "\\w+" "\\&" beg end))

;;;###autoload
(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

;;;###autoload
(defun job/calc-restart-and-trail ()
  (interactive)
  (calc)
  (call-interactively 'calc-reset)
  (calc-trail-display t))

;;; Insert Credentials
;;;###autoload
(defun job/insert-credentials-postbank-giro-iban ()
  (interactive)
  (load-library "~/.password-store/.data/mycredentials.el.gpg")
  (insert job/credentials-postbank-giro-iban))

;;;###autoload
(defun job/insert-credentials-postbank-giro-bic ()
  (interactive)
  (load-library "~/.password-store/.data/mycredentials.el.gpg")
  (insert job/credentials-postbank-giro-bic))



;; http://emacsredux.com/blog/2013/07/25/increment-and-decrement-integer-at-point/
(require 'thingatpt)

(defun thing-at-point-goto-end-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip over optional sign
    (when (looking-at "[+-]")
      (forward-char 1))
    ;; Skip over digits
    (skip-chars-forward "[[:digit:]]")
    ;; Check for at least one digit
    (unless (looking-back "[[:digit:]]" 1)
      (error "No integer here"))))
(put 'integer 'beginning-op 'thing-at-point-goto-end-of-integer)

(defun thing-at-point-goto-beginning-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip backward over digits
    (skip-chars-backward "[[:digit:]]")
    ;; Check for digits and optional sign
    (unless (looking-at "[+-]?[[:digit:]]")
      (error "No integer here"))
    ;; Skip backward over optional sign
    (when (looking-back "[+-]" 1)
        (backward-char 1))))
(put 'integer 'beginning-op 'thing-at-point-goto-beginning-of-integer)

(defun thing-at-point-bounds-of-integer-at-point ()
  "Get boundaries of integer at point."
  (save-excursion
    (let (beg end)
      (thing-at-point-goto-beginning-of-integer)
      (setq beg (point))
      (thing-at-point-goto-end-of-integer)
      (setq end (point))
      (cons beg end))))
(put 'integer 'bounds-of-thing-at-point 'thing-at-point-bounds-of-integer-at-point)

(defun thing-at-point-integer-at-point ()
  "Get integer at point."
  (let ((bounds (bounds-of-thing-at-point 'integer)))
    (string-to-number (buffer-substring (car bounds) (cdr bounds)))))
(put 'integer 'thing-at-point 'thing-at-point-integer-at-point)

(defun increment-integer-at-point (&optional inc)
  "Increment integer at point by one.

With numeric prefix arg INC, increment the integer by INC amount."
  (interactive "p")
  (let ((inc (or inc 1))
        (n (thing-at-point 'integer))
        (bounds (bounds-of-thing-at-point 'integer)))
    (delete-region (car bounds) (cdr bounds))
    (insert (int-to-string (+ n inc)))))

(defun decrement-integer-at-point (&optional dec)
  "Decrement integer at point by one.

With numeric prefix arg DEC, decrement the integer by DEC amount."
  (interactive "p")
  (increment-integer-at-point (- (or dec 1))))


;;;###autoload
(defun job/org-reveal-handout ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "#+BEGIN_NOTES" nil t)
    (replace-match "#+BEGIN_COMMENT" t nil))
  (goto-char (point-min))
  (while (search-forward "#+END_NOTES" nil t)
    (replace-match "#+END_COMMENT" t nil))
  (goto-char (point-min))
  (while (search-forward "#+ATTR_REVEAL: :frag" nil t)
    (beginning-of-line)
    (kill-line)))

(provide 'dot-defun)
;;; dot-defun.el ends here

