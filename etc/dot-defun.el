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

(provide 'dot-defun)
;;; dot-defun.el ends here

