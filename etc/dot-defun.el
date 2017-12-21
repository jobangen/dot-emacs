;;;_,dot-defun


;;;###autoload
(defun job/beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

;;;###autoload
(defun job/capitalize-last-word ()
  (interactive)
  (left-word)
  (capitalize-word 1))

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


(provide 'dot-defun)
;;; dot-defun.el ends here

