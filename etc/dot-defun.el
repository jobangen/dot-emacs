;;;_,dot-defun

;;;###autoload
(defun job/update-zoom ()
  (interactive)
  (with-temp-buffer
    (let ((filename "/home/job/tmp/var/zoom_amd64.deb"))
      (url-copy-file "https://zoom.us/client/latest/zoom_amd64.deb" filename)
      (async-shell-command (concat
                            "cd /home/job/tmp/var/ && "
                            "sudo apt install ./zoom_amd64.deb && "
                            "rm -f zoom_amd64.deb")))))

;;;###autoload
(defun job/beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

;;;###autoload
(defun job/berlinantiquariat-billing ()
  (interactive)
  (let ((month
         (read-string "Monat: ")))
    (shell-command
     (concat "mkdir -p /home/job/tmp/tools/rechnung-ba/ && cat /home/job/templates/org/ba-rechnung-collector.org ~/Dropbox/db/journal/" month "* > /home/job/tmp/tools/rechnung-ba/" month ".org"))
    (find-file (concat "/home/job/tmp/tools/rechnung-ba/" month ".org"))
    (goto-char (point-min))
    (while (search-forward "year-month" nil t)
      (replace-match month t nil))
    (search-forward "#+BEGIN: propview" nil t)
    (org-ctrl-c-ctrl-c)
    (search-forward "#+TBLFM:" nil t)
    (org-ctrl-c-ctrl-c)
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
  (kill-dired-buffers)
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
  (add-hook 'kill-emacs-hook
            '(lambda () (shell-command "shutdown --poweroff now")) t)
  (kill-dired-buffers)
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

;;; dired

;;;###autoload
(defun kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

;;;###autoload
(defun job-dired-goto-my-dirs ()
  (interactive)
  (let ((destination
         (completing-read "Dir: "
                          '(("proj")
                            ("Dropbox")
                            ("emacs")
                            ("home")
                            ("archive")
                            ("archive: nav")
                            ("archive: current year")) nil t nil)))
    (when (string-equal destination "proj")
      (counsel-find-file "~/proj/"))
    (when (string-equal destination "archive")
      (counsel-find-file "~/archive/"))
    (when (string-equal destination "Dropbox")
      (counsel-find-file "~/Dropbox/"))
    (when (string-equal destination "emacs")
      (counsel-find-file "~/.emacs.d/"))
    (when (string-equal destination "home")
      (counsel-find-file "~/"))
    (when (string-equal destination "archive: nav")
      (counsel-find-file "/home/job/archive/.nav-date-description/"))
    (when (string-equal destination "archive: current year")
      (counsel-find-file "/home/job/archive/date-description/2021/"))))

;;;###autoload
(defun job-dired-move2archive (&optional arg file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      current-prefix-arg
      files)))
  (dired-do-shell-command "~/src/job-m2a-interactive-wrapper-with-gnome-terminal.sh" arg file-list))

;;;###autoload
(defun job-dired-move-files-to-refile-dir (&optional arg file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      current-prefix-arg
      files)))
  (dired-do-shell-command "~/src/job-move-files-to-refile.sh" arg file-list))

;;;###autoload
(defun job-dired-move-files-to-texts-dir (&optional arg file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      current-prefix-arg
      files)))
  (dired-do-shell-command "~/src/job-move-files-to-texts.sh" arg file-list))

;;;###autoload
(defun job-dired-copy-files-to-temp-dir (&optional arg file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      current-prefix-arg
      files)))
  (dired-do-shell-command "~/src/job-copy-files-to-temp.sh" arg file-list))


;;;###autoload
(defun job-dired-cp-mv-files-to-destinations (&optional arg file-list)
  (interactive)
  (let ((destination
         (completing-read "Choose action: "
                          '(("1. Move to archive")
                            ("2. Copy to temp")
                            ("3. Move to refile")
                            ("4. Move to texts")) nil t nil)))
    (when (string-equal destination "3. Move to refile")
      (call-interactively 'job-dired-move-files-to-refile-dir))
    (when (string-equal destination "1. Move to archive")
      (call-interactively 'job-dired-move2archive))
    (when (string-equal destination "2. Copy to temp")
      (call-interactively 'job-dired-copy-files-to-temp-dir))
    (when (string-equal destination "4. Move to texts")
      (call-interactively 'job-dired-move-files-to-texts-dir))))

;;;###autoload
(defun job-navigate-date-description ()
  (interactive)
  (counsel-find-file "~/archive/.nav-date-description/"))

;;; crontab
;;;###autoload
(defun crontab-e ()
    (interactive)
    (with-editor-async-shell-command "crontab -e"))

;;;###autoload
(defun job-check-logs-for-errors ()
  (interactive)
  (counsel-ag "error" "~/tmp/logs" nil))


;;;###autoload
(defun job/byte-compile-current-file ()
  (interactive)
  (byte-compile-file (buffer-file-name)))

;;;###autoload
(defun job/writing-mode ()
  (interactive)
  (text-scale-set 1)
  (setq line-spacing 5)
  (linum-mode 0)
  (writegood-mode)
  (olivetti-mode)
  (olivetti-set-width '70))

;;;###autoload
(defun job/current-timezone-offset-hours ()
  (concat
   (s-pad-left 2 "0" (int-to-string (/ (car (current-time-zone)) 3600)))
   ":00"))


(org-link-set-parameters "arch" :follow #'myarchive-open)

(defun myarchive-open (path)
  (let ((fname (concat date-description-dir path "*")))
    (find-file-other-window fname t)))

;; https://stackoverflow.com/questions/37038441/generate-a-random-5-letternumber-string-at-cursor-point-all-lower-case/37039205
(defun job/random-alnum ()
  (let* ((alnum "abcdefghijklmnopqrstuvwxyz0123456789")
         (i (% (abs (random)) (length alnum))))
    (substring alnum i (1+ i))))

(defun job/random-string (n)
  "Generate a slug of n random alphanumeric characters.
   Inefficient implementation; don't use for large n."
  (if (= 0 n)
      ""
    (concat (job/random-alnum) (job/random-string (1- n)))))

(defun job/process-file ()
  (interactive)
  (let* ((id-len 21)
         (filename (dired-get-filename))
         (fnamebase (file-name-base filename))
         (filetype (downcase (file-name-extension filename)))
         (dt-old nil)
         (id-old nil)
         (modtime (format-time-string "%Y-%m-%dT%H.%M.%S"
                                      (nth 5 (file-attributes filename))))
         (datetime-options (list modtime
                                 ;; (format-time-string
                                 ;;  "%Y-%m-%dT%H.%M.%S" (current-time))
                                 ))
         (tags (filetags-extract-filetags filename))) ;; todo, save old
    (recenter-top-bottom)
    (cond ((s-starts-with? "Direkt_Depot_8010659390" fnamebase)
           (let* ((datestring (s-right 8 fnamebase))
                  (year (s-left 4 datestring))
                  (month (substring datestring 4 6))
                  (day (s-right 2 datestring)))
             (message datestring)
             (push (format "%s-%s-%s" year month day)
                   datetime-options)
             (setq tags (append tags '("in" "diba"))))))
    (save-match-data
      (string-match (rx
                     (group
                      (repeat 4 (any "0-9")) "-"
                      (repeat 2 (any "0-9")) "-"
                      (repeat 2 (any "0-9"))
                      (zero-or-one
                       (and "T"
                            (repeat 2 (any "0-9")) "."
                            (repeat 2 (any "0-9")) "."
                            (repeat 2 (any "0-9")))))
                     (zero-or-one "-")
                     (zero-or-one
                      (group (or (repeat 1 (any "a-z" "0-9"))
                                 (repeat 5 (any "a-z" "0-9"))
                                 (repeat 10 (any "a-z" "0-9"))))
                      "--"))
                    fnamebase)
      (push (match-string 1 fnamebase) datetime-options)
      (setq dt-old (match-string 1 fnamebase)) ;; not elegant
      (setq id-old (match-string 2 fnamebase)))
    (dired-display-file)
    (other-window 1)
    (ignore-errors (pdf-view-fit-width-to-window))
    (previous-window-any-frame)
    (let* ((dt-new (if (equal filetype "jpg")
                       modtime
                     (completing-read "Datetime: " datetime-options)))
           (id-rand (if (and (equal dt-old dt-new)
                             (< 0 (length id-old))
                             (= 20 (+ (length dt-new) (length id-old))))
                        id-old
                      (job/random-string (- (- id-len 1) (length dt-new)))))
           (tags-new (mapcar (lambda (tag)
                               (concat "+" tag))
                             tags))
           (filename-proc (read-string
                           "File description: "
                           (downcase
                            (s-left
                             40
                             (car (split-string
                                   (s-chop-prefixes
                                    (list dt-old "--" "-" id-old "--")
                                    fnamebase)))))))
           (filename-new (format "%s-%s--%s.%s"
                                 dt-new
                                 id-rand
                                 filename-proc
                                 filetype)))
      ;; (message tags-new)
      (rename-file filename filename-new)
      (filetags-update-tags-write
       (concat (dired-current-directory) filename-new)
       tags-new)
      (revert-buffer)
      (recenter-top-bottom)
      (unless (or (equal filetype "jpg")
                  (equal filetype "jpeg"))
        (filetags-dired-update-tags)
        (recenter-top-bottom))
      (next-line)
       (job/process-file)
      ;; (job-dired-cp-mv-files-to-destinations)
      ;; (sleep-for 3)
       )))

;;;###autoload
(defun job/tags-loop ()
  (interactive)
  (dired-display-file)
  (filetags-dired-update-tags)
  (next-line)
  (job/tags-loop))



(defun job/process-pdf ()
  (interactive)
  (let* ((filename (thing-at-point 'filename))
         (datetime nil)
         (tags nil))
    (cond ((s-starts-with? "Direkt_Depot_8010659390" filename)
           (let* ((datestring (s-right 8 (file-name-base filename)))
                  (year (s-left 4 datestring))
                  (month (substring datestring 4 6))
                  (day (s-right 2 datestring)))
             (setq datetime (parse-time-string (format "%s-%s-%s" year month day)))
             (setq tags '("+in" "+dkb")))))
    (dired-display-file)
    (other-window 1)
    (pdf-view-fit-width-to-window)
    (other-window 1)
    (let* ((datestring (if datetime
                           (format "%s-%s-%s"
                                   (nth 5 date) ;; year
                                   (s-pad-left 2 "0" (int-to-string
                                                      (nth 4 date))) ;; month
                                   (s-pad-left 2 "0" (int-to-string
                                                      (nth 3 date)))) ;; day
                         (read-string "Datestring: ")))
           (rand-string (job/random-string 5))
           (filename-proc (read-string "File description: "
                                       (downcase (s-left 40 (s-chop-suffix ".pdf" filename)))))
           (filename-new (format "%s-%s--%s.pdf"
                                 datestring
                                 rand-string
                                 filename-proc)))
      (rename-file filename filename-new)
      (filetags-update-tags-write
       (concat (dired-current-directory) filename-new)
       tags)
      (revert-buffer)
      (filetags-dired-update-tags)
      (job-dired-cp-mv-files-to-destinations)
      ;; (sleep-for 3)
      ;; (job/process-pdf) ; geht nicht, weil er das gnome-terminal nicht abwartet
      )))


;;; pdf-tools
;; https://www.reddit.com/r/emacs/comments/9p2yyq/marking_and_splitting_pdfs_with_pdfstools/


;;; uib pyautomation
;;;###autoload
(defun pyautomation-update-wikidata ()
  (interactive)
  (let ((instance (completing-read "Instance:" '("termwikitest" "termwikiprod"))))
 (pyvenv-activate "c:/Users/jba054/src/terminologi-py-automation/.venv")
 (async-shell-command (format "cd c:/Users/jba054/src/terminologi-py-automation/termbot/scripts && python tp_update_wikidata.py --instance=%s" instance)
                        "*Update wikidata*")
   (switch-to-buffer-other-window "*Update wikidata*")
   (goto-char (point-max))
   (other-window 1)))

;;; writing
(defvar job/writing-file "")
(defvar job/writing-session-number 0)
(defvar job/writing-wordcounter-day 0)
(defvar job/writing-wordaim-day 1000)
(defvar job/writing-wordcounter-session 0)
(defvar job/writing-wordaim-session 250)

(defun job/writing-get-current-word-count ()
  "Get word count of current tex file."
  (string-to-number
   (car (split-string
         (tex-count-words (point-min) (point-max))))))

(defun job/writing-set-current-file ()
  "Set current buffer as writing file."
  (setq job/writing-file (buffer-file-name)))

;;;###autoload
(defun job/writing-new-day ()
  "Start new day: Set writing aim for today and reset counter to current count."
  (interactive)
  (let ((aim (read-number "Aim for today: " job/writing-wordaim-day)))
    (setq job/writing-session-number 0)
    (job/writing-set-current-file)
    (setq
     job/writing-wordcounter-day (job/writing-get-current-word-count))
    (setq job/writing-wordaim-day aim)))

;;;###autoload
(defun job/writing-new-session ()
  "Start new session.
Set writing aim for current session and reset counter to current
count."
  (interactive)
  (let ((aim (read-number "Aim for this session: " job/writing-wordaim-session)))
    (job/writing-set-current-file)
    (setq job/writing-session-number (+ 1 job/writing-session-number))
    (setq job/writing-wordcounter-session (job/writing-get-current-word-count))
    (setq job/writing-wordaim-session aim)))

;;;###autoload
(defun job/writing-finish-session ()
  "Finish current session by writing data to session note."
  (interactive)
  (find-file job/writing-file)
  (let ((current-count (- (job/writing-get-current-word-count)
                          job/writing-wordcounter-session))
        (session-dur (read-string "Session duration: ")))
    (job/writing-ensure-session-note)
    (outline-next-heading)
    (open-line 1)
    (insert (format
             "| %s | %s | %s | %s | %s%% |"
             job/writing-session-number
             session-dur
             current-count
             job/writing-wordaim-session
             (truncate (* 100 (/ (float current-count)
                                 job/writing-wordaim-session)))
             ))
    (org-table-align)
    ))

(defun job/writing-ensure-session-note ()
  "Ensure writing note for today."
  (zettelkasten-journal-daily-file)
  (goto-char (point-min))
  (unless (search-forward "** Writing Note" nil t)
    (progn
      (search-forward ":RDF_TYPE: time:DateTimeDescription")
      (outline-next-heading)
      (open-line 1)
      (insert (format-time-string "** Writing Note %Y-%m-%d"))
      (org-set-property "CUSTOM_ID" (format-time-string "%Y-%m-%dT%H%M%S.%1N"))
      (org-set-property "RDF_TYPE" "zkt:Note")
      (org-set-property "GENERATED_AT_TIME"
                        (concat (format-time-string "%Y-%m-%dT%H:%M:%S+")
                                (job/current-timezone-offset-hours)))
      (outline-next-heading)
      (open-line 1)
      (insert (format "- Daily goal: %s\n" job/writing-wordaim-day))
      (insert "| Nr. | Duration | S. Written | S. Goal | S. Perc. |\n")
      (insert "|-----+----------+------------+---------+----------|")
      )))

;;;###autoload
(defun job/writing-get-status ()
  "Write current writing status to minibuffer."
  (interactive)
  (when (equal job/writing-file (buffer-file-name))
    (let* ((day-progress (- (job/writing-get-current-word-count)
                            job/writing-wordcounter-day))
           (day-percentage (truncate (* 100 (/ (float day-progress)
                                               job/writing-wordaim-day))))
           (session-progress (- (job/writing-get-current-word-count)
                                job/writing-wordcounter-session))
           (session-percentage (truncate (* 100 (/ (float session-progress)
                                                   job/writing-wordaim-session)))))
      (message "[Writing] Day: %s%% (%s/%s), Session %s: %s%% (%s/%s)"
               day-percentage day-progress job/writing-wordaim-day
               job/writing-session-number
               session-percentage session-progress job/writing-wordaim-session))))

(run-with-idle-timer
 3 t #'job/writing-get-status)

(defun job/start-gitbash ()
  "Start gitbash in current folder."
  (interactive)
  (async-start-process "gitbash" "C:/Program Files/Git/git-bash.exe" nil))

(defun job/start-windows-terminal ()
  (interactive)
  (async-start-process
   "wt"
   "C:/Program Files/WindowsApps/Microsoft.WindowsTerminal_1.22.10731.0_x64__8wekyb3d8bbwe/wt.exe"
   nil
   "-d" (replace-regexp-in-string "/" "\\" (expand-file-name default-directory) t t)))

(defun job/create-new-dd-folder ()
  (interactive)
  (find-file (expand-file-name "~/OneDrive - University of Bergen/archive/date-description/2025"))
  (revert-buffer)
  (let* ((id-len 21)
         (date (org-read-date))
         (rand (job/random-string (- (- id-len 1) (length date))))
         (desc (read-string "Folder name: ")))
    (find-file (expand-file-name "~/OneDrive - University of Bergen/archive/date-description/2025"))
    (dired-create-directory (concat date "-" rand "--" desc)))
  (revert-buffer))

;;; DU

(provide 'dot-defun)
;;; dot-defun.el ends here
