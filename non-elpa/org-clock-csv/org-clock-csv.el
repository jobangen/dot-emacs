;;; org-clock-csv.el --- Export `org-mode' clock entries to CSV format.

;; Copyright (C) 2016 Aaron Jacobs

;; Author: Aaron Jacobs <atheriel@gmail.com>
;; URL: https://github.com/atheriel/org-clock-csv
;; Package-Version: 20160906.1047
;; Keywords: org
;; Version: 1.0

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package makes use of the `org-element' API to extract clock
;; entries from org files and convert them into CSV format. It is
;; intended to facilitate clocked time analysis in external programs.

;; In interactive mode, calling `org-clock-csv' will open a buffer
;; with the parsed entries from the files in `org-agenda-files'. The
;; function can also be called from Lisp code with a file list
;; argument, and there is an `org-clock-csv-batch' version that will
;; output the CSV content to standard output (for use in batch mode).

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'org-element)

;;;; Configuration options:

(defgroup org-clock-csv nil
  "Export `org-mode' clock entries to CSV format."
  :group 'external)

(defcustom org-clock-csv-header "task;category;date;start;end;duration;effort;tags"
  "Header for the CSV output.

Be sure to keep this in sync with changes to
`org-clock-csv-row-fmt'."
  :group 'org-clock-csv)

(defcustom org-clock-csv-row-fmt #'org-clock-csv-default-row-fmt
  "Function to parse a plist of properties for each clock entry
and produce a comma-separated CSV row.

Be sure to keep this in sync with changes to
`org-clock-csv-header'.

See `org-clock-csv-default-row-fmt' for an example."
  :group 'org-clock-csv)

(defun org-clock-csv-default-row-fmt (plist)
  "Default row formatting function."
  (mapconcat #'identity
	     (list (plist-get plist ':task)
		   (plist-get plist ':category)
		   (plist-get plist ':date)
		   (plist-get plist ':start)
		   (plist-get plist ':end)
                   (plist-get plist ':duration)
		   (plist-get plist ':effort)
;;		   (plist-get plist ':ishabit)
		   (plist-get plist ':tags))
	     ";"))

;;;; Utility functions:

(defsubst org-clock-csv--pad (num)
  "Add a leading zero when NUM is less than 10."
  (if (> num 10) num (format "%02d" num)))

;;;; Internal API:

(defun org-clock-csv--find-category (element)
  "Find the category of a headline ELEMENT, optionally recursing
upwards until one is found.

Returns an empty string if no category is found."
  (let ((category (org-element-property :CATEGORY element))
	(current element)
	(curlvl  (org-element-property :level element)))
    ;; If the headline does not have a category, recurse upwards
    ;; through the parent headlines, checking if there is a category
    ;; property in any of them.
    (while (not category)
      (setq current (if (equal curlvl 1)
			(org-element-lineage current)
		      (org-element-lineage current '(headline)))
	    curlvl (- curlvl 1))
      (setq category (org-element-property :CATEGORY current))
      ;; If we get to the root of the org file with no category, just
      ;; set it to the empty string.
      ;;
      ;; TODO: File-level categories are stored not as properties, but
      ;; as keyword elements in the `org-data' structure. In order to
      ;; extract them, it will probaby require a call to
      ;; `org-element-map'. Since this could be an expensive operation
      ;; on an org file with no headline-level categories, but a
      ;; single file-level category, it would need to be cached.
      (unless (equal 'headline (org-element-type current))
	(setq category "")))
    category))


(defun org-clock-csv--parse-element (element)
  "Ingest clock ELEMENT and produces a plist of its relevant
properties."
  (when (and (equal (org-element-type element) 'clock)
	     ;; Only ingest closed, inactive clock elements.
	     (equal (org-element-property :status element) 'closed)
	     (equal (org-element-property
		     :type (org-element-property :value element))
		    'inactive-range))
    (let* ((timestamp (org-element-property :value element))
	   ;; Find the first headline that contains this clock element.
	   (parent-headline (org-element-lineage element '(headline)))
	   (task (org-element-property :raw-value parent-headline))
	   (effort (org-element-property :EFFORT parent-headline))
	   ;; TODO: Handle tag inheritance, respecting the value of
	   ;; `org-tags-exclude-from-inheritance'.
	   (tags (mapconcat #'identity
			    (org-element-property :tags parent-headline) ":"))
	   (ishabit (when (equal "habit" (org-element-property
					  :STYLE parent-headline))
		      "t"))
	   (category (org-clock-csv--find-category parent-headline))
           (date (format "%d-%s-%s"
			  (org-element-property :year-start timestamp)
			  (org-clock-csv--pad
			   (org-element-property :month-start timestamp))
			  (org-clock-csv--pad
			   (org-element-property :day-start timestamp))))
	   (start (format "%d-%s-%s %s:%s"
			  (org-element-property :year-start timestamp)
			  (org-clock-csv--pad
			   (org-element-property :month-start timestamp))
			  (org-clock-csv--pad
			   (org-element-property :day-start timestamp))
			  (org-clock-csv--pad
			   (org-element-property :hour-start timestamp))
			  (org-clock-csv--pad
			   (org-element-property :minute-start timestamp))))
	   (end (format "%d-%s-%s %s:%s"
			  (org-element-property :year-end timestamp)
			  (org-clock-csv--pad
			   (org-element-property :month-end timestamp))
			  (org-clock-csv--pad
			   (org-element-property :day-end timestamp))
			  (org-clock-csv--pad
			   (org-element-property :hour-end timestamp))
			  (org-clock-csv--pad
			   (org-element-property :minute-end timestamp))))
      (duration (org-element-property :duration element)))
      (list :task task
	    :category category
            :date date
	    :start start
	    :end end
            :duration duration
	    :effort effort
	    :ishabit ishabit
	    :tags tags))))


(defun org-clock-csv--get-entries (filelist &optional no-check)
  "Retrieves clock entries from files in FILELIST.

When NO-CHECK is non-nil, skip checking if all files exist."
  (when (not no-check)
    ;; For the sake of better debug messages, check whether all of the
    ;; files exists first.
    (mapc (lambda (file) (cl-assert (file-exists-p file))) filelist))
  (cl-loop for file in filelist append
	   (with-current-buffer (find-file file)
	     (org-element-map (org-element-parse-buffer) 'clock
	       #'org-clock-csv--parse-element nil nil))))

;;;; Public API:

;;;###autoload
(defun org-clock-csv (&optional infile)
  "Export clock entries from INFILE to CSV format.

When INFILE is a filename or list of filenames, export clock
entries from these files. Otherwise, use `org-agenda-files'.

See also `org-clock-csv-batch' for a function more appropriate
for use in batch mode."
  (interactive)
  ;; TODO: Handle an OUTFILE argument.
  (let* ((filelist (if (null infile) (org-agenda-files)
		     (if (listp infile) infile (list infile))))
	 (buffer (get-buffer-create "*clock-entries-csv*"))
	 (entries (org-clock-csv--get-entries filelist)))
    (message "entries found: %d" (length entries))
    (with-current-buffer buffer
      (goto-char 0)
      (erase-buffer)
      (insert org-clock-csv-header "\n")
      (mapc (lambda (entry)
	      (insert (concat (funcall org-clock-csv-row-fmt entry) "\n")))
	    entries))
    (switch-to-buffer buffer)
    (write-region (point-min)
                  (point-max)
                  "~/Dropbox/db/stats/clock-entries.csv" nil)))

;;;###autoload
(defun org-clock-csv-batch (&optional infile)
  "Export clock entries from INFILE in CSV format to standard output.

This function is identical in function to `org-clock-csv' except
that it directs output to `standard-output'. It is intended for
use in batch mode."
  (let* ((filelist (if (null infile) (org-agenda-files)
		     (if (listp infile) infile (list infile))))
	 (entries (org-clock-csv--get-entries filelist)))
    (princ (concat org-clock-csv-header "\n"))
    (mapc (lambda (entry)
	    (princ (concat (funcall org-clock-csv-row-fmt entry) "\n")))
	  entries)))

(provide 'org-clock-csv)

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-clock-csv.el ends here
