(setq user-mail-address "jobangen@gmail.com"
      user-full-name "Jan Ole Bangen")

(setq gnus-select-method '(nnnil "")
      gnus-secondary-select-methods 
	'((nnimap "gmail"
		(nnimap-stream shell)
		(nnimap-shell-program "/usr/lib/dovecot/imap -o mail_location=maildir:$HOME/Mail/gmail:LAYOUT=fs")
                (nnimap-inbox "INBOX")
                (nnimap-split-methods default))
          (nnimap "zedat"
		(nnimap-stream shell)
		(nnimap-shell-program "/usr/lib/dovecot/imap -o mail_location=maildir:$HOME/Mail/zedat:LAYOUT=fs"))
          (nnimap "zedatma"
		(nnimap-stream shell)
		(nnimap-shell-program "/usr/lib/dovecot/imap -o mail_location=maildir:$HOME/Mail/zedatma:LAYOUT=fs"))
          (nntp "news.gwene.org")))

(setq nnmail-split-methods
      '(("yggdrasill" "^[TC][oc]:.*yggdrasill@lists.Uni-Marburg.DE")
        ("fsi-religionsw" "^[TC][oc]:.*fsi-religionswissenschaft@lists.fu-berlin.de")
        ("hsozkult" "^From:.*hsk.mail@geschichte.hu-berlin.de")
        ("INBOX" "")))


;;        ("geschkult" "^To:.*alle@verteiler.geschkult.fu-berlin.de")




;; Sending Mail
;; Let Gnus change the "From:" line by looking at current group we are in.
;; X-Message-.. passt den Server an
(setq gnus-posting-styles
      '(("gmail" (address "jobangen@gmail.com")
         ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587"))
        ("zedat" (address "jobangen@zedat.fu-berlin.de")
         ("X-Message-SMTP-Method" "smtp mail.zedat.fu-berlin.de 587"))
        ("zedatma" (address "job@zedat.fu-berlin.de")
         ("X-Message-SMTP-Method" "smtp mail.zedat.fu-berlin.de 587"))
        )
      )

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it)

; Gesendete Mails werden bei gmail nicht archiviert; update, damit die methode immer .gnus entspricht
(setq gnus-update-message-archive-method  t)
(setq gnus-message-archive-group
      '(("gmail" nil)
        ("zedat" "nnimap+zedat:sent")
        ("zedatma" "nnimap+zedatma:sent")
        )
)

;;Mails, die über GCC in den sent ordner wandern, werden als gelesen markiert
(setq gnus-gcc-mark-as-read t)



;; Gnus-delay - send delayed mail with C-c C-j
(gnus-delay-initialize)

;;
(gnus-demon-add-handler 'gnus-group-get-new-news 5 nil)
(gnus-demon-init)



;; Header, reply
(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq message-citation-line-format "On  %d. %b %Y (%R), %f wrote:\n")

;; Wide reply; nil bedeutet, das nur mein name aus dem CC-Feld gelöscht wird.
(setq message-dont-reply-to-names "jobangen@zedat.fu-berlin.de")




;; Versteht auch die "internationalisierten" Versionen als Reply
(setq message-subject-re-regexp
      (concat
       "^[ \t]*"
         "\\("
           "\\("
             "[Aa][Nn][Tt][Ww]\\.?\\|"     ; antw
             "[Aa][Ww]\\|"                 ; aw
             "[Ff][Ww][Dd]?\\|"            ; fwd
             "[Oo][Dd][Pp]\\|"             ; odp
             "[Rr][Ee]\\|"                 ; re
             "[Rr][\311\351][Ff]\\.?\\|"   ; ref
             "[Ss][Vv]"                    ; sv
           "\\)"
           "\\(\\[[0-9]*\\]\\)"
           "*:[ \t]*"
         "\\)"
       "*[ \t]*"
       ))



;; Forwarding
(setq message-forward-ignored-headers "DKIM-Signature:\\|^Return-path:\\|^Received:\\|^Received-SPF:\\|^Delivered-To:\\|^Authentication-Results:\\|^Thread-.*:\\|^Message-ID:\\|^References:\\|^In-Reply-To:\\|^Accept-Language:\\|^Content-Language:\\|^X-.*:")




;;moving Mail
;;Gmail, Move to archive 
(define-key gnus-summary-mode-map "va" 
  (lambda () (interactive)
    (gnus-summary-put-mark-as-unread nil)
    (gnus-summary-move-article nil "nnimap+gmail:arch" nil)
    (gnus-summary-next-unread-article)))

(define-key gnus-summary-mode-map "vd"
  (lambda () (interactive)
    (gnus-summary-delete-article)
    (gnus-summary-next-article)))

(define-key gnus-summary-mode-map "vg"
  (lambda () (interactive)
    (gnus-summary-move-article nil "nnimap+zedat:geschkult" nil)
    (gnus-summary-next-unread-article)))


(define-key gnus-summary-mode-map "v4" 
  (lambda () (interactive)
        (gnus-summary-move-article nil "nnimap+zedat:2014" nil)))

(define-key gnus-summary-mode-map "v5" 
  (lambda () (interactive)
        (gnus-summary-move-article nil "nnimap+zedat:2015" nil)))

(define-key gnus-summary-mode-map "v6" 
  (lambda () (interactive)
        (gnus-summary-move-article nil "nnimap+zedat:2016" nil)))

(define-key gnus-summary-mode-map "vi" 
  (lambda () (interactive)
    (gnus-summary-put-mark-as-unread nil)
    (gnus-summary-move-article nil "nnimap+zedat:2017/17-irw" nil)
    (gnus-summary-next-unread-article)))

(define-key gnus-summary-mode-map "vv" 
  (lambda () (interactive)
    (gnus-summary-put-mark-as-unread nil)
    (gnus-summary-move-article nil "nnimap+zedat:2017" nil)
    (gnus-summary-next-unread-article)))



;; Gnus startet im topic-mode
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; In Group sent wird der Empfänger angezeigt und nicht ich als Absender
(setq gnus-ignored-from-addresses
      "jobangen@googlemail.com\\|jobangen@zedat.fu-berlin.de\\|j.o.bangen@web.de\\|job@zedat.fu-berlin.de")

(setq gnus-summary-sort-functions '(gnus-summary-sort-by-most-recent-date))
(setq gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
;;(setq gnus-summary-thread-gathering-function
;;      'gnus-gather-threads-by-references)


(setq notmuch-fcc-dirs nil); MÜsste das FCC ausschalten..

(setq nndraft-directory "~/Mail/drafts")

(setq-default
     gnus-summary-line-format "%4i %U%R %(%&user-date;  %-20,20f  %B%S%)\n"
     gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
     gnus-sum-thread-tree-false-root ""
     gnus-sum-thread-tree-indent " "
     gnus-sum-thread-tree-leaf-with-other "├► "
     gnus-sum-thread-tree-root ""
     gnus-sum-thread-tree-single-leaf "╰► "
     gnus-sum-thread-tree-vertical "│")



;;(gnus-add-configuration
;; '(summary
;;   (horizontal 1.0
;;               (vertical 29 (tree 1.0))
;;               (vertical 1.0 (summary 1.0 point)))))

(gnus-add-configuration  ; summary view
 '(summary
   (vertical 1.0
             (horizontal 1.0 (summary 1.0 point) ("*BBDB*" 0.18)))))


;;(gnus-add-configuration
;; '(article
;;   (horizontal 1.0
;;               (vertical 26 (tree 1.0))
;;               (vertical 1.0 (summary 0.35 point) (article 1.0)))))

(gnus-add-configuration  ; article view
 '(article
   (vertical 1.0
             (horizontal 1.0 (summary 1.0 point) ("*BBDB*" 0.18))
             (horizontal 0.75 (article 1.0)))))

(gnus-add-configuration  ; reply-yank view
 '(reply
   (vertical 1.0
             (horizontal 1.0 (summary 1.0 point) ("*BBDB*" 0.18))
             (horizontal 0.75 (article 1.0) (reply-yank 0.5)))))

(gnus-add-configuration  ; reply-yank view
 '(reply-yank
   (vertical 1.0
             (horizontal 1.0 (summary 1.0 point) ("*BBDB*" 0.18))
             (horizontal 0.75 (article 1.0) (reply-yank 0.5)))))


(setq gnus-group-line-format "%P%3y:%c%B\n")
(setq gnus-topic-line-format "%i%2{%n - %A%}%v\n")


(setq gnus-use-trees t
      gnus-generate-tree-function 'gnus-generate-horizontal-tree
      gnus-tree-minimize-window nil)


;; don't ask how many emails to download
;;(setq gnus-large-newsgroup 'nil)

;; tells gnus to get new mail and also display all old mail
(define-key gnus-summary-mode-map (kbd "C-c C-c")
  (lambda ()
    (interactive)
    (gnus-summary-rescan-group 'all)))


;; Scoring
(setq gnus-use-adaptive-scoring '(word line))
(setq gnus-adaptive-word-length-limit 5)
(setq gnus-adaptive-word-no-group-words t)
(setq gnus-score-expiry-days 30)

(setq gnus-score-interactive-default-score 100)

(setq gnus-default-adaptive-score-alist
      '((gnus-unread-mark)
        (gnus-read-mark (subject 1)(from 2))
        (gnus-replied-mark)
        (gnus-forwarded-mark)
        (gnus-ticked-mark (subject 0)(from 0))
        (gnus-dormant-mark (subject 0)(from 0))
        (gnus-del-mark (subject 0)(from 0))
        (gnus-killed-mark (subject -1)(from -2))
        (gnus-expirable-mark)
        (gnus-kill-file-mark)
        (gnus-ancient-mark)
        (gnus-low-score-mark)
        (gnus-catchup-mark)))

;;(setq gnus-decay-scores t)
;;(setq gnus-score-decay-constant 1) ;default = 3
;;(setq gnus-score-decay-scale 0.03) ;default = 0.05

;;BBDB-Einträge bekommen einen Bonus
(setq bbdb/gnus-score-default 100) 
(setq gnus-score-find-score-files-function
      '(gnus-score-find-bnews bbdb/gnus-score))




; offlineimap kann aus gnus heraus gestartet werden
(define-key gnus-group-mode-map (kbd "vo")
  '(lambda ()
     (interactive)
     (shell-command "offlineimap&" "*offlineimap*" nil)))




;; BBDB-Integration
(require 'bbdb)
(bbdb-initialize 'gnus 'message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)




;; Notmuch searches usw
(require 'notmuch)
(add-hook 'gnus-group-mode-hook 'my/notmuch-shortcut)
(require 'org-gnus)

(define-key notmuch-search-mode-map (kbd "/") 'notmuch-search-filter)

(defun my/notmuch-shortcut ()
  (define-key gnus-group-mode-map "/" 'notmuch-search)
  )

(defun notmuch-file-to-group (file)
  "Calculate the Gnus group name from the given file name."
  (let ((group (file-name-directory (directory-file-name (file-name-directory file)))))
    (setq group (replace-regexp-in-string ".*/Mail/" "nnimap+" group))
    (setq group (replace-regexp-in-string "zedat/" "zedat:" group))
    (setq group (replace-regexp-in-string "zedatma/" "zedatma:" group))
    (setq group (replace-regexp-in-string "gmail/" "gmail:" group))
    (setq group (replace-regexp-in-string "/$" "" group))
    (if (string-match ":$" group)
        (concat group "INBOX")
      (replace-regexp-in-string ":\\." ":" group))))

(defun notmuch-goto-message-in-gnus ()
  "Open a summary buffer containing the current notmuch
     article."
  (interactive)
  (unless (gnus-alive-p) (with-temp-buffer (gnus)))
  (let ((group (notmuch-file-to-group (notmuch-show-get-filename)))
     	(message-id
     	 (replace-regexp-in-string "\"" ""
                                   (replace-regexp-in-string "^id:" ""
                                                             (notmuch-show-get-message-id)))))
    (if (and group message-id)
     	(progn
     	  (gnus-summary-read-group group 1) ; have to show at least one old message
     	  (gnus-summary-refer-article message-id)) ; simpler than org-gnus method?
      (message "Couldn't get relevant infos for switching to Gnus."))))

     (define-key notmuch-show-mode-map (kbd "C-c C-c") 'notmuch-goto-message-in-gnus)
