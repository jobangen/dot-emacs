;;gnus init und setup
(gnus-demon-init)

(setq user-mail-address "jobangen@gmail.com"
      user-full-name "Jan Ole Bangen")
(setq nndraft-directory "~/Mail/drafts")


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


;; get rid of message
(setq gnus-always-read-dribble-file t)


;; Sending Mail
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type  'starttls
      smtpmail-smtp-service 587
      smtpmail-auth-credentials "~/.authinfo.gpg"
      smtpmail-debug-info t
      )

(setq gnus-confirm-mail-reply-to-news t)
(setq gnus-confirm-treat-mail-like-news nil)


;; gnutls
(setq starttls-use-gnutls t)
(setq starttls-gnutls-program "gnutls-cli")
(setq starttls-extra-arguments nil)


;; Let Gnus change the "From:" line by looking at current group we are in.
;; X-Message-.. passt den Server an
(setq gnus-posting-styles
      '((""
         (address "jobangen@gmail.com")
         ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587"))
        ("zedat"
         (address "jobangen@zedat.fu-berlin.de")
         ("X-Message-SMTP-Method" "smtp mail.zedat.fu-berlin.de 587"))
        ("zedatma"
         (address "job@zedat.fu-berlin.de")
         ("X-Message-SMTP-Method" "smtp mail.zedat.fu-berlin.de 587"))
        )
      )


; Gesendete Mails werden bei gmail nicht archiviert; update, damit die methode immer .gnus entspricht
(setq gnus-update-message-archive-method  t)
(setq gnus-message-archive-group
      '(("gmail" nil)
        ("zedatma" "nnimap+zedatma:sent")
        ("zedat" "nnimap+zedat:sent")
        )
)

;;Mails, die über GCC in den sent ordner wandern, werden als gelesen markiert
(setq gnus-gcc-mark-as-read t)



;; Gnus-delay - send delayed mail with C-c C-j
(gnus-delay-initialize)



;; gnus group
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq gnus-group-line-format "%P%3y:%C%B\n")
(setq gnus-topic-line-format "%i%2{%n - %A%}%v\n")
(gnus-demon-add-handler 'gnus-group-get-new-news 5 nil)




;;;;;;;;; Summary ;;;;;;;;;
(setq gnus-summary-sort-functions '(gnus-summary-sort-by-most-recent-date))
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
(setq gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))

(setq-default gnus-summary-line-format "%U%R %(%&user-date;  %-20,20f  %B%S%)\n"
              gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
              gnus-sum-thread-tree-indent " "
              gnus-sum-thread-tree-root "● "
              gnus-sum-thread-tree-false-root "● "
              gnus-sum-thread-tree-vertical "│"
              gnus-sum-thread-tree-leaf-with-other "├► "
              gnus-sum-thread-tree-single-leaf "╰► "
              gnus-sum-thread-tree-single-indent "")

(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "Today, %H:%M")
        ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
        (604800 . "%A, %H:%M") ;;that's one week
        ((gnus-seconds-month) . "%d. %B, %H:%M")
        ;; ((gnus-seconds-year) . "%B %d %H:%M")
        (t . "%Y-%m-%d, %H:%M"))) ;;this one is used when no other does match


(setq gnus-summary-line-format
      (concat " "
              "%U%R"
              "  "
              "%*%~(max-right 25)~(pad-right 25)n"
              "  "
              "%B%~(max-right 85)~(pad-right 85)s"
              "  "
              "%-120=%&user-date;\n"))
 
;;moving Mail
;;Gmail, Move to archive 
(define-key gnus-summary-mode-map "va" 
  (lambda () (interactive)
    (gnus-summary-put-mark-as-unread nil)
    (gnus-summary-move-article nil "nnimap+gmail:arch" nil)
    (gnus-summary-next-unread-article)))

(define-key gnus-summary-mode-map "vt"
  (lambda () (interactive)
    (gnus-summary-move-article nil "nnimap+gmail:trash" nil)
    (gnus-summary-next-article)))

(define-key gnus-summary-mode-map "vd"
  (lambda () (interactive)
    (gnus-summary-move-article nil "nnimap+zedat:Trash" nil)
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








;; Header, reply
(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq message-citation-line-format "On  %d. %b %Y (%R), %f wrote:\n")

;; Wide reply; nil bedeutet, das nur mein name aus dem CC-Feld gelöscht wird.
(setq message-dont-reply-to-names "jobangen@zedat.fu-berlin.de")

;; In Group sent wird der Empfänger angezeigt und nicht ich als Absender
(setq gnus-ignored-from-addresses
      "jobangen@googlemail.com\\|jobangen@zedat.fu-berlin.de\\|j.o.bangen@web.de\\|job@zedat.fu-berlin.de")


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


;; gnus layout
(gnus-add-configuration
 '(summary
   (horizontal 1.0
               (group 0.14) (summary 1.0 point))))

(gnus-add-configuration
 '(article
   (horizontal 1.0
               (group 0.14) (vertical 1.0
                                      (summary 10 point)
                                      (article 1.0)))))
(gnus-add-configuration
 '(reply
   (horizontal 1.0
               (group 0.14) (vertical 1.0
                                      (summary 10)
                                      (horizontal 1.0 (article 1.0) (message 0.5 point))))))

(gnus-add-configuration
 '(reply-yank
   (horizontal 1.0
               (group 0.14) (vertical 1.0
                                      (summary 10)
                                      (horizontal 1.0 (article 1.0) (message 0.5 point))))))


;; don't ask how many emails to download
;;(setq gnus-large-newsgroup 'nil)

;; tells gnus to get new mail and also display all old mail
(define-key gnus-summary-mode-map (kbd "C-c C-c")
  (lambda ()
    (interactive)
    (gnus-summary-rescan-group 'all)))



; offlineimap kann aus gnus heraus gestartet werden
(define-key gnus-group-mode-map (kbd "vo")
  '(lambda ()
     (interactive)
     (shell-command "offlineimap&" "*offlineimap*" nil)))





;; Notmuch searches usw
(require 'notmuch)
(add-hook 'gnus-group-mode-hook 'my/notmuch-shortcut)
(require 'org-gnus)

(setq notmuch-fcc-dirs nil) ;;Schaltet das FCC aus 


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








