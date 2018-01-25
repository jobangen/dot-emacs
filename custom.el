(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-fold-macro-spec-list
   (quote
    (("[f]"
      ("sidenote"))
     ("[fn]"
      ("footnote" "marginpar"))
     ("[ac]"
      ("autocite" "avolcite"))
     ("[c]"
      ("cite"))
     ("[l]"
      ("label"))
     ("[r]"
      ("ref" "pageref" "eqref"))
     ("[i]"
      ("index" "glossary"))
     ("[1]:||--"
      ("item"))
     ("…"
      ("dots"))
     ("(C)"
      ("copyright"))
     ("(R)"
      ("textregistered"))
     ("TM"
      ("texttrademark"))
     ("\"{1}\""
      ("enquote"))
     ("'{1}'"
      ("enquote*"))
     ("\"{2}\""
      ("blockcquote"))
     ("\"{2}\""
      ("textcquote"))
     ("\"{1}\""
      ("chapname"))
     ("'{1}'"
      ("uneigtl"))
     (1
      ("part" "part*" "chapter" "chapter*" "section" "section*" "subsection" "subsection*" "subsubsection" "subsubsection*" "paragraph" "paragraph*" "subparagraph" "subparagraph*" "emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt" "textbf" "textsc" "textup")))))
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/var/bmkp/current-bookmark.el")
 '(custom-safe-themes
   (quote
    ("5391f6fab7dcd2518abd9080f3d560c0b5a9a255f2234fe95eb7906ba80feff0" "0d2f6d7345f1436e0965811a8c5ec1f73cb29c11fb87adbdc06a5dd381866499" "9380c5827687bea7e33f3de9581634ff7a3650301c2bcb2b0985c003e788d16a" "5a848f952143e77d7f8390aae473d92244b5578068f891624980a009639555e0" "64efb275c70040303b369883cc2e7f5c39cec63c64a28b97af347a8a590916f7" "f9067db92e0b04c6ece11216818d65820066cba695cda297b9652507ede2004e" "bab23829cc09cc41137635f3c192c63bf6f8d1bb38fa38efc8dcf8a7584069c8" "ac2d1d08abd4f7f1e89579d2600296d2da2db672468fe923556dfaa196fe0715" "a41a3c9820e66ce2bba3f79d2abd70468498e39eaaac3bbfa1d25e4387cabbef" "bfcd9d3fd6db86a2c9c01ee9f733240e6da0311df6f0a1f3f96a4f3ba223b019" "82b683ec9becd4cdb97d08a6793e362553a12532ee32d4a2393490048f788b56" "9f68007d7cfe9cc162980f031ae1aa255a310a7c4c171c76c413b6ac5fcc80b6" "bca7db624fe4f1fc955e8fb9d2dadb28219716bbdf2c5d77e9b8dc61a026781d" "f23b0aba5d53cdbe3555de9787ba32c5bc9c97d3684e2f01f04bb9bafadcfc74" "1d276b7253b9a45bd14d4d8c25b292162c8e30d87d8b5f29c9af5bb3034bffea" "58ad5aa307995ef63e6ba706972ce077a746075fe86c6779395c9e6afde5a3d5" "c38d8e218839480595af7fd3b98265f9be898ddf1b93091f291d2d5e19b8e27a" "1b71a27736e51d36e3e05ce99ca690cc25cdb3afdf0b19c70ec03cb96edd8761" "91af0f6b07cdc517ee008b9f273cb6830cbbac37f002016ac68daed6e6a57576" "5fe2a1fff62e526ba1a3f62ee7b80567116f60ecce2c80181e43dbb02762d4bf" "16f35742ebc8c6a191d4963c95da9d8d5b8e6a7c6eddc6bbc33dfe3c08c91fc7" "98d473c26547f25d7989ba5945b108658244879e245da769f8619bfa0a0e1984" "3fb28786a39d8fa091a9d42bee01a0c1d4021b8a36d26de81463ad24f6be1832" "efdfa20f4c4374d6f8d61e9db29b13115b532159bb794e0c36515dca82c2f061" "4185c523a58178932582b0b31668d25b9425a216490f24b02bd24118f2fb9b95" "6735c39fa9d838647a04e7370207ee715a828985c77f0ca0c68dffdc6023a821" "00588211a5b3cdbfb74e926088572be7d1d27602eb7b39037a1a67487a638d26" "c9d34cd49dba26bb6239067a1fb43ff833edd985831f961511cf9b54b53a5e5f" "e563b954b7d592672d89d5f8030ec9998e1f91de8c03397f7d6032e487aae436" "3d8377f9314b95be7b57d96e2a02930a14e7fc505f37b4395db28061ebe75bec" "dd0998ebc24793e78766966fe4a89a1d531dcca49f736dbfc873c9088df6c072" "ab3afc1bf3bbd68ea8801a03001b23b6560096ece6b8d8a07a7d78b0e9a853b7" "9ce01f0620b31b7ad08e9e36767a82e8b805ab7ec5053b62aea37fe74db56fb2" "9a023dd19c8dcd868674fabbc3274ae7797f8a02104d73bb753fa053207b115b" "c891cc25872cab9600dd497694f367d681a6dda20bcde6a62c668e1cf9cc148c" "647734b7ee8daa0b4f817a8c57c3e726c8448673c1df45182fa7f9013195da9a" "fb3b8d94b34d42fa17181a9084d46afdef21208f452e3d3770a64613187726e2" "508f708de7c06653400a960fdaf22b0727c74319e7ca4b30d437b0cb6b763764" "0a632f86240a2dc68c2c1a648da12681835648042bdd78f93de764e0d138757e" "329ffb0770b301c9c79d37377fa8812d90bf2db2fe65159f5d669f5da0c65169" "e07db0bb60aa3b6fb2f4fe1a6db3fefc63309d3a627e844e279e851c70498008" "6180a4187e6b55b753d8af9802d15bc9480125c68ad86094ed2433f38f0a1dad" "389da734f1c52a5a7737169dc75feb78abcdd88fd9d6b970ab94ed6da84da3ba" "5243f5e0141d4db03759838f8b04bd8f24725a818f1e16d5e64576ed3b6d7f8c" "6b0af66423c179f05262917e3f334ddf95f4befab0f87ff0e97ca5ebade6be8b" "78fa43416c269e895801141cbae045bce93b2f4bfc6104bda532f91fca8325a6" "08689db061183ff1047a2fa85ff350e7bf5b86937ff4be81ceb40f2750ecc3b7" "292297a33ae0709b1bfc41ffc6571f11124b272c28c88f0eef85fc2014542881" "8144e12804174e03781ecbaade5aa721d277089123f7729811782d5face8e785" "9082990ca9938d17658cf178e231f9f75bd9b5cf51a82b126b4b60154604df0b" "6fafe0f3f0b5a42ae1f68aae79fddab0feb79756177a5b26d57a9ace81cb2a61" "ce72521455bfcbd5406029259063c7d7775c912ca34dff98e4e5990b91b720e1" "68db167117d71ca721d0c86407e83b241a3ca744fd11af6a71ebc5188cfcc434" "ea3f977c88da384abf83ac03a99e3ef193e11e134961443eb84448923e3b26ea" "d397d018baa369e1815a57c8e6cf637ff126c1d4c2edf1bfa47e02e0f2160120" "b35b5f259a5d05129cf43c8404c309c2659b2e4efb507d4e6472dfebe644b27d" "04b2073ae3813280d05d3e8794475c4324bb256051883f56c0e986bb964a9a40" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(org-agenda-files
   (quote
    ("~/Dropbox/db/org/pers.org" "~/Dropbox/db/zk/zettel-logs.txt" "~/Dropbox/db/org/goals.org" "/home/job/Dropbox/db/org/wiss.org" "~/Dropbox/db/org/memacs/git.org" "~/Dropbox/db/org/memacs/filenametimestamps.org" "~/Dropbox/db/org/memacs/photos.org" "~/Dropbox/db/contacts.org" "~/Dropbox/db/org/journal.org" "~/Dropbox/db/org/calender.org_archive" "~/Dropbox/db/org/calender.org" "~/Dropbox/diss/diss.org" "/home/job/Dropbox/db/org/inbox.org" "/home/job/Dropbox/db/org/antiq.org" "/home/job/Dropbox/db/org/irw.org" "/home/job/Dropbox/db/org/orgtest.org")))
 '(org-drill-optimal-factor-matrix
   (quote
    ((1
      (2.36 . 3.86)
      (2.1799999999999997 . 3.72)
      (2.5 . 4.0)
      (2.6 . 4.14)
      (1.96 . 3.58)))))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-collector)))
 '(package-selected-packages
   (quote
    (goldendict neato-graph-bar bibtex-utils pomodoro remem zettelkasten epa-file hippie-expand abbrev dired org-notmuch gnus-dired git-wip-mode sdcv-mode writegood-mode bibtex-completion org-indent org-collector org-checklist org-contacts ox-extra org-clock-csv counsel-notmuch helpful rainbow-delimiters yasnippet org-plus-contrib use-package ess-view ess-R-data-view smartparens org-ref diminish shell-pop htmlize ox-reveal exwm xelb pass gnuplot-mode dired-collapse ag dired-launch no-littering undo-tree smart-mode-line ivy markdown-mode link-hint ivy-pass lispy calfw-org calfw smex define-word chronos org-brain counsel-projectile org-clock-convenience www-synonyms which-key wgrep wc-mode volatile-highlights transcribe pomidor picpocket peep-dired pcache paperless org-projectile org-present org-pdfview org-gcal org-autolist offlineimap multiple-cursors messages-are-flowing magit ledger-mode latex-extra langtool keyfreq ivy-rich ivy-hydra ivy-bibtex interleave haskell-mode gscholar-bibtex google-translate git-timemachine german-holidays flyspell-correct-ivy expand-region ess engine-mode dired-subtree dired-filter deft csv-mode counsel char-menu bug-hunter bookmark+ bbdb-ext ace-window ace-jump-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button))))))
