(deftheme presentation
  "")

(let ((class '((class color) (min-colors 89)))

)


 (custom-theme-set-faces
  'presentation

  `(org-default ((,class (:slant normal :weight normal :foreground "#333333" :background "#FFFFFF" :family "Liberation Sans"))))

  `(org-level-1 ((,class (:height 1.6 :slant normal :weight normal :family "Liberation Sans"))))
))

(provide-theme 'greylines)
