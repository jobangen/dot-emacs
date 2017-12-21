;;;_,EXWM
(require 'exwm)

;;; Workspaces
(setq exwm-workspace-number 4)

(setq exwm-workspace-show-all-buffers t) ;; share exwm buffers in all workspaces, not just the workspace in which it was created in (the default behaviour)
(setq exwm-layout-show-all-buffers t)

;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

;;; Key Bindings
;; 's-r': Reset
(exwm-input-set-key (kbd "s-r") #'exwm-reset)
;; 's-w': Switch workspace
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
;; 's-N': Switch to certain workspace
(dotimes (i 10)
  (exwm-input-set-key (kbd (format "s-%d" i))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))

(exwm-input-set-key (kbd "s-<tab>") 'other-window)

(exwm-input-set-key (kbd "s-f")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command "firefox" nil "firefox")))
(exwm-input-set-key (kbd "s-m")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command
                       "gnome-terminal" nil "gnome-terminal")))
(exwm-input-set-key (kbd "s-z")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command "slock" nil "slock")))
(exwm-input-set-key (kbd "s-x") 'counsel-linux-app)

(exwm-input-set-key (kbd "s-+") 'exwm-layout-enlarge-window-horizontally)
(exwm-input-set-key (kbd "s--") 'exwm-layout-shrink-window-horizontally)

(exwm-input-set-key (kbd "<XF86AudioMute>")
                    (lambda ()
                      (interactive)
                      (shell-command-to-string
                       "amixer -q -D pulse set Master toggle")
                      (shell-command
                       "amixer -D pulse sget Master | grep -m1 -P -o '\[[0-9]+%.*'")))
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")
                    (lambda ()
                      (interactive)
                      (shell-command-to-string
                       "amixer -q -D pulse set Master 2%+ unmute")
                      (shell-command
                       "amixer -D pulse sget Master | grep -m1 -P -o '[\[0-9]+%.*'")))
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>")
                    (lambda ()
                      (interactive)
                      (shell-command-to-string
                       "amixer -q -D pulse set Master 2%- unmute")
                      (shell-command
                       "amixer -D pulse sget Master | grep -m1 -P -o '\[[0-9]+%.*'")))

(exwm-input-set-key (kbd "s-<f11>")
                    (lambda ()
                      (interactive)
                      (shell-command-to-string "xbacklight -dec 10")
                      (shell-command "xbacklight")))
(exwm-input-set-key (kbd "s-<f12>")
                    (lambda ()
                      (interactive)
                      (shell-command-to-string "xbacklight -inc 10")
                      (shell-command "xbacklight")))

(exwm-input-set-key (kbd "<pause>") 'keyboard-quit)
(exwm-input-set-key (kbd "<print>")
                    (lambda ()
                      (interactive)
                      (shell-command-to-string "gnome-screenshot")))

;;; Simulation Keys
(exwm-input-set-simulation-keys
 '(([?\C-b] . left)
   ([?\C-e] . (S-end))
   ([?\C-d] . delete)
   ([?\C-g] . escape)
   ([?\C-f] . right)
   ([?\C-h] . backspace)
   ([?\C-k] . (S-end delete))
   ([?\C-n] . down)
   ([?\C-m] . return)
   ([?\C-p] . up)
   ([?\C-s] . ?\C-f)
   ([?\C-w] . (C-backspace))
   ([?\C-y] . ?\C-v)
   ([?\M-w] . ?\C-c)
   ([?\M-<] . home)
   ([?\M->] . end)
   ))


;;; Bar Modes usw
(if (window-system)
    (progn
      (menu-bar-mode -1)
      (tooltip-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (set-fringe-mode '(1 . 1))))

(exwm-enable)

(provide 'dot-exwm)
;;; dot-exwm.el ends here
