;;;_,EXWM
(require 'exwm)

(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(1 "DP1" 1 "HDMI1"))
(exwm-randr-enable)

;;; Workspaces
(setq exwm-workspace-number 5)

(setq exwm-workspace-show-all-buffers t) ;; share exwm buffers in all workspaces, not just the workspace in which it was created in (the default behaviour)
(setq exwm-layout-show-all-buffers t)

(defun exwm-workspace-next ()
  (interactive)
  (let ((next-numb (mod (+ 1 exwm-workspace-current-index) exwm-workspace-number)))
    (exwm-workspace-switch next-numb)))

;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

;;; Key Bindings
;;; Functions
;; http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/
;;;###autoload
(defun job/kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

;;;###autoload
(defun job/firefox ()
  (interactive)
  (start-process-shell-command "firefox" nil "firefox"))

;;;###autoload
(defun job/qutebrowser ()
  (interactive)
  (start-process-shell-command "qutebrowser" nil "qutebrowser"))

;;;###autoload
(defun job/gnome-terminal ()
  (interactive)
  (start-process-shell-command
   "gnome-terminal" nil "gnome-terminal"))

(defun job/geeqie ()
  (interactive)
  (start-process-shell-command
   "geeqie" nil "geeqie"))

;;;###autoload
(defun job/slock ()
  (interactive)
  (start-process-shell-command "slock" nil "slock"))

;;;###autoload
(defun job/volume-mute ()
  (interactive)
  (shell-command-to-string "amixer -q -D pulse set Master mute")
  (shell-command "amixer -D pulse sget Master | grep -m1 -P -o '\[[0-9]+%.*'"))

;;;###autoload
(defun job/volume-raise ()
  (interactive)
  (shell-command-to-string "amixer -q -D pulse set Master 2%+ unmute")
  (shell-command "amixer -D pulse sget Master | grep -m1 -P -o '[\[0-9]+%.*'"))

;;;###autoload
(defun job/volume-lower ()
  (interactive)
  (shell-command-to-string "amixer -q -D pulse set Master 2%- unmute")
  (shell-command "amixer -D pulse sget Master | grep -m1 -P -o '\[[0-9]+%.*'"))

;;;###autoload
(defun job/brightness-increase ()
  (interactive)
  (shell-command-to-string "xbacklight -dec 10")
  (shell-command "xbacklight"))

;;;###autoload
(defun job/brightness-decrease ()
  (interactive)
  (shell-command-to-string "xbacklight -inc 10")
  (shell-command "xbacklight"))

;;;###autoload
(defun job/gnome-screenshot ()
  (interactive)
  (shell-command-to-string "gnome-screenshot"))

;; Sensibel bei der Formatierung. Lispy machts kaputt
(setq exwm-input-global-keys
      `(([?\s-c] . exwm-input-toggle-keyboard)
        ([?\s-f] . job/qutebrowser)
        ([?\s-g] . gnus)
        ([?\s-j] . exwm-workspace-next)
        ([?\s-k] . job/kill-current-buffer)
        ([?\s-m] . job/gnome-terminal)
        ([?\s-q] . job/geeqie)
        ([?\s-r] . exwm-reset)
        ([?\s-w] . exwm-workspace-switch)
        ([?\s-x] . counsel-linux-app)
        ([?\s-z] . job/slock)
        ([s-f11] . job/brightness-increase)
        ([s-f12] . job/brightness-decrease)
        ([s-tab] . other-window)
        ([?\s-+] . exwm-layout-enlarge-window-horizontally)
        ([?\s--] . exwm-layout-shrink-window-horizontally)
        ([print] . job/gnome-screenshot)
        ([XF86AudioMute] . job/volume-mute)
        ([XF86AudioRaiseVolume] . job/volume-raise)
        ([XF86AudioLowerVolume] . job/volume-lower)))

;; 's-N': Switch to certain workspace
(dotimes (i 10)
  (exwm-input-set-key (kbd (format "s-%d" i))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))

;;; Simulation Keys
(setq exwm-input-simulation-keys
 '(([?\C-b] . left)
   ([?\C-e] . (S-end))
   ([?\C-d] . delete)
   ([?\C-g] . escape)
   ([?\C-f] . right)
   ;; ([?\C-h] . backspace)
   ([?\C-k] . (S-end delete))
   ([?\C-n] . down)
   ([?\C-m] . return)
   ([?\C-p] . up)
   ([?\C-s] . ?\C-f)
   ([?\C-w] . ?\C-x)
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

(message "Enabling EXWM.")
(exwm-enable)

(provide 'dot-exwm)
;;; dot-exwm.el ends here

