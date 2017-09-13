;;; shell-interaction.el --- Collection of functions to interact with bash-shell -*- lexical-binding: t -*-

;; Copyright (C) 2017 Jan Ole Bangen.

;; Author: Jan Ole Bangen <jobangen@gmail.com>
;; URL:
;; Package-Version: 20170913.2010
;; Version: 0.1.2
;; Package-Requires:
;; Keywords: shell convenience

;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Personal collection of snippets and shell-commands
;;
;;; Code:
(defcustom shell-interaction-var-directory
  (concat user-emacs-directory "var/shell-interaction/")
  "Path for data-files"
  :group 'shell-interaction
  :type 'string)


;;;###autoload
(defun pdftk ()
  (interactive)
  (let ((operation
         (completing-read "Choose Operation: "
                          '(("1->1")
                            ("2->1")
                            ("1->n")) nil t nil)))
    (when (string-equal operation "1->1")
      (let ((input-file
             (read-file-name "Input:"))
            (arguments
             (read-string "Arguments: "))
            (output-file
             (read-file-name "Output:")))
        (shell-command
         (concat "pdftk " input-file " cat " arguments " output " output-file))))
    (when (string-equal operation "2->1")
      (let ((input-file-1
             (read-file-name "Input 1:"))
            (input-file-2
             (read-file-name "Input 2:"))
            (output-file
             (read-file-name "Output:")))
        (shell-command
         (concat "pdftk " input-file-1 " " input-file-2 " cat output " output-file))))
    (when (string-equal operation "1->n")
      (let ((input-file
             (read-file-name "Input:"))
            ;; (output-dir
            ;;  (read-file-name "Output:"))
            )
        (shell-command
         (concat "pdftk " input-file " burst"))))))


;;;###autoload
(defun xrandr-list ()
  (interactive)
  (with-output-to-temp-buffer "*xrandr*"
    (shell-command "xrandr"
                   "*xrandr*"
                   "*Messages*")))

(defun xrandr-reset ()
  (interactive)
  (shell-command
   (concat "xrandr --output eDP1 --mode 1366x768 &"
           "xrandr --output DP1 --off &"
           "xrandr --output HDMI1 --off &"
           "xrandr --output VIRTUAL1 --off")))

(defun xrandr-main-1024x768 ()
  (interactive)
  (sc "xrandr --output eDP1 --mode 1024x768"))

(defun xrandr-main-1366x768 ()
  (interactive)
  (sc "xrandr --output eDP1 --mode 1366x768"))

(defun xrandr-vga-1024x768-clone ()
  (interactive)
  (sc "xrandr --output DP1 --mode 1024x768"))

(defun xrandr-vga-1024x768-left ()
  (interactive)
  (sc "xrandr --output DP1 --mode 1024x768 --left-of eDP1"))

(defun xrandr-vga-1366x768-clone ()
  (interactive)
  (sc "xrandr --output DP1 --mode 1366x768"))

(defun xrandr-vga-1366x768-left ()
  (interactive)
  (sc "xrandr --output DP1 --mode 1366x768 --left-of eDP1"))

(defun xrandr-hdmi-1024x768-clone ()
  (interactive)
  (sc "xrandr --output HDMI1 --mode 1024x768"))

(defun xrandr-hdmi-1024x768-left ()
  (interactive)
  (sc "xrandr --output HDMI1 --mode 1024x768 --left-of eDP1"))

(defun xrandr-hdmi-1366x768-clone ()
  (interactive)
  (sc "xrandr --output HDMI1 --mode 1366x768"))

(defun xrandr-hdmi-1366x768-left ()
  (interactive)
  (sc "xrandr --output HDMI1 --mode 1366x768 --left-of eDP1"))

(defun xrandr-preset-topoi ()
  (interactive)
    (shell-command
     (concat "xrandr --output eDP1 --mode 1366x768 &"
             "xrandr --output HDMI1 --mode 1680x1050 --left-of eDP1 &"
             "xmodmap ~/.Xmodmap")))


;;;###autoload
(defun wlan-write-find-channels ()
  (interactive)
  (shell-command
   (concat "iwlist wlp3s0 scanning > "
           shell-interaction-var-directory
           "channels.txt"))
  (find-file (concat shell-interaction-var-directory "channels.txt")))

;;;###autoload
(defun wlan-toggle ()
  "Toggle WLAN with nmcli."
  (interactive)
  (if (string-equal (shell-command-to-string "nmcli radio wifi") "aktiviert\n")
      (progn
        (shell-command-to-string "nmcli radio wifi off")
        (message "WLAN deactivated"))
    (shell-command-to-string "nmcli radio wifi on")
    (message "WLAN activated")))


;;;###autoload
(defun vpn-zedat-shell ()
  (interactive)
  (load-library "~/.password-store/.data/mycredentials.el.gpg")
  (if (string-equal (shell-command-to-string "nmcli radio wifi") "aktiviert\n")
      (if (y-or-n-p "Deactivate WLAN?")
          (progn
            (shell-command-to-string "nmcli radio wifi off")
            (message "WLAN deactivated"))))
  (with-temp-buffer
    (cd "/sudo::/")
    (async-shell-command (concat " echo " job/credentials-fu-berlin-password
                                 " | openconnect -s /usr/share/vpnc-scripts/vpnc-script vpn.fu-berlin.de --user=jobangen --passwd-on-stdin"))))

(provide 'shell-interaction)

;;; shell-interaction.el ends here
