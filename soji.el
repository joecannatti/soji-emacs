;;; soji.el --- Mindful Workday Tool

;; Copyright (C) 2017 Joe Cannatti
;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:
;;; SOJI
(require 'cl)
(require 'org)
(require 'org-habit)
(require 'org-pomodoro)

(defgroup soji-settings nil "Tweakable stuff for the Soji Mindful Workday Tool"
  :group 'extensions)

(defcustom soji-file "~/Dropbox/Notes/soji_note/soji.org"
  "File where soji stores notes"
  :type 'file
  :set (lambda (varname val)
         (set-default varname
                      (expand-file-name val)))
  :require 'soji
  :group 'soji-settings)

(defcustom soji-work-tag "StitchFix"
  "How to tag work day soji files. Usually your company name is best"
  :type 'string
  :require 'soji
  :group 'soji-settings)

(defcustom soji-dim-percentage 70 "How transparent to make the screen during a break"
  :type 'number
  :require 'soji
  :group 'soji-settings)

(defcustom soji-bright-percentage 100 "How transparent to make the screen during normal work"
  :type 'number
  :require 'soji
  :group 'soji-settings)

(defcustom soji-pomodoro-length 25
  "Length of Pomodoros"
  :type 'number
  :require 'soji
  :group 'soji-settings)

(defcustom soji-break-length 5
  "Length of Breaks
" :type 'number
:require 'soji
:group 'soji-settings)

(define-minor-mode soji-mode
  "Soji log editor mode"
  nil
  " Soji"
  '()
  :group 'soji-settings
  :lighter " Soji")

(defun jc/soji-break (min)
  (interactive "nMinutes: ")
  (let ((org-pomodoro-short-break-length min))
    (jc/soji-open)
    (org-pomodoro-start :short-break)))

(defun jc/soji-open ()
  (interactive)
  (let ((starting-buffer (current-buffer)))
    (if (org-clock-is-active)
        (org-clock-jump-to-current-clock)
      (find-file soji-file)
      (org-set-visibility-according-to-property)
      (goto-char (point-min)))
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (if (org-clock-is-active)
        (switch-to-buffer starting-buffer)
      (org-agenda nil "n")
        ;;;(spacemacs/toggle-current-window-dedication)
      )))

(defun jc/soji-work (length)
  (interactive "P")
  (let ((org-pomodoro-length (or length soji-pomodoro-length)))
    (org-pomodoro))
  (org-narrow-to-subtree)
  (delete-other-windows)
  (spacemacs/toggle-current-window-dedication)
  (split-window-horizontally)
  (other-window 1)
  (spacemacs/switch-to-scratch-buffer))

(defun jc/soji-agenda-work (length)
  (interactive "P")
  (org-agenda-switch-to)
  (jc/soji-work length))

(defun jc/org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion
                       (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE")
                 "habit")
        subtree-end
      nil)))

(defun jc/org-skip-subtree-if-not-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion
                       (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE")
                 "habit")
        nil
      subtree-end)))

(defun jc/soji-dim ()
  (interactive)
  (set-frame-parameter (selected-frame)
                       'alpha
                       soji-dim-percentage))

(defun jc/soji-end ()
  (interactive)
  (jc/soji-dim)
  (jc/soji-open))

(defun jc/soji-bright ()
  (interactive)
  (set-frame-parameter (selected-frame)
                       'alpha
                       soji-bright-percentage))

(defvar my/org-habit-show-graphs-everywhere t)

(defun my/org-agenda-mark-habits ()
  (when (and my/org-habit-show-graphs-everywhere
             (not (get-text-property (point)
                                     'org-series)))
    (let ((cursor (point)) item
          data)
      (while (setq cursor (next-single-property-change cursor 'org-marker))
        (setq item (get-text-property cursor 'org-marker))
        (when (and item
                   (org-is-habit-p item))
          (with-current-buffer (marker-buffer item)
            (setq data (org-habit-parse-todo item)))
          (put-text-property cursor
                             (next-single-property-change cursor 'org-marker)
                             'org-habit-p
                             data))))))

(advice-add #'org-agenda-finalize :before #'my/org-agenda-mark-habits)
(add-hook 'org-pomodoro-started-hook 'jc/soji-bright)
(add-hook 'org-pomodoro-finished-hook 'jc/soji-end)
(add-hook 'org-pomodoro-killed-hook 'jc/soji-dim)
(add-hook 'org-pomodoro-break-finished-hook 'jc/soji-open)
(provide 'soji)
;;; soji.el ends here
