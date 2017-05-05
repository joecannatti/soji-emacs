;; soji.el --- Mindful Workday Tool

;; Package-Requires: ((emacs "24.5"))
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

(eval-when-compile
  (require 'cl))
(require 'org)
(require 'org-habit)
(require 'org-pomodoro)

(defgroup soji-settings nil "Tweakable stuff for the Soji Mindful Workday Tool"
  :group 'extensions)

(defcustom soji-file "~/Dropbox/Notes/soji_note/soji.org"
  "File where soji stores notes."
  :type 'file
  :set (lambda (varname val)
         (set-default varname
                      (expand-file-name val)))
  :require 'soji
  :group 'soji-settings)

(defcustom soji-work-tag "StitchFix"
  "How to tag work day soji files. Usually your company name is best."
  :type 'string
  :require 'soji
  :group 'soji-settings)

(defcustom soji-dim-percentage 70 "How transparent to make the screen during a break."
  :type 'number
  :require 'soji
  :group 'soji-settings)

(defcustom soji-bright-percentage 100 "How transparent to make the screen during normal work."
  :type 'number
  :require 'soji
  :group 'soji-settings)

(defcustom soji-pomodoro-length 25
  "Length of Pomodoros."
  :type 'number
  :require 'soji
  :group 'soji-settings)

(defcustom soji-break-length 5
  "Length of Breaks." :type 'number
:require 'soji
:group 'soji-settings)

(define-minor-mode soji-mode
  "Soji log editor mode"
  nil
  " Soji"
  '()
  :group 'soji-settings
  :lighter " Soji")

(defun soji-break (min)
  "Begin a break of length MIN."
  (interactive "nMinutes: ")
  (let ((org-pomodoro-short-break-length min))
    (soji-open)
    (org-pomodoro-start :short-break)))

(defun soji-open ()
  "Jump to the Soji home screen."
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
      ;;;(set-window-dedicated-p (selected-window) t)
      )))

(defun soji-work (length)
  "Begin a pomodoro of LENGTH."
  (interactive "P")
  (let ((org-pomodoro-length (or length soji-pomodoro-length)))
    (org-pomodoro))
  (org-narrow-to-subtree)
  (delete-other-windows)
  (set-window-dedicated-p (selected-window) t)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(defun soji-agenda-work (length)
  "Begin a pomodoro of LENGTH."
  (interactive "P")
  (org-agenda-switch-to)
  (soji-work length))

(defun soji-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion
                       (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE")
                 "habit")
        subtree-end
      nil)))

(defun soji-skip-subtree-if-not-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion
                       (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE")
                 "habit")
        nil
      subtree-end)))

(defun soji-dim ()
  "Dim Emacs or make it partially transparent."
  (interactive)
  (set-frame-parameter (selected-frame)
                       'alpha
                       soji-dim-percentage))

(defun soji-end ()
  "End a pomodoro."
  (interactive)
  (soji-dim)
  (soji-open))

(defun soji-bright ()
  "Brighten Emacs or make it opaque."
  (interactive)
  (set-frame-parameter (selected-frame)
                       'alpha
                       soji-bright-percentage))

(defun soji-org-agenda-mark-habits ()
  "Mark org-habit style TODOs."
  (when (not (get-text-property (point)
                                'org-series))
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

(advice-add #'org-agenda-finalize :before #'soji-org-agenda-mark-habits)
(add-hook 'org-pomodoro-started-hook 'soji-bright)
(add-hook 'org-pomodoro-finished-hook 'soji-end)
(add-hook 'org-pomodoro-killed-hook 'soji-dim)
(add-hook 'org-pomodoro-break-finished-hook 'soji-open)
(provide 'soji)
;;; soji.el ends here
