;;; el-secretario-org-space.el --- Spaced repetition module of el-secretario -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Leo
;;
;; Author: Leo <https://github.com/Zetagon>
;; Maintainer: Leo <github@relevant-information.com>
;; Created: March 21, 2021
;; Modified: March 21, 2021
;; Version: 0.0.1
;; Keywords: convenience
;; Homepage: https://git.sr.ht/~zetagon/el-secretario
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file LICENSE.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;;  Spaced repetition module of el-secretario
;;
;;; Code:
(require 'el-secretario-source)
(require 'el-secretario)
(require 'dash)
(require 'org)


(defvar el-secretario-org-space-increment-percentage nil
  "The percent of tasks to statistically defer.
nil means 100% are deferred")

(defun el-secretario-org-space--increment ()
  "Increment the delta value of the delta property with a spaced repetition algorithm."
  (unless (and el-secretario-org-space-increment-percentage
             (<= el-secretario-org-space-increment-percentage (random 100)))
    (let ((cap
           (-some-> (org-entry-get (point)
                                   "EL-SECRETARIO-DELTA-CAP")
             string-to-number))
          (reset-cap
           (-some-> (org-entry-get (point)
                                   "EL-SECRETARIO-DELTA-RESET-CAP")
             string-to-number)))
      (--> (org-entry-get (point)
                          "EL-SECRETARIO-DELTA")
        (or it "1")
        (string-to-number it)
        (if (and cap (>= it cap))
            it
          (1+ it))
        (if (and reset-cap (>= it reset-cap))
            2
          it)
        (number-to-string it)
        (org-set-property "EL-SECRETARIO-DELTA" it)))))

;;;###autoload
(defun el-secretario-org-space--reset ()
  "Reset the delta value to the default value."
 (org-set-property "EL-SECRETARIO-DELTA" "2") )

;;;###autoload
(defun el-secretario-org-space-reschedule ()
  "Reschedule org entry at point n days into the future.
Where n is incremented by 1 for each time this function is called on that entry"
  (let ((delta (org-entry-get (point)
                              "EL-SECRETARIO-DELTA")))
    (org-schedule nil (concat "+" delta "d")))
  (el-secretario-org-space--increment))

;;;###autoload
(defun el-secretario-org-space-schedule-and-reset (arg &optional time no-hercules)
  "Like `org-schedule' but it also resets the delta property.
TIME and ARG is passed through to `org-schedule'
Resetting is done with `el-secretario-org-space-reset'
If NO-HERCULES is non-nil, don't bring up the current source's keymap."
  (interactive "P")
  (unless time
    (hercules--hide))
  (el-secretario-org-space--reset)
  (org-schedule arg time)
  (unless no-hercules
    (el-secretario-activate-keymap)))

;;;###autoload
(defun el-secretario-org-space-compare-le (x y)
  "Return t if X is scheduled before Y, nil otherwise.

An unscheduled element is considered to be scheduled before all other elements.

The use case for this is to pass it as sorting function to
`el-secretario-org-make-source'.

X and Y should be elements as returned by `el-secretario-org--parse-headline'."
  (if-let ((x-scheduled (plist-get x :scheduled)))
      (if-let ((y-scheduled (plist-get y :scheduled)))
          (time-less-p (org-timestamp-to-time x-scheduled)
                       (org-timestamp-to-time y-scheduled))
        nil)
    t))

(provide 'el-secretario-org-space)
;;; el-secretario-org-space.el ends here
