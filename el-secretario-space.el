;;; el-secretario-space.el --- Spaced repetition module of el-secretario -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Leo
;;
;; Author: Leo <https://github.com/Zetagon>
;; Maintainer: Leo <github@relevant-information.com>
;; Created: March 21, 2021
;; Modified: March 21, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/Zetagon/el-secretario
;; Package-Requires: ((emacs 26.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Spaced repetition module of el-secretario
;;
;;; Code:

(defun el-secretario-space--increment ()
  (--> (org-entry-get (point)
                      "EL-SECRETARIO-DELTA")
    (or it "1")
    (string-to-number it)
    (1+ it)
    (number-to-string it)
    (org-set-property "EL-SECRETARIO-DELTA" it)))

(defun el-secretario-space--reset ()
 (org-set-property "EL-SECRETARIO-DELTA" "1") )

(defun el-secretario-space-reschedule ()
  "Reschedule org entry at point n days into the future.
Where n is incremented by 1 for each time this function is called on that entry"
  (let ((delta (org-entry-get (point)
                              "EL-SECRETARIO-DELTA")))
    (org-schedule nil (concat "+" delta "d")))
  (el-secretario-space--increment))

(defun el-secretario-space-schedule-and-reset (&optional time)
  "Like `org-schedule' but it also resets the delta property.
TIME is passed through to `org-schedule'
Resetting is done with `el-secretario-space-reset'"
  (interactive)
  (el-secretario-space--reset)
  (call-interactively #'org-schedule))

(provide 'el-secretario-space)
;;; el-secretario-space.el ends here
