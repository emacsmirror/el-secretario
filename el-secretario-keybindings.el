;;; el-secretario-keybindings.el --- Keybindings that use general.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Leo
;;
;; Author: Leo Okawa Ericson <http://github/Zetagon>
;; Maintainer: Leo <github@relevant-information.com>
;; Created: June 08, 2021
;; Modified: June 08, 2021
;; Version: 0.0.1
;; Keywords: convenience
;; Homepage: https://git.sr.ht/~zetagon/el-secretario
;; Package-Requires: ((emacs "27.1") (el-secretario "0.0.1") (general "0.1"))
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
;; Default set of keybindings used in el-secretario. To use the default
;; keybindings load this file after loading el-secretario.
;;
;;; Code:
(require 'general)

(eval-after-load 'el-secretario-source
  (general-define-key
   :keymaps 'el-secretario-source-default-map
   "n" '(el-secretario-next-item :which-key "next")
   "p" '(el-secretario-previous-item :which-key "previous")))

(eval-after-load 'el-secretario-mu4e
  (general-define-key
   :keymaps 'el-secretario-mu4e-map
   "n" '(el-secretario-next-item :which-key "next")
   "p" '(el-secretario-previous-item :which-key "previous")))

(eval-after-load 'el-secretario-elfeed
  (general-define-key
   :keymaps 'el-secretario-elfeed-map
   "n" '(el-secretario-next-item :which-key "next")
   "p" '(el-secretario-previous-item :which-key "previous")
   "+" '(elfeed-show-tag :which-key "Add tag")
   "-" '(elfeed-show-untag :which-key "Remove tag")
   "c" '(org-capture :which-key "Org Capture")
   "o" '(elfeed-show-visit :which-key "Open in browser")))

(eval-after-load 'el-secretario-tasks
  (general-define-key
   :keymaps 'el-secretario-tasks-keymap
   "s" '((lambda () (interactive) (el-secretario-tasks--skip-task t)) :which-key "Skip task")
   "b" '(el-secretario-tasks-begin-task :which-key "Begin task")
   "t" '((lambda () (interactive) (el-secretario-message--with-pre-buffer (org-todo))) :which-key "TODO" )))

(eval-after-load 'el-secretario-org
  (general-define-key
   :keymaps 'el-secretario-org-keymap
   "n" '(el-secretario-next-item :which-key "next")
   "p" '(el-secretario-previous-item :which-key "previous")
   "r" '((lambda () (interactive) (org-refile) (el-secretario-next-item)) :wk "Refile")
   "R" '((lambda () (interactive)
           (let ((org-reverse-note-order t))
             (org-refile)
             (el-secretario-next-item))) :wk "Refile to top")
   "t" '(org-set-tags-command :wk "Tags")
   "T" '(org-todo :wk "Tags")
   "s" '(el-secretario-org-schedule :wk "Schedule")
   "z" '(el-secretario-org-ignore-current-item :wk "Snooze")
   "d" '(el-secretario-org-deadline :wk  "Deadline")
   "D" '((lambda () (interactive)
           (save-restriction
             (org-narrow-to-subtree)
             (delete-region (point-min) (point-max))))
         :wk "Delete visible")
   "q" '((lambda () (interactive) (el-secretario-end-sesion)) :wk "Quit"))

  (hercules-def
   :keymap 'el-secretario-org-keymap))

(eval-after-load 'el-secretario-message
  (general-define-key
   :keymaps 'el-secretario-message-message-keymap
   "q" '(el-secretario-message--back-to-pre-message :which-key "quit" ) ))

(defun el-secretario-org-schedule (arg &optional time)
  (interactive "P")
  (hercules--hide)
  (funcall-interactively #'org-schedule arg time)
  (el-secretario-activate-keymap))

(defun el-secretario-org-deadline (arg &optional time)
  (interactive "P")
  (hercules--hide)
  (funcall-interactively #'org-deadline arg time)
  (el-secretario-activate-keymap))

(provide 'el-secretario-keybindings)
;;; el-secretario-keybindings.el ends here
