;;; el-secretario-keybindings.el Keybindings that use general.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Leo
;;
;; Author: Leo Okawa Ericson <http://github/Zetagon>
;; Maintainer: Leo <github@relevant-information.com>
;; Created: June 08, 2021
;; Modified: June 08, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://git.sr.ht/~zetagon/el-secretario
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(require 'general)

(general-define-key
 :keymaps 'el-secretario-default-map
 "n" '(el-secretario/next-item :which-key "next")
 "p" '(el-secretario/previous-item :which-key "previous"))

(general-define-key
 :keymaps 'el-secretario-tasks-keymap
 "s" '((lambda () (interactive) (el-secretario-tasks--skip-task t)) :which-key "Skip task")
 "b" '(#'el-secretario-tasks-begin-task :which-key "Begin task")
 "t" '((lambda () (interactive) (el-secretario-message--with-pre-buffer (org-todo))) :which-key "TODO" ))

(general-define-key
 :keymaps 'el-secretario-org-keymap
 "n" '(el-secretario/next-item :which-key "next")
 "p" '(el-secretario/previous-item :which-key "previous")
 "r" '((lambda () (interactive) (org-refile) (el-secretario/next-item)) :wk "Refile")
 "R" '((lambda () (interactive)
         (let ((org-reverse-note-order t))
           (org-refile)
           (el-secretario/next-item))) :wk "Refile to top")
 "t" '(org-set-tags-command :wk "Tags")
 "T" '(org-todo :wk "Tags")
 "s" '(el-secretario-org-schedule :wk "Schedule")
 "z" '(el-secretario-org/ignore-current-item :wk "Snooze")
 "d" '(el-secretario-org-deadline :wk  "Deadline")
 "D" '((lambda () (interactive)
         (save-restriction
           (org-narrow-to-subtree)
           (delete-region (point-min) (point-max))))
       :wk "Delete visible")
 "q" '((lambda () (interactive) (el-secretario-end-sesion)) :wk "Quit"))

(hercules-def
 :keymap 'el-secretario-org-keymap)

(general-define-key
 :keymaps 'el-secretario-message-message-keymap
 "q" '(el-secretario-message--back-to-pre-message :which-key "quit" ) )

(defun el-secretario-org-schedule (arg &optional time)
  (interactive "P")
  (hercules--hide)
  (funcall-interactively #'org-schedule arg time)
  (el-secretario/activate-keymap))

(defun el-secretario-org-deadline (arg &optional time)
  (interactive "P")
  (hercules--hide)
  (funcall-interactively #'org-deadline arg time)
  (el-secretario/activate-keymap))

(provide 'el-secretario-keybindings)
;;; el-secretario-keybindings.el ends here
