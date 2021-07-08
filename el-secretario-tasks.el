;;; el-secretario-tasks.el --- The tasks module -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Leo
;;
;; Author: Leo <https://github.com/Zetagon>
;; Maintainer: Leo <github@relevant-information.com>
;; Created: December 26, 2020
;; Modified: December 26, 2020
;; Version: 0.0.1
;; Keywords: convenience
;; Homepage: https://git.sr.ht/~zetagon/el-secretario
;; Package-Requires: ((emacs "27.1")  (org-ql "0.6-pre") (dash "2.18.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module is still a bit unclear to me
;;
;;; Code:
(require 'el-secretario-source)
(require 'dash)
(defclass el-secretario-tasks-source (el-secretario-source)
  ((next-item-hook :initarg :next-item-hook
                   :initform nil)
   (tasks-left :initform nil)
   (tasks-skipped :initform nil)))

(defvar el-secretario-tasks-keymap (make-sparse-keymap))

(defun el-secretario-tasks-begin (&optional keymap next-item-hook)
  "TODO"
  (interactive)
  (el-secretario-start-session
   (el-secretario-tasks-source
    :keymap-body (or keymap #'el-secretario-tasks-keymap)
    :next-item-hook (or next-item-hook (lambda ())))))

;; TODO Take an optional marker argument. Go to that marker instead of clocking task. If nil, goto clocking task
(cl-defmethod el-secretario-source-init ((obj el-secretario-tasks-source) &optional ARG)
  "TODO"
  ;; Sort according to priority
  (with-slots (tasks-left tasks-skipped) obj
    (setq tasks-left nil)
    (save-excursion
      (unless ARG (org-clock-goto))
      (save-restriction
        (org-save-outline-visibility t
          (org-show-all)
          (let ((closest-todo-upwards (point)))
            (while (and (el-secretario-org-up-heading 1)
                        (not (org-get-todo-state)))
              ;; TODO Error recovery if there is no parent heading with project todo state
              (when (org-get-todo-state)
                (setq closest-todo-upwards (point))))
            (goto-char closest-todo-upwards)
            (org-narrow-to-subtree)
            (while (outline-next-heading)
              (when (member (org-get-todo-state) org-not-done-keywords)
                (push (el-secretario-org--parse-headline) tasks-left)))))))
    (setq items-done nil)
    (setq tasks-left (nreverse tasks-left))
    (el-secretario-activate-keymap)
    (el-secretario-tasks--skip-task)))


(cl-defmethod el-secretario-source-next-item ((obj el-secretario-tasks-source))
  "Skip a task.
If DECREASE-PRIORITY is non-nil also decrease its priority."
  (with-slots (tasks-left tasks-skipped next-item-hook) obj
    (if-let ((task (pop tasks-left)))
        (progn
          (push task tasks-skipped)
          (switch-to-buffer (plist-get task :buffer))
          (widen)
          (goto-char (plist-get task :begin))
          (org-narrow-to-subtree)
          (el-secretario-tasks--run-task-hook (el-secretario-org--parse-headline)
                           :EL-SECRETARIO-REVIEW-TASK-HOOK)

          (funcall next-item-hook)
          (el-secretario-activate-keymap))
      (el-secretario--next-source))))


(defun el-secretario-tasks-begin-task ()
  "Begin working on a task.
In particular, run it's begin task hook."
  (interactive)
  (el-secretario-tasks--run-begin-task-hook (el-secretario-org--parse-headline)))

(defvar el-secretario-tasks-files nil
  "The files that `el-secretario-tasks-choose-task'a will search through")


(defun el-secretario-tasks--run-task-hook (task hook-name &optional default-hook)
  "Run a hook defined in the property of a org subtree.
The hook will be called at the beginning of the line of the headline.

TASK is a plist from `el-secretario-org--parse-headline'.
HOOK-NAME is the org property that the hook is stored in.
DEFAULT-HOOK is a quoted s-exp to run if there is no hook in this subtree."
  (with-current-buffer (plist-get task :buffer)
    (save-excursion
      (goto-char (plist-get task :begin))
      (eval (or (-some-> (plist-get task hook-name)
                  (read))
                default-hook)
            t))))

;; TODO refactor this to use `org-entry-properties' instead
(defun el-secretario-tasks--run-begin-task-hook (task)
  "Run the begin task hook.
See `el-secretario-tasks--run-task-hook' for more info. "
  (el-secretario-tasks--run-task-hook task :EL-SECRETARIO-BEGIN-TASK-HOOK '(org-clock-in)))


;; TODO rename to something better
(defun el-secretario-tasks--finish-task-hook ()
  (when (member org-state org-done-keywords)
    (el-secretario-tasks--run-task-hook (el-secretario-org--parse-headline)
                     :EL-SECRETARIO-FINISH-TASK-HOOK)))




(provide 'el-secretario-tasks)
;;; el-secretario-tasks.el ends here
