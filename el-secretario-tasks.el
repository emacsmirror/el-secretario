;;; el-secretario-tasks.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Leo
;;
;; Author: Leo <https://github.com/Zetagon>
;; Maintainer: Leo <github@relevant-information.com>
;; Created: December 26, 2020
;; Modified: December 26, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://git.sr.ht/~zetagon/el-secretario
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5") (org-ql "0.6-pre"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;; (el-secretario-start-session (list (el-secretario-tasks-make-source (todo) ("~/Documents/el-secretario/test/test.org"))))
;;; Code:

(defun el-secretario-tasks-make-source (query files &optional keymap)
  "QUERY is an arbitrary org-ql query. FILES is the files to search through.
NEXT-ITEM-HOOk is called on each heading.
KEYMAP is a keymap to use during review of this source."
  (make-el-secretario-source
    :init-function  (lambda () (el-secretario-tasks--init query files ))
    :next-function #'el-secretario-tasks--skip-task
    :prev-function (lambda ())
    :keymap-body (or keymap #'el-secretario-tasks-keymap)
    :finished-hook #'widen
    :next-item-hook (lambda ())) )

(defvar el-secretario-tasks-keymap (make-sparse-keymap))
(general-define-key
 :keymaps 'el-secretario-tasks-keymap
 "s" '((lambda () (el-secretario-tasks--skip-task t)) :which-key "Skip task")
 "b" '((lambda () el-secretario-tasks-begin-task) :which-key "Begin task")
 "t" '((lambda () (el-secretario-message--with-pre-buffer (org-todo))) :which-key "TODO" ))


(defvar el-secretario-tasks--tasks-left nil)
(defvar el-secretario-tasks--tasks-skipped nil)

(defun el-secretario-tasks--init (query files)
  "The initialization function for the tasks source"
  (interactive)
  ;; Bypass the caching for org-ql as it breaks some stuff
  (dolist (f files)
    (with-current-buffer (find-file-noselect f)
      (set-buffer-modified-p t)
      (basic-save-buffer)))
  ;; Sort according to priority
  (setq el-secretario-tasks--tasks-left
        (org-ql-select files query
          :action #'el-secretario-org--parse-headline
          :sort (lambda (x y)
                  (< (plist-get x :EL-SECRETARIO-PRIORITY)
                     (plist-get y :EL-SECRETARIO-PRIORITY)))))
  (setq el-secretario-tasks--items-done nil)
  (el-secretario/activate-keymap)
  (el-secretario-tasks--skip-task))

(defun el-secretario-tasks--skip-task (&optional decrease-priority)
  "Skip a task.
If DECREASE-PRIORITY is non-nil also decrease its priority."
  (interactive)
  (if-let ((task (pop el-secretario-tasks--tasks-left)))
      (progn
        (push task el-secretario-tasks--tasks-skipped)
        (switch-to-buffer (plist-get task :buffer))
        (widen)
        (goto-char (plist-get task :begin))
        (org-narrow-to-subtree)

        (when decrease-priority
          (let ((priority (plist-get task :EL-SECRETARIO-PRIORITY)))
            (org-set-property "EL-SECRETARIO-PRIORITY"
                              (number-to-string (+ (max (round  (* 0.2 priority)) 1)
                                                   priority)))))
        (funcall (el-secretario-source-next-item-hook
                  (car el-secretario--current-source-list)))
        (el-secretario/activate-keymap))
    (el-secretario--next-source)))


(defun el-secretario-tasks-begin-task ()
  "Begin working on a task.
In particular, run it's begin task hook."
  (interactive)
  (el-secretario-tasks--run-begin-task-hook (el-secretario-org--parse-headline)))

(defvar el-secretario-tasks-files nil
  "The files that `el-secretario-tasks-choose-task'a will search through")

(defun el-secretario-tasks-choose-task ()
  "Choose a task based on a priority queue from `el-secretario-tasks-files'.
This will go through the matched tasks from the lowest priority
first until you find a task you want to work on. All tasks you
dismissed will get their priority decreased.
"
  (interactive)
  (let* ((task-list
          (progn
            ;; Bypass the caching for org-ql as it breaks some stuff
            (dolist (f el-secretario-tasks-files)
              (with-current-buffer (find-file-noselect f)
                (set-buffer-modified-p t)
                (basic-save-buffer)))
            ;; Sort according to priority
            (org-ql-select el-secretario-tasks-files '(property "EL-SECRETARIO-PRIORITY")
              :action #'el-secretario-org--parse-headline
              :sort (lambda (x y)
                      (< (plist-get x :EL-SECRETARIO-PRIORITY)
                         (plist-get y :EL-SECRETARIO-PRIORITY)))))))
    ;; Go through the tasks and ask the user if they want to do it
    (cl-dolist (task task-list)
      (switch-to-buffer  (plist-get task :buffer))
      (save-restriction
        (widen)
        (goto-char (plist-get task :begin))
        (org-narrow-to-subtree)
        (if (not (el-secretario--y-or-n-p "Do you want to work on this task? "))
            (let ((priority (plist-get task :EL-SECRETARIO-PRIORITY)))
              (org-set-property "EL-SECRETARIO-PRIORITY"
                                (number-to-string (+ (max (round  (* 0.2 priority)) 1)
                                                     priority))))
          (el-secretario-tasks--run-begin-task-hook task)
          (cl-return))))))

(defun el-secretario-tasks--normalize-priorities (tasks)
  "Normalize the priorities of TASKS.
This is done by subtracting every priority with the lowest
priority."
  (-->
      (-min-by (-on #'> (lambda (x) (plist-get x :EL-SECRETARIO-PRIORITY))) tasks)
    (save-excursion
      (mapc (lambda (t)
              (with-current-buffer (plist-get t :buffer)
                (goto-char (plist-get t :begin))
                (org-set-property "EL-SECRETARIO-PRIORITY"
                                  (-> (- (plist-get t :EL-SECRETARIO-PRIORITY)
                                         (plist-get it :EL-SECRETARIO-PRIORITY))
                                    (number-to-string))))) tasks))))

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
    (el-secretario-tasks--run-task-hook
     (el-secretario-org--parse-headline)
     :EL-SECRETARIO-FINISH-TASK-HOOK)))


(defun el-secretario-tasks-subtask-begin (&optional keymap)
  "TODO"
  (interactive)
  (el-secretario-start-session
   (list (make-el-secretario-source
          :init-function  (lambda () (el-secretario-tasks--subtask-init))
          :next-function #'el-secretario-tasks--skip-task
          :prev-function (lambda ())
          :keymap-body (or keymap #'el-secretario-tasks-keymap)
          :finished-hook #'widen
          :next-item-hook (lambda ()
                            (el-secretario-tasks--run-task-hook
                             (el-secretario-org--parse-headline)
                             :EL-SECRETARIO-REVIEW-TASK-HOOK))))) )



;; TODO Take an optional marker argument. Go to that marker instead of clocking task. If nil, goto clocking task
(defun el-secretario-tasks--subtask-init (&optional ARG)
  "TODO"
  (interactive)
  ;; Sort according to priority
  (setq el-secretario-tasks--tasks-left nil)
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
              (push (el-secretario-org--parse-headline) el-secretario-tasks--tasks-left)))))))
  (setq el-secretario-tasks--items-done nil)
  (setq el-secretario-tasks--tasks-left (nreverse el-secretario-tasks--tasks-left))
  (el-secretario/activate-keymap)
  (el-secretario-tasks--skip-task))


(provide 'el-secretario-tasks)
;;; el-secretario-tasks.el ends here
