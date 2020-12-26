;;; el-secretario-tasks.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Leo
;;
;; Author: Leo <http://github/leo>
;; Maintainer: Leo <leo@leo-B85-HD3>
;; Created: December 26, 2020
;; Modified: December 26, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/leo/el-secretario-tasks
;; Package-Requires: ((emacs 26.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defun el-secretario-tasks--parse-headline ()
  "Parse headline at point and put in some more relevant information"
  (--> (org-element-headline-parser (line-end-position))
       (nth 1 it)
       (plist-put it :file-name (buffer-file-name))
       (plist-put it :buffer (current-buffer))
       (plist-put it :EL-SECRETARIO-PRIORITY
                  (string-to-number (plist-get
                                     it
                                     :EL-SECRETARIO-PRIORITY)))))

(defun el-secretario-tasks-choose-task ()
  "Choose a task based on a priority queue.
This will go through the matched tasks from the lowest priority
first until you find a task you want to work on. All tasks you
dismissed will get their priority decreased.
"
  (interactive)
  (let* ((files (list "/tmp/test.org"))
         (task-list
          (progn
            ;; Bypass the caching for org-ql as it breaks some stuff
            (dolist (f files)
              (with-current-buffer (find-file-noselect f)
                (set-buffer-modified-p t)
                (basic-save-buffer)))
            ;; Sort according to priority
            (org-ql-select files '(property "EL-SECRETARIO-PRIORITY")
              :action #'el-secretario-tasks--parse-headline
              :sort (lambda (x y)
                      (< (plist-get x :EL-SECRETARIO-PRIORITY)
                         (plist-get y :EL-SECRETARIO-PRIORITY)))))))
    ;; Go through the tasks and ask the user if they want to do it
    (dolist (task task-list)
      (switch-to-buffer  (plist-get task :buffer))
      (save-restriction
        (widen)
        (goto-char (plist-get task :begin))
        (org-narrow-to-subtree)
        (if (not (y-or-n-p "Do you want to work on this task? "))
            (let ((priority (plist-get task :EL-SECRETARIO-PRIORITY)))
              (org-set-property "EL-SECRETARIO-PRIORITY"
                                (number-to-string (+ (max (round  (* 0.2 priority)) 1)
                                                     priority))))
          (el-secretario-tasks--run-begin-task-hook task)
          (return))))))

(defun el-secretario-tasks--run-task-hook (task hook-name default-hook)
  "Run a hook defined in the property of a org subtree.
The hook will be called at the beginning of the line of the headline.

TASK is a plist from `el-secretario-tasks--parse-headline'.
HOOK-NAME is the org property that the hook is stored in.
DEFAULT-HOOK is a quoted s-exp to run if there is no hook in this subtree."
  (with-current-buffer (plist-get task :buffer)
    (save-excursion
      (goto-char (plist-get task :begin))
      (eval (or (-some-> (plist-get task hook-name)
                  (read))
                default-hook
                )
            t))))

(defun el-secretario-tasks--run-begin-task-hook (task)
  "Run the begin task hook.
See `el-secretario-tasks--run-task-hook' for more info. "
  (el-secretario-tasks--run-task-hook task :EL-SECRETARIO-BEGIN-TASK-HOOK '(message "Mock clock in")))

(defun el-secretario-tasks--finish-task-hook ()
  (when (member org-state org-done-keywords)
    (el-secretario-tasks--run-task-hook (el-secretario-tasks--parse-headline) :EL-SECRETARIO-FINISH-TASK-HOOK '(identity))))

(add-hook 'org-after-todo-state-change-hook #'el-secretario-tasks--finish-task-hook)

(provide 'el-secretario-tasks)
;;; el-secretario-tasks.el ends here
