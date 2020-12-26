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

(defun el-secretario-tasks--query-action ()
  (--> (org-element-headline-parser (line-end-position))
       (nth 1 it)
       (plist-put it :file-name (buffer-file-name))
       (plist-put it :buffer (current-buffer))
       (plist-put it :EL-SECRETARIO-PRIORITY
                  (string-to-number (plist-get
                                     it
                                     :EL-SECRETARIO-PRIORITY)))))

(defun el-secretario-tasks-choose-task ()
  (interactive)
  (let* ((files (list "/tmp/test.org"))
         (task-list
          ;; Bypass the caching for org-ql as it breaks some stuff
          (progn
            (dolist (f files)
              (with-current-buffer (find-file-noselect f)
                (set-buffer-modified-p t)
                (basic-save-buffer)))
            (org-ql-select files '(property "EL-SECRETARIO-PRIORITY")
              :action #'el-secretario-tasks--query-action
              :sort (lambda (x y)
                      (< (plist-get x :EL-SECRETARIO-PRIORITY)
                         (plist-get y :EL-SECRETARIO-PRIORITY)))))))
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
          (el-secretario-tasks-run-begin-task-hook task)
          (return))))))

(defun el-secretario-tasks-run-begin-task-hook (task)
  (with-current-buffer (plist-get task :buffer)
    (save-excursion
      (goto-char (plist-get task :begin))
      (eval (or (-some-> (plist-get task :EL-SECRETARIO-BEGIN-TASK-HOOK)
                  (read))
                ;; "(org-clock-in)"
                '(message "Mock clock in!")
                )
            t))))
(provide 'el-secretario-tasks)
;;; el-secretario-tasks.el ends here
