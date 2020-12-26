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
       (plist-put it :ELSECRETARIOPRIORITY
                  (string-to-number (plist-get
                                     it
                                     :ELSECRETARIOPRIORITY)))))

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
            (org-ql-select files '(property "ELSECRETARIOPRIORITY")
              :action #'el-secretario-tasks--query-action
              :sort (lambda (x y)
                      (< (plist-get x :ELSECRETARIOPRIORITY)
                         (plist-get y :ELSECRETARIOPRIORITY)))))))
    (dolist (task task-list)
      (switch-to-buffer  (plist-get task :buffer))
      (save-restriction
        (widen)
        (goto-char (plist-get task :begin))
        (org-narrow-to-subtree)
        (if (y-or-n-p "Do you want to work on this task? ")
            (break)
          (let ((priority (plist-get task :ELSECRETARIOPRIORITY)))
            (org-set-property "ELSECRETARIOPRIORITY"
                              (number-to-string (+ (max (round  (* 0.2 priority)) 1)
                                                   priority)))))))))

(dolist (x '(1 2 3))
  (when (= x 2)
    (return x)))
(provide 'el-secretario-tasks)
;;; el-secretario-tasks.el ends here
