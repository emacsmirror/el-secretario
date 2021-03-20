;;; test-el-secretario.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Leo
;;
;; Author: Leo <http://github/leo>
;; Maintainer: Leo <leo@leo-B85-HD3>
;; Created: February 17, 2021
;; Modified: February 17, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/leo/test-el-secretario
;; Package-Requires: ((emacs 26.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;;  Commentary:
;;
;;  description
;;
;;; Code:

(require 'buttercup)
(require 'hydra)
(require 'el-secretario)
(require 'el-secretario-org)

(setq buttercup-colors '((black . 30)
                         (red . 31)
                         (green . 32)
                         (yellow . 34)
                         (blue . 34)
                         (magenta . 35)
                         (cyan . 36)
                         (white . 37))
     el-secretario-is-testing t)
(defvar el-secretario-org-buffer-s  "
* TODO FOO
:PROPERTIES:
:EL-SECRETARIO-PRIORITY: 74
:END:

* TODO bar
:PROPERTIES:
:EL-SECRETARIO-PRIORITY: 85
:EL-SECRETARIO-BEGIN-TASK-HOOK: (insert \"foobar\")
:END:

* TODO baz
:PROPERTIES:
:EL-SECRETARIO-PRIORITY: 106
:EL-SECRETARIO-FINISH-TASK-HOOK: (insert \"foobar\")
:END:

** TODO subtask1
** TODO subtask2
* TODO Daily review
:PROPERTIES:
:EL-SECRETARIO-BEGIN-TASK-HOOK: (progn  (el-secretario-tasks-subtask-begin))
:END:
** TODO Sync remarkable inbox
:PROPERTIES:
:EL-SECRETARIO-REVIEW-TASK-HOOK: (review-item-fun)
:END:

")

(defmacro test-el-secretario-reset-file (s file)
  `(progn
     (unless (file-exists-p (concat (temporary-file-directory) "el-secretario"))
       (make-directory (concat (temporary-file-directory) "el-secretario")))
     (setq ,file (find-file-noselect
                 (concat (temporary-file-directory) "el-secretario/tmp-test.org")))
     (with-current-buffer file
       (read-only-mode -1)
       (widen)
       (delete-region (point-min) (point-max))
       (org-mode)
       (insert ,s)
       (save-buffer))))

(describe "Org module"
  :var* (file source next-item-fun review-item-fun)


  (before-each

    (test-el-secretario-reset-file el-secretario-org-buffer-s file)

    (setf (symbol-function 'next-item-fun) (lambda () ))
    (spy-on 'next-item-fun)

    (setf (symbol-function 'review-item-fun) (lambda () ))
    (spy-on 'review-item-fun)

    (setq source (list (el-secretario-org-make-source '(todo)
                                                      (list file)
                                                      #'next-item-fun))))

  (it "can run a test!"
    (expect t :to-be t))

  (it "runs the next-item hook on each todo heading"
    (el-secretario-start-session source)
    (dotimes (_ 6)
      (el-secretario-next-item))
    (expect 'next-item-fun :to-have-been-called-times 7))

  (it "runs the review-item hook on each todo heading"
    (el-secretario-start-session (list (el-secretario-org-make-source '(todo)
                                                                      (list file)
                                                                      #'next-item-fun)))
    (dotimes (_ 7)
      (el-secretario-next-item))
    (expect 'review-item-fun :to-have-been-called-times 1)))

(describe "Tasks module"
  :var* (file source next-item-fun review-item-fun)

  (before-each

    (test-el-secretario-reset-file el-secretario-org-buffer-s file)

    (setf (symbol-function 'next-item-fun) (lambda () ))
    (spy-on 'next-item-fun)

    (setf (symbol-function 'review-item-fun) (lambda () ))
    (spy-on 'review-item-fun)

    (setq source (list (el-secretario-org-make-source '(todo)
                                                      (list file)
                                                      #'next-item-fun))))
  (it "can increase the priority value of tasks that are skipped"
    (let ((el-secretario-tasks-files (list (buffer-file-name file)))
          (el-secretario--y-or-no-p-input-list '(nil nil y)))
      (el-secretario-tasks-choose-task))
    (with-current-buffer file
      (widen)
      (goto-char 0)
      (search-forward  "EL-SECRETARIO-PRIORITY")


      (expect (string-to-number (org-entry-get (point)
                                               "EL-SECRETARIO-PRIORITY"))
              :to-be-greater-than
              74)
      (search-forward  "EL-SECRETARIO-PRIORITY")


      (expect (string-to-number (org-entry-get (point)
                                               "EL-SECRETARIO-PRIORITY"))
              :to-be-greater-than
              85)
      (search-forward  "EL-SECRETARIO-PRIORITY")


      (expect (string-to-number (org-entry-get (point)
                                               "EL-SECRETARIO-PRIORITY"))
              :to-equal
              106)))
  (describe "Subtasks"))

(provide 'test-el-secretario)
;;; test-el-secretario.el ends here
