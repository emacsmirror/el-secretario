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
;;; Commentary:
;;
;;  description
;;
;;; Code:

(require 'buttercup)


(describe "Org module"
  :var (buf source count-fun)

  (before-all
    (let ((files (list (get-buffer-create "*el-secretario-test-buf*"))))
      (setq source (list (el-secretario-org--make-source '(todo)
                                                        files
                                                        #'count-fun))))
    )

  (before-each
    (setf (symbol-function 'count-fun) (lambda () ))
    (setq buf (get-buffer-create "*el-secretario-test-buf*"))
    (with-current-buffer buf (insert "foo"))
    (display-warning 'buttercup
                     (concat "foo:" (prin1-to-string (with-current-buffer buf (buffer-substring-no-properties (point-min)
                                                                                                              (point-max))))))
    (with-current-buffer buf
      (delete-region (point-min) (point-max))
      (org-mode)
      (insert "
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
** WAITING Sync phone inbox
:PROPERTIES:
:EL-SECRETARIO-REVIEW-TASK-HOOK: (el-secretario-message--display-message-prompt \"Sync phone Inbox\")
:END:

** TODO Sync remarkable inbox
:PROPERTIES:
:EL-SECRETARIO-REVIEW-TASK-HOOK: (el-secretario-message--display-message-prompt \"Sync reMarkable Inbox\")
:END:

"))
    )

  (xit "runs!"
    (expect t :to-be t))
  (it "runs the next-item hook"
    (message source)

    (expect count-fun :to-have-been-called-times 8)))

(provide 'test-el-secretario)
;;; test-el-secretario.el ends here
