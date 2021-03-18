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
(require 'hydra)
(require 'el-secretario)
(require 'el-secretario-org)



(describe "Org module"
  :var* (file source count-fun)


  (before-each

    (setf (symbol-function 'count-fun) (lambda () ))
    (spy-on 'count-fun)

    (unless (file-exists-p (concat (temporary-file-directory) "el-secretario"))
      (make-directory (concat (temporary-file-directory) "el-secretario")))
    (setq file (find-file-noselect
                (concat (temporary-file-directory) "el-secretario/tmp-test.org")))

    (setq source (list (el-secretario-org-make-source '(todo)
                                                      (list file)
                                                      #'count-fun)))

    (with-current-buffer file
      (read-only-mode -1)
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
** TODO Sync remarkable inbox
:PROPERTIES:
:EL-SECRETARIO-REVIEW-TASK-HOOK: (el-secretario-message--display-message-prompt \"Sync reMarkable Inbox\")
:END:

")
      (save-buffer)))

  (it "can run a test!"
    (expect t :to-be t))

  (it "runs the next-item hook"
    (el-secretario-start-session source)
    (dotimes (_ 7)
      (el-secretario-next-item))
    (expect 'count-fun :to-have-been-called-times 7)))

(provide 'test-el-secretario)
;;; test-el-secretario.el ends here
