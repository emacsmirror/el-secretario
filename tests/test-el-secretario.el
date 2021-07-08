;;; test-el-secretario.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Leo
;;
;; Author: Leo <https://github.com/Zetagon>
;; Maintainer: Leo <github@relevant-information.com>
;; Created: February 17, 2021
;; Modified: February 17, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://git.sr.ht/~zetagon/el-secretario
;; Package-Requires: ((emacs "27.1") )
;;
;; This file is not part of GNU Emacs.
;;
;;;  Commentary:
;;
;;  description
;;
;;; Code:

(require 'buttercup)
(require 'el-secretario)
(require 'el-secretario-message)
(require 'el-secretario-tasks)
(require 'el-secretario-source)
(require 'el-secretario-org)
(require 'el-secretario-space)
(require 'el-secretario-function)

(setq buttercup-colors '((black . 30)
                         (red . 31)
                         (green . 32)
                         (yellow . 34)
                         (blue . 34)
                         (magenta . 35)
                         (cyan . 36)
                         (white . 37))
     el-secretario--is-testing t)
(defvar el-secretario-org-buffer-s  "
* TODO FOO :a:
:PROPERTIES:
:EL-SECRETARIO-PRIORITY: 74
:END:

* TODO bar :b:
:PROPERTIES:
:EL-SECRETARIO-PRIORITY: 85
:EL-SECRETARIO-BEGIN-TASK-HOOK: (insert \"foobar\")
:END:

* TODO baz :a:
:PROPERTIES:
:EL-SECRETARIO-PRIORITY: 106
:EL-SECRETARIO-FINISH-TASK-HOOK: (insert \"foobar\")
:END:

** TODO subtask1
:PROPERTIES:
:ID:       sub-task1
:END:
** TODO subtask2
:PROPERTIES:
:ID:       sub-task2
:END:
* TODO Daily review :b:
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
       (save-buffer)
       (goto-char 0))))

(defun foo ()
  (interactive)
  (el-secretario-start-session (list (el-secretario-org-make-source '(tags "a")
                                                                    (list (find-file-noselect
                                                                           "/tmp/el-secretario/tmp-test.org"))
                                                                      :next-item-hook #'next-item-fun)
                                       (el-secretario-org-make-source '(tags "b")
                                                                      (list (find-file-noselect
                                                                           "/tmp/el-secretario/tmp-test.org"))
                                                                      :next-item-hook #'next-item-fun))))
(describe "Example module"
  (it "can go through the items"
    (el-secretario-start-session (el-secretario-example-source
                                  :items-left '(1 2 3 4 5)))
    (expect (el-secretario-example-get-current-val (car el-secretario--current-source-list))
            :to-equal 1)
    (el-secretario-next-item)
    (expect (el-secretario-example-get-current-val (car el-secretario--current-source-list))
            :to-equal 2)
    (el-secretario-next-item)
    (expect (el-secretario-example-get-current-val(car el-secretario--current-source-list))
            :to-equal 3)
    (el-secretario-next-item)
    (expect (el-secretario-example-get-current-val(car el-secretario--current-source-list))
            :to-equal 4)
    (el-secretario-next-item)
    (expect (el-secretario-example-get-current-val(car el-secretario--current-source-list))
            :to-equal 5))
  (it "can go backwards"
    (el-secretario-start-session (el-secretario-example-source
                                  :items-left '(1 2 3 4 5)))
    (expect (el-secretario-example-get-current-val (car el-secretario--current-source-list))
            :to-equal 1)
    (el-secretario-previous-item)
    (expect (el-secretario-example-get-current-val (car el-secretario--current-source-list))
            :to-equal 1)
    (el-secretario-next-item)
    (expect (el-secretario-example-get-current-val (car el-secretario--current-source-list))
            :to-equal 2)
    (el-secretario-next-item)
    (expect (el-secretario-example-get-current-val(car el-secretario--current-source-list))
            :to-equal 3))
  (it "can go to the next source when there are no items left"
    (el-secretario-start-session (list (el-secretario-example-source
                                        :items-left '(1 2))
                                       (el-secretario-example-source
                                        :items-left '(3 4))))
    (expect (el-secretario-example-get-current-val (car el-secretario--current-source-list))
            :to-equal 1)
    (el-secretario-next-item)
    (expect (el-secretario-example-get-current-val (car el-secretario--current-source-list))
            :to-equal 2)
    (el-secretario-next-item)
    (expect (el-secretario-example-get-current-val(car el-secretario--current-source-list))
            :to-equal 3)
    (el-secretario-next-item)
    (expect (el-secretario-example-get-current-val(car el-secretario--current-source-list))
            :to-equal 4))
  (it "can go to the previous source when there are no items left"
    (el-secretario-start-session (list (el-secretario-example-source
                                        :items-left '(1 2))
                                       (el-secretario-example-source
                                        :items-left '(3 4))))
    (expect (el-secretario-example-get-current-val (car el-secretario--current-source-list))
            :to-equal 1)
    (el-secretario-next-item)
    (expect (el-secretario-example-get-current-val (car el-secretario--current-source-list))
            :to-equal 2)
    (el-secretario-next-item)
    (expect (el-secretario-example-get-current-val(car el-secretario--current-source-list))
            :to-equal 3)
    (el-secretario-previous-item)
    (expect (el-secretario-example-get-current-val (car el-secretario--current-source-list))
            :to-equal 2)
    (el-secretario-next-item)
    (expect (el-secretario-example-get-current-val(car el-secretario--current-source-list))
            :to-equal 3)
    (el-secretario-next-item)
    (expect (el-secretario-example-get-current-val(car el-secretario--current-source-list))
            :to-equal 4)))

(describe "Org module"
  :var* (file source next-item-fun review-item-fun)


  (before-each
    (setq org-directory "/tmp/el-secretario")

    (test-el-secretario-reset-file el-secretario-org-buffer-s file)

    (setf (symbol-function 'next-item-fun) (lambda () ))
    (spy-on 'next-item-fun)

    (setf (symbol-function 'review-item-fun) (lambda () ))
    (spy-on 'review-item-fun)

    (setq source (list (el-secretario-org-make-source '(todo)
                                                      (list file)
                                                      :next-item-hook #'next-item-fun))))

  (it "can run a test!"
    (expect t :to-be t))

  (it "Starting a session only calls next-item once"
    (el-secretario-start-session source)
    (expect 'next-item-fun :to-have-been-called-times 1))


  (it "runs the next-item hook on each todo heading"
    (el-secretario-start-session source)
    (dotimes (_ 6)
      (el-secretario-next-item))
    (expect 'next-item-fun :to-have-been-called-times 7))

  (it "runs the review-item hook on each todo heading"
    (el-secretario-start-session (list (el-secretario-org-make-source '(todo)
                                                                      (list file)
                                                                      :next-item-hook #'next-item-fun
                                                                      :ids '("sub-task1"))))
    (dotimes (_ 7)
      (el-secretario-next-item))
    (expect 'next-item-fun :to-have-been-called-times 8)
    (expect 'review-item-fun :to-have-been-called-times 1))
  (it "can forwards and backwards"
    (el-secretario-start-session (list (el-secretario-org-make-source '(todo)
                                                                      (list file)
                                                                      :next-item-hook #'next-item-fun)))

    (el-secretario-next-item)
    (el-secretario-previous-item)
    (expect (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position))
            :to-equal "* TODO FOO :a:"))

  (it "should only call the next-item hook once on each todo heading"
    (el-secretario-start-session (list (el-secretario-org-make-source '(todo)
                                                                      (list file)
                                                                      :next-item-hook #'next-item-fun)))

    (el-secretario-next-item)
    (el-secretario-previous-item)
    (el-secretario-next-item)
    (expect 'next-item-fun :to-have-been-called-times 2))
  (it "can go backwards and forwards across sources"
    (el-secretario-start-session (list (el-secretario-org-make-source '(tags "a")
                                                                      (list file)
                                                                      :next-item-hook #'next-item-fun)
                                       (el-secretario-org-make-source '(tags "b")
                                                                      (list file)
                                                                      :next-item-hook #'next-item-fun)))

    (el-secretario-next-item)
    (el-secretario-next-item)
    (el-secretario-next-item)
    (el-secretario-next-item)
    (expect (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position))
            :to-equal "* TODO bar :b:")
    (el-secretario-previous-item)
    (expect (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position))
            :to-equal "** TODO subtask2")
    (el-secretario-next-item)
    (expect (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position))
            :to-equal "* TODO bar :b:")
    (expect 'next-item-fun :to-have-been-called-times 5) )

  (it "uses the directly provided ids"
    (el-secretario-start-session
     (list (el-secretario-org-make-source '(not (todo))
                                          (list file)
                                          :next-item-hook #'next-item-fun
                                          :ids '("sub-task1"
                                                 "sub-task2"))))
    (dotimes (_ 7)
      (el-secretario-next-item))
    (expect 'next-item-fun :to-have-been-called-times 2))
  (it "sorts the queried items"
    (el-secretario-start-session
     (list (el-secretario-org-make-source '(todo)
                                          (list file)
                                          :next-item-hook #'next-item-fun
                                          :compare-fun (lambda (x y)
                                                         (< (plist-get x :marker)
                                                            (plist-get y :marker)))
                                          :shuffle-p t)))

    (expect (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position))
            :to-equal "* TODO FOO :a:")
    (el-secretario-next-item)
    (expect (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position))
            :to-equal "* TODO bar :b:")
    (el-secretario-next-item)
    (expect (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position))
            :to-equal "* TODO baz :a:")
    (el-secretario-next-item)
    (expect (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position))
            :to-equal "** TODO subtask1")
    (el-secretario-next-item)
    (expect (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position))
            :to-equal "** TODO subtask2")
    (el-secretario-next-item)
    (expect (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position))
            :to-equal "* TODO Daily review :b:"))
  (it "can remove an item from the current source"

    (el-secretario-start-session (list (el-secretario-org-make-source '(todo)
                                                                      (list file)
                                                                      :next-item-hook #'next-item-fun)))

    (el-secretario-next-item)
    (expect (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position))
            :to-equal "* TODO bar :b:")
    (el-secretario-org-ignore-current-item)
    (expect (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position))
            :to-equal "* TODO baz :a:")
    (el-secretario-previous-item)
    (expect (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position))
            :to-equal "* TODO FOO :a:"))
  (describe "el-secretario-start-session with org module"

    (it "can reuse a function accepted as an argument"
      (let ((my-source (lambda ()
                         (el-secretario-org-make-source
                          '(todo)
                          (list file)
                          :next-item-hook #'next-item-fun
                          :ids '("sub-task1")))))

        (el-secretario-start-session my-source)

        (expect (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))
                :to-equal "** TODO subtask1")

        (dotimes (_ 6)
          (el-secretario-next-item))

        (with-current-buffer file
          ;; Invalidate org-ql's cache
          (set-buffer-modified-p t))

        (el-secretario-start-session my-source)

        (expect (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))
                :to-equal "** TODO subtask1")

        (dotimes (_ 6)
          (el-secretario-next-item))

        (expect 'next-item-fun :to-have-been-called-times 14)))))

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
                                                      :next-item-hook #'next-item-fun))))
  (describe "Subtasks"))

(describe "spaced repetition module"
  :var (file)
  (before-each

    (test-el-secretario-reset-file
     (concat
      "* TODO write a test
SCHEDULED: "
      (ts-format "<%Y-%m-%d %a>" (ts-dec 'day 3 (ts-now)))
      "
:PROPERTIES:
:EL-SECRETARIO-DELTA: 3
:END:
") file))
  (it "can increment the delta value"
    (with-current-buffer file
      (el-secretario-space--increment)
      (expect (string-to-number (org-entry-get (point) "EL-SECRETARIO-DELTA"))
              :to-equal
              4)))
  (it "can schedule a todo on the day `delta' days in the future"
    (with-current-buffer file
      (outline-next-heading)
      (el-secretario-space--increment)
      (el-secretario-space-reschedule)
      (let ((actual (->> (org-entry-get (point) "SCHEDULED")
                      (ts-parse)))
            (expected (->> (ts-now)
                        (ts-inc 'day 4))))
        (expect (ts-year actual)
                :to-be
                (ts-year expected))
        (expect (ts-day-of-year actual)
                :to-be
                (ts-day-of-year expected)))))

  (it "can reset the delta value to 2"
    (with-current-buffer file
      (outline-next-heading)
      (el-secretario-space--reset)
      (expect (string-to-number (org-entry-get (point) "EL-SECRETARIO-DELTA"))
              :to-equal
              2)))
  (it "can reset the delta value when the delta value reaches a cap"
    (test-el-secretario-reset-file
     (concat
      "* TODO write a test
:PROPERTIES:
:EL-SECRETARIO-DELTA: 1
:EL-SECRETARIO-DELTA-RESET-CAP: 4
:END:") file)

    (with-current-buffer file
      (outline-next-heading)
      (el-secretario-space--increment)
      (el-secretario-space--increment)
      (expect (string-to-number (org-entry-get (point) "EL-SECRETARIO-DELTA"))
              :to-equal
              3)

      (el-secretario-space--increment)
      (expect (string-to-number (org-entry-get (point) "EL-SECRETARIO-DELTA"))
              :to-equal
              2)))
  (it "can stop incrementing when it reaches a cap"
    (test-el-secretario-reset-file
     (concat
      "* TODO write a test
:PROPERTIES:
:EL-SECRETARIO-DELTA: 1
:EL-SECRETARIO-DELTA-CAP: 4
:END:") file)

    (with-current-buffer file
      (outline-next-heading)
      (el-secretario-space--increment)
      (expect (string-to-number (org-entry-get (point) "EL-SECRETARIO-DELTA"))
              :to-equal
              2)
      (el-secretario-space--increment)
      (el-secretario-space--increment)
      (el-secretario-space--increment)
      (el-secretario-space--increment)
      (el-secretario-space--increment)
      (el-secretario-space--increment)

      (el-secretario-space--increment)
      (expect (string-to-number (org-entry-get (point) "EL-SECRETARIO-DELTA"))
              :to-equal
              4)))
  (it "can sort according to scheduled time"
    (test-el-secretario-reset-file
     (concat
      "* TODO Third task
SCHEDULED: <2021-03-03>
* TODO Second task
SCHEDULED: <2021-02-02>

* TODO First task")
     file)
    (with-current-buffer file
      (goto-char (point-min))
      (let (x y)
        (setq x (el-secretario-org--parse-headline))
        (setq y (progn (outline-next-heading)
                       (el-secretario-org--parse-headline) ))
        (expect (el-secretario-space-compare-le x y)
                :to-be nil)
        (expect (el-secretario-space-compare-le y x)
                :to-be t)
        (setq x (el-secretario-org--parse-headline))
        (setq y (progn (outline-next-heading)
                      (el-secretario-org--parse-headline) ))
        (expect (el-secretario-space-compare-le x y)
                :to-be nil)
        (expect (el-secretario-space-compare-le y x)
                :to-be t)))

    (el-secretario-start-session
     (list (el-secretario-org-make-source '(todo)
                                          (list file)
                                          :next-item-hook #'next-item-fun
                                          :compare-fun #'el-secretario-space-compare-le
                                          :shuffle-p t)))
    (expect (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position))
            :to-equal "* TODO First task")
    (expect (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position))
            :not :to-equal "* TODO Second task")
    (el-secretario-next-item)
    (expect (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position))
            :to-equal "* TODO Second task")
    (el-secretario-next-item)
    (expect (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position))
            :to-equal "* TODO Third task")))
(describe "function module"
  :var* (next-item-fun)

  (before-each

    (setf (symbol-function 'next-item-fun) (lambda () ))
    (spy-on 'next-item-fun))

  (it "calls the function"
    (el-secretario-start-session (el-secretario-function-source
                                  :func #'next-item-fun))
    (expect #'next-item-fun :to-have-been-called-times 1))
  (it "can go to the next source"
    (el-secretario-start-session (list (el-secretario-function-source
                                        :func #'next-item-fun)
                                       (el-secretario-example-source
                                        :items-left '(1 2 3 4 5))) )
    (expect #'next-item-fun :to-have-been-called-times 1)
    (el-secretario-next-item)
    (expect (el-secretario-example-get-current-val (car el-secretario--current-source-list))
            :to-equal 1))
  (it "can go to the previous source"
    (el-secretario-start-session (list (el-secretario-example-source
                                        :items-left '(1))
                                       (el-secretario-function-source
                                        :func #'next-item-fun)) )
    (el-secretario-next-item)
    (el-secretario-previous-item)
    (expect #'next-item-fun :to-have-been-called-times 1)
    (expect (el-secretario-example-get-current-val (car el-secretario--current-source-list))
            :to-equal 1)))
(require 'el-secretario-example)

(describe "el-secretario-start-session"
    (it "can accepte a function as argument"
      (el-secretario-start-session (lambda ()
                                     (el-secretario-example-source
                                      :items-left '(1 2 3 4 5))))
      (expect (el-secretario-example-get-current-val (car el-secretario--current-source-list))
              :to-equal 1)
      (el-secretario-next-item)
      (expect (el-secretario-example-get-current-val (car el-secretario--current-source-list))
              :to-equal 2)
      (el-secretario-next-item)
      (expect (el-secretario-example-get-current-val(car el-secretario--current-source-list))
              :to-equal 3)
      (el-secretario-next-item)
      (expect (el-secretario-example-get-current-val(car el-secretario--current-source-list))
              :to-equal 4)
      (el-secretario-next-item)
      (expect (el-secretario-example-get-current-val(car el-secretario--current-source-list))
              :to-equal 5))
    (it "can reuse a function accepted as an argument"
      (let ((my-source (lambda ()
                         (el-secretario-example-source
                          :items-left '(1 2 3 4 5)))))
        (el-secretario-start-session my-source)
        (expect (el-secretario-example-get-current-val (car el-secretario--current-source-list))
                :to-equal 1)
        (el-secretario-next-item)
        (expect (el-secretario-example-get-current-val (car el-secretario--current-source-list))
                :to-equal 2)
        (el-secretario-next-item)
        (expect (el-secretario-example-get-current-val(car el-secretario--current-source-list))
                :to-equal 3)
        (el-secretario-start-session my-source)
        (expect (el-secretario-example-get-current-val(car el-secretario--current-source-list))
                :to-equal 1)
        (el-secretario-next-item)
        (expect (el-secretario-example-get-current-val(car el-secretario--current-source-list))
                :to-equal 2)
        (el-secretario-next-item)
        (expect (el-secretario-example-get-current-val(car el-secretario--current-source-list))
                :to-equal 3))))
(provide 'test-el-secretario)
;;; test-el-secretario.el ends here
