;;; el-secretario-org.el org-mode module for el-secretario -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Leo
;;
;; Author: Leo Okawa Ericson <http://github/Zetagon>
;; Maintainer: Leo <github@relevant-information.com>
;; Created: September 20, 2020
;; Modified: October 17, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://git.sr.ht/~zetagon/el-secretario
;; Package-Requires: ((emacs 26.1) (cl-lib "0.5") (hydra "0.15.0")(org-ql "0.6-pre"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'org-ql)

(defhydra el-secretario-org-hydra ()
  ("n" el-secretario-next-item "next" :exit t)
  ("r" (progn (org-refile) (el-secretario-next-item)) "Refile" :exit t)
  ("t" org-set-tags-command "Tags")
  ("T" org-todo"Tags")
  ("s" org-schedule "Schedule")
  ("d" org-deadline  "Deadline")
  ("D" (delete-region (point-min) (point-max)) "Delete visible")
  ("q" (el-secretario-end-sesion) "Quit" :exit t)
  ("/" nil "disable hydra"  :exit t))


(defvar el-secretario-org-narrow-function #'org-narrow-to-subtree
  "Function to use for narrowing when goint to the next item.
For example `el-secretario-org-narrow-to-highest-todo' might be
useful for when you have defined projects to be a todo entry with
subtrees that are also todos. It can then be useful to see the context when reviewing.")
(defun el-secretario-org-narrow ()
  (funcall el-secretario-org-narrow-function))

(defun el-secretario-org-narrow-to-highest-todo ()
  "Narrow to highest grandparent heading that is a todo."
  (save-excursion
    (let ((closest-todo-upwards (point)))
      (while (el-secretario-org-up-heading 1)
        ;; TODO Error recovery if there is no parent heading with project todo state
        (when (org-get-todo-state)
          (setq closest-todo-upwards (point))))
      (goto-char closest-todo-upwards)
      (org-narrow-to-subtree)
      (outline-hide-leaves)))
  (outline-show-entry))

(defun el-secretario-org-make-source (query files &optional next-item-hook hydra)
  "QUERY is an arbitrary org-ql query. FILES is the files to search through.
NEXT-ITEM-HOOk is called on each heading.
HYDRA is an hydra to use during review of this source."
  (make-el-secretario-source
   :init-function  (lambda () (el-secretario-org-init query files))
   :next-function  #'el-secretario-org-next-item
   :prev-function  #'el-secretario-org-previous-item
   :hydra-body (or hydra #'el-secretario-org-hydra/body)
   :finished-hook #'widen
   :next-item-hook (or next-item-hook (lambda ()))) )


(defvar el-secretario--org-items-left nil
  "A list of items that should be reviewed")

(defvar el-secretario--org-items-done nil
  "A list of items that has been reviewed")


(defun el-secretario-org-init (query &optional files)
  "TODO"
  (setq el-secretario--org-items-left
        (org-ql-select (or files
                           (org-agenda-files)) query
                           :action '(list (current-buffer)
                                          (point-marker))))
  (setq el-secretario--org-items-done nil)
  (funcall (el-secretario-source-hydra-body
            (car el-secretario-current-source-list)))
  (el-secretario-org-next-item))

(defun el-secretario-org-next-item ()
  "TODO"

  (if-let ((item (pop el-secretario--org-items-left)))
      (cl-destructuring-bind (buf pos) item
        (push (list buf pos) el-secretario--org-items-done)
        (switch-to-buffer buf)
        (widen)
        (goto-char pos)
        (el-secretario-org-narrow)

        (funcall (el-secretario-source-next-item-hook
                  (car el-secretario-current-source-list)))
        (el-secretario-org-update-status-buffer)
        (funcall (el-secretario-source-hydra-body
                  (car el-secretario-current-source-list)))
        (el-secretario-tasks--run-task-hook
                             (el-secretario-tasks--parse-headline)
                             :EL-SECRETARIO-REVIEW-TASK-HOOK))
    (message "No next item!")
    (el-secretario--next-source)))

(defvar date nil)
(defun el-secretario-org-update-status-buffer ()
  "Update the status buffer with useful information.
That information is the currently visible schedule dates and deadlines "
  (interactive)
  (let ((date (calendar-current-date))
        deadlines
        scheduleds)
    (save-excursion
      (setq deadlines (org-agenda-get-deadlines))
      (setq scheduleds (org-agenda-get-scheduled)))
    (with-current-buffer (get-buffer-create el-secretario-status-buffer-name)
      (delete-region (point-min) (point-max))
      (--each deadlines
        (insert "Deadline: " it "\n"))
      (--each scheduleds
        (insert "Scheduled: " it "\n")))))

(defun el-secretario-org-previous-item ()
  "TODO"
  (pop el-secretario--org-items-done)
  (unless (car el-secretario--org-items-done)
    (cl-destructuring-bind (buf pos) (car el-secretario--org-items-done)
      (push (list buf pos) el-secretario--org-items-left)
      (widen)
      (set-window-buffer (selected-window) buf)
      (goto-char pos)
      (el-secretario-org-narrow)
      't)
    (message "No next item!")
    nil))

(defun el-secretario-org-add-tag (&rest tags)
  "Add TAGS to current headline."
  (org-set-tags (cl-remove-duplicates
                 (append tags (or (org-get-tags nil 't)
                                  '()))
                 :test #'string-equal)))

(defun el-secretario-org-remove-tag (&rest tags)
  "Remove TAGS from current headline."
  (org-set-tags (--filter
                 (let ((-compare-fn #'string-equal))
                   (not (-contains? tags it)))
                   (org-get-tags nil 't))))

(defun el-secretario-org-up-heading (arg)
  "Call `outline-up-heading' but return position if succeeds and nil otherwise"
  (condition-case nil
      (progn
        (outline-up-heading arg)
        (point))
    (error nil)))

(provide 'el-secretario-org)
;;; el-secretario-org.el ends here
