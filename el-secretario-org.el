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

(defclass el-secretario-org-source (el-secretario-source)
  ((query :initarg :query)
   (files :initarg :files)
   (compare-fun :initarg :compare-fun)
   (shuffle-p :initarg :shuffle-p)
   (:next-item-hook :initarg :next-item-hook)
   (ids :initarg :ids)
   (items-left :initform '())
   (items-done :initform '())))


(cl-defun el-secretario-org-make-source (query files &key next-item-hook compare-fun hydra shuffle-p ids)
  "

QUERY is an arbitrary org-ql query.

FILES is the files to search through.

NEXT-ITEM-HOOk is called on each heading.

HYDRA is an hydra to use during review of this source.

IDS is a list of IDs of elements that should be added to the list
of queried items.

If SHUFFLE-P is non-nil, shuffle the list of queried items before
reviewing.

If COMPARE-FUN is non-nil, sort the list of queried items using
that function. Sorting happens after shuffling if SHUFFLE-P is
non-nil. COMPARE-FUN should take two arguments which are returned
by `el-secretario-org--parse-headline' See
`el-secretario-space-compare-le' for an example sorting
function."
  (el-secretario-org-source
   :query query
   :files files
   :compare-fun compare-fun
   :shuffle-p shuffle-p
   :next-item-hook next-item-hook
   :ids ids
   :hydra (or hydra #'el-secretario-org-hydra/body)))


(cl-defmethod el-secretario-init ((obj el-secretario-org-source))
  "TODO"
  (with-slots (query files compare-fun shuffle-p ids hydra items-left items-done) obj
      (setq items-left
            (append (-map (lambda (id)
                            (let ((m (org-id-find id 'marker)))
                              (when m
                                (with-current-buffer (marker-buffer m)
                                  (save-excursion
                                    (goto-char m)
                                    (el-secretario-org--parse-headline))))))
                          ids)
                    (org-ql-select (or files
                                       (org-agenda-files)) query
                                       :action #'el-secretario-org--parse-headline)))
    (when shuffle-p
      (el-secretario--shuffle items-left))
    (when compare-fun
      (setq items-left (sort items-left compare-fun)))
    (setq items-done nil)
    (el-secretario-activate-hydra)
    (el-secretario-source-next-item obj)))


(cl-defmethod el-secretario-source-next-item ((obj el-secretario-org-source))
  "TODO"
  (with-slots (items-left items-done) obj
    (if-let ((item (pop items-left)))
        (let ((buf (plist-get item :buffer ))
              (pos (plist-get item :marker)))
          (outline-show-all)
          (push (list buf pos) items-done)
          (switch-to-buffer buf)
          (widen)
          (goto-char pos)
          (el-secretario-org-narrow)
          (funcall (oref obj :next-item-hook))

          (el-secretario-org-update-status-buffer)
          (el-secretario-activate-hydra)
          (el-secretario-tasks--run-task-hook
           (el-secretario-org--parse-headline)
           :EL-SECRETARIO-REVIEW-TASK-HOOK))
      (message "No next item!")
      (el-secretario--next-source))))

(defvar date nil)
(defun el-secretario-org-update-status-buffer ()
  "Update the status buffer with useful information.
That information is the currently visible schedule dates and deadlines."
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
        (insert "Scheduled: " it "\n"))))
  (when-let ((win (get-buffer-window
                 (get-buffer-create el-secretario-status-buffer-name))))
      (with-selected-window win
        (fit-window-to-buffer))))

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

(defun el-secretario-org--parse-headline ()
  "Parse headline at point and put in some more relevant information.
This is like `org-element-headline-parser' but with some extra properties put in.
"
  (--> (org-element-headline-parser (line-end-position))
    (nth 1 it)
    (plist-put it :file-name (buffer-file-name))
    (plist-put it :buffer (current-buffer))
    (plist-put it :marker (point-marker))
    (plist-put it :EL-SECRETARIO-PRIORITY
               (or (-some-> (plist-get
                             it
                             :EL-SECRETARIO-PRIORITY)
                     (string-to-number))
                   1))))

(provide 'el-secretario-org)
;;; el-secretario-org.el ends here
