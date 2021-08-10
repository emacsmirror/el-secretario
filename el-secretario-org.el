;;; el-secretario-org.el --- Org mode module for el-secretario -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Leo
;;
;; Author: Leo Okawa Ericson <http://github/Zetagon>
;; Maintainer: Leo <github@relevant-information.com>
;; Created: September 20, 2020
;; Modified: October 17, 2020
;; Version: 0.0.1
;; Keywords: convenience
;; Homepage: https://git.sr.ht/~zetagon/el-secretario
;; Package-Requires: ((emacs "27.1")  (org-ql "0.6-pre") (dash "2.18.1") (el-secretario "0.0.1"))
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file LICENSE.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; The org source for el-secretario.
;;
;;; Code:

(require 'org-ql)
(require 'el-secretario-source)
(require 'el-secretario-tasks)
(require 'dash)


(defvar el-secretario-org-keymap (make-sparse-keymap))


(defvar el-secretario-org-narrow-function #'org-narrow-to-subtree
  "Function to use for narrowing when goint to the next item.
For example `el-secretario-org-narrow-to-highest-todo' might be
useful for when you have defined projects to be a todo entry with
subtrees that are also todos. It can then be useful to see the context when reviewing.")
(defun el-secretario-org-narrow ()
  "Function to use for narrowing to the current item."
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
   (current-item :initform nil)
   (items-left :initform '())
   (items-done :initform '())))


;;;###autoload
(cl-defun el-secretario-org-make-source (query files &key next-item-hook compare-fun keymap shuffle-p ids keymap)
  "\
QUERY is an arbitrary org-ql query.

FILES is the files to search through.

NEXT-ITEM-HOOK is called on each heading.

KEYMAP is an keymap to use during review of this source.

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
   :keymap (or keymap #'el-secretario-org-keymap)))

(cl-defmethod el-secretario-source-activate ((obj el-secretario-org-source) &optional backwards)
  (el-secretario-activate-keymap)
  (el-secretario-source-activate-item obj))


(cl-defmethod el-secretario-source-init ((obj el-secretario-org-source) &optional backwards)
  "TODO"
  (with-slots (query files compare-fun shuffle-p ids items-left items-done) obj
    (dolist (f files)
      (if (bufferp f)
          (with-current-buffer f
            (widen))
        (with-current-buffer (find-file-noselect f)
          (widen))))
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
    (setq items-done nil))
  (el-secretario-activate-keymap)
  (el-secretario-source-next-item obj))

(cl-defmethod el-secretario-source-activate-item ((obj el-secretario-org-source))
  (with-slots (current-item) obj

    (let ((buf (plist-get current-item :buffer ))
          (pos (plist-get current-item :marker)))
      (outline-show-all)
      (switch-to-buffer buf)
      (widen)
      (goto-char pos)
      (el-secretario-org-narrow)
      (unless (plist-get current-item :called-next-item-hook)
        (when-let ((hook (oref obj :next-item-hook)))
          (funcall hook)))
      (setq current-item (plist-put current-item :called-next-item-hook t))

      (el-secretario-org--update-status-buffer)
      (el-secretario-activate-keymap)
      (el-secretario-tasks--run-task-hook
       (el-secretario-org--parse-headline)
       :EL-SECRETARIO-REVIEW-TASK-HOOK))))

(cl-defmethod el-secretario-source-next-item ((obj el-secretario-org-source))
  "TODO"
  (with-slots (items-left items-done current-item) obj
    (if-let ((item (pop items-left)))
        (progn
          (when current-item
            (push current-item items-done))
          (setq current-item item)
          (el-secretario-source-activate-item obj))

      (message "No next item!")
      (el-secretario--next-source))))

(cl-defmethod el-secretario-source-previous-item ((obj el-secretario-org-source))
  "TODO"
  (with-slots (items-left items-done current-item) obj
    (if-let ((item (pop items-done)))
        (progn
          (when current-item
            (push current-item items-left))
          (setq current-item item)
          (el-secretario-source-activate-item obj))
      (message "No previous item!")
      (el-secretario--previous-source))))

(defun el-secretario-org-ignore-current-item ()
  "Remove the current item from this session."
  (interactive)
  (when el-secretario--current-source-list
    (with-slots (current-item) (car el-secretario--current-source-list)
      (setq current-item nil)
      (el-secretario-next-item))))

(defvar date)
(defun el-secretario-org--update-status-buffer ()
  "Update the status buffer with useful information.
That information is the currently visible schedule dates and deadlines."
  (interactive)
  (let ((date (calendar-current-date))
        deadlines
        scheduleds)
    (save-excursion
      (setq deadlines (org-agenda-get-deadlines))
      (setq scheduleds (org-agenda-get-scheduled)))
    (with-current-buffer (get-buffer-create el-secretario--status-buffer-name)
      (delete-region (point-min) (point-max))
      (--each deadlines
        (insert "Deadline: " it "\n"))
      (--each scheduleds
        (insert "Scheduled: " it "\n"))))
  (when-let ((win (get-buffer-window
                 (get-buffer-create el-secretario--status-buffer-name))))
      (with-selected-window win
        (fit-window-to-buffer))))



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
  "Call `outline-up-heading' but return position if succeeds and nil otherwise.

Pass ARG to `outline-up-heading'."
  (condition-case nil
      (progn
        (outline-up-heading arg)
        (point))
    (error nil)))

(defun el-secretario-org--parse-headline ()
  "Parse headline at point and put in some more relevant information.

This is like `org-element-headline-parser' but with some extra
properties put in."
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
