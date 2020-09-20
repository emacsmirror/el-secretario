;;; all-reviewer-org.el org-mode module for all-reviewer -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Leo
;;
;; Author: Leo <http://github/leo>
;; Maintainer: Leo <leo@leo-B85-HD3>
;; Created: September 20, 2020
;; Modified: September 20, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/leo/all-reviewer-org
;; Package-Requires: ((emacs 26.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'org-ql)

(defvar all-reviewer--org-items-left nil
  "A list of items that should be reviewed")

(defvar all-reviewer--org-items-done nil
  "A list of items that has been reviewed")


(defun all-reviewer-org-init (query)
  "TODO"
  (setq all-reviewer--org-items-left
        (cons nil
              (org-ql-select "/tmp/test.org" '(todo)
                :action '(list (current-buffer)
                               (point-marker)))))
  (setq all-reviewer--org-items-done nil))

(defun all-reviewer-org-next-item ()
  "TODO"
  (pop all-reviewer--org-items-left)
  (if (car all-reviewer--org-items-left)
    (cl-destructuring-bind (buf pos) (car all-reviewer--org-items-left)
      (push (list buf pos) all-reviewer--org-items-done)
      (widen)
      (set-window-buffer (selected-window) buf)
      (goto-char pos)
      (org-narrow-to-subtree)
      't)
    (message "No next item!")
    nil))

(defun all-reviewer-org-previous-item ()
  "TODO"
  (pop all-reviewer--org-items-done)
  (unless (car all-reviewer--org-items-done)
    (cl-destructuring-bind (buf pos) (car all-reviewer--org-items-done)
      (push (list buf pos) all-reviewer--org-items-left)
      (widen)
      (set-window-buffer (selected-window) buf)
      (goto-char pos)
      (org-narrow-to-subtree)
      't)
    (message "No next item!")
    nil))

(defun all-reviewer-org-add-tag (&rest tags)
  "Add TAGS to headline."
  (org-set-tags (append tags (or (org-get-tags nil 't)
                              '()))))

(defun all-reviewer-org-remove-tag (&rest tags)
  "Add TAGS to headline."
  (org-set-tags (--keep (string-equal tags it) (org-get-tags nil 't))))

(provide 'all-reviewer-org)
;;; all-reviewer-org.el ends here
