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
;; Homepage: https://github.com/Zetagon/el-secretario
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
(defhydra el-secretario-org-hydra ()
  ("n" el-secretario-next-item "next" :exit t)
  ("r" (progn (org-refile) (el-secretario-next-item)) "Refile" :exit t)
  ("t" org-set-tags-command "Tags")
  ("s" org-schedule "Schedule")
  ("d" org-deadline  "Deadline")
  ("D" (delete-region (point-min) (point-max)) "Delete visible")
  ("q" (switch-to-buffer el-secretario--original-buffer) "Quit" :exit t)
  ("/" nil "disable hydra"  :exit t))

(defmacro el-secretario-org-make-source (query files &optional next-item-hook hydra)
  "QUERY is an arbitrary org-ql query. FILES is the files to search through.
NEXT-ITEM-HOOk is called on each heading.
HYDRA is an hydra to use during review of this source."
  `(make-el-secretario-source
    :init-function  (lambda () (el-secretario-org-init (quote ,query) (quote ,files) ))
    :next-function  #'el-secretario-org-next-item
    :prev-function  #'el-secretario-org-previous-item
    :hydra-body #'el-secretario-org-hydra/body
    :finished-hook #'widen
    :next-item-hook (or ,next-item-hook (lambda ()))) )


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
        (org-narrow-to-subtree)
        (funcall (el-secretario-source-next-item-hook
                  (car el-secretario-current-source-list)))
        (funcall (el-secretario-source-hydra-body
                  (car el-secretario-current-source-list))) )
    (message "No next item!")
    (el-secretario--next-source)))

(defun el-secretario-org-previous-item ()
  "TODO"
  (pop el-secretario--org-items-done)
  (unless (car el-secretario--org-items-done)
    (cl-destructuring-bind (buf pos) (car el-secretario--org-items-done)
      (push (list buf pos) el-secretario--org-items-left)
      (widen)
      (set-window-buffer (selected-window) buf)
      (goto-char pos)
      (org-narrow-to-subtree)
      't)
    (message "No next item!")
    nil))

(defun el-secretario-org-add-tag (&rest tags)
  "Add TAGS to headline."
  (org-set-tags (cl-remove-duplicates
                 (append tags (or (org-get-tags nil 't)
                                  '()))
                 :test #'string-equal)))

(defun el-secretario-org-remove-tag (&rest tags)
  "Add TAGS to headline."
  (org-set-tags (--keep (string-equal tags it) (org-get-tags nil 't))))

(provide 'el-secretario-org)
;;; el-secretario-org.el ends here
