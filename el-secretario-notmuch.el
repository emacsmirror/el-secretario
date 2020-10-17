;;; el-secretario-notmuch.el Notmuch implementation for el-secretario -*- lexical-binding: t; -*-
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

(defmacro el-secretario-notmuch-make-source (query &optional next-item-hook)
   :hydra-body #'el-secretario-default-hydra/body
  "Convenience macro for creating a source for notmuch mail.
QUERY is a normal notmuch query.
NEXT-ITEM-HOOk is called on each heading.
HYDRA is an hydra to use during review of this source"
  `(make-el-secretario-source
    :init-function  (lambda () (el-secretario--notmuch-init  ,query))
    :next-function  #'el-secretario--notmuch-show-next-thread
    :prev-function  #'notmuch-show-previous-thread-show
    :hydra-body #'el-secretario-default-hydra/body
    :finished-hook (lambda ())
    :next-item-hook (or ,next-item-hook (lambda ()))) )

(defun el-secretario--notmuch-init (&optional query)
  (notmuch-search (or query "tag:unread")
                  't
                  nil
                  0
                  nil)
  (notmuch-search-first-thread)
  (sit-for 0.1)
  (el-secretario--notmuch-search-show-thread)
  (funcall (el-secretario-source-hydra-body
            (car el-secretario-current-source-list))))

(defun el-secretario--notmuch-show-next-thread (&optional previous)
  "Like `notmuch-show-next-thread' but call `el-secretario--notmuch-search-show-thread' instead"
  (interactive "P")
  (let ((parent-buffer notmuch-show-parent-buffer))
    (notmuch-bury-or-kill-this-buffer)
    (when (buffer-live-p parent-buffer)
      (switch-to-buffer parent-buffer)
      (and (if previous
	       (notmuch-search-previous-thread)
	     (notmuch-search-next-thread))
	   (el-secretario--notmuch-search-show-thread)))))


(defun el-secretario--notmuch-search-show-thread (&optional elide-toggle)
  "Like `notmuch-search-show-thread' but call `el-secretario--next-source' if there are no more mail."
  (interactive "P")
  (let ((thread-id (notmuch-search-find-thread-id))
        (subject (notmuch-search-find-subject)))
    (if (> (length thread-id) 0)
        (progn (notmuch-show thread-id
                             elide-toggle
                             (current-buffer)
                             notmuch-search-query-string
                             ;; Name the buffer based on the subject.
                             (concat "*"
                                     (truncate-string-to-width subject 30 nil nil t)
                                     "*")))
      (message "End of search results.")
      (el-secretario--next-source))))
(provide 'el-secretario-notmuch)
;;; el-secretario-notmuch.el ends here
