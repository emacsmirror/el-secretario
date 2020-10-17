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

(defun el-secretario--notmuch-init ()
  (notmuch-search "tag:test" ;; "tag:notdone AND NOT tag:deleted NOT tag:gmail/Inbox"
                  't
                  nil
                  0
                  nil)
  (notmuch-search-first-thread)
  (sit-for 0.2)
  (el-secretario--notmuch-search-show-thread))

(defun el-secretario--notmuch-show-next-thread (&optional previous)
  "Move to the next item in the search results, if any.

  If PREVIOUS is non-nil, move to the previous item in the
search results instead."
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
  "Display the currently selected thread.

With a prefix argument, invert the default value of
`notmuch-show-only-matching-messages' when displaying the
thread."
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
