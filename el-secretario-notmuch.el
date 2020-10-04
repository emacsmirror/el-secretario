;;; el-secretario-notmuch.el Notmuch implementation for el-secretario -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Leo
;;
;; Author: Leo <http://github/leo>
;; Maintainer: Leo <leo@leo-B85-HD3>
;; Created: September 27, 2020
;; Modified: September 27, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/leo/el-secretario-notmuch
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
  (notmuch-search "tag:notdone AND NOT tag:deleted NOT tag:gmail/Inbox"
                  't
                  nil
                  0
                  nil)
  ;; (notmuch-search-first-thread)
  ;; (funcall-interactively 'notmuch-search-show-thread)
  )

(defun el-secretario--notmuch-next-item (&optional elide-toggle)
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
			             "*"))
               't)
      (message "End of search results.")
      nil)))
(provide 'el-secretario-notmuch)
;;; el-secretario-notmuch.el ends here
