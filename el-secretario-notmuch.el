;;; el-secretario-notmuch.el --- Notmuch implementation for el-secretario -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Leo
;;
;; Author: Leo Okawa Ericson <http://github/Zetagon>
;; Maintainer: Leo <github@relevant-information.com>
;; Created: September 20, 2020
;; Modified: October 17, 2020
;; Version: 0.0.1
;; Keywords: convenience mail
;; Homepage: https://git.sr.ht/~zetagon/el-secretario
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; An notmuch source for el-secretario
;;
;;; Code:
(require 'el-secretario-source)
(require 'notmuch)

(defclass el-secretario-notmuch-source (el-secretario-source)
  ((query :initarg :query)))

;;;###autoload
(defun el-secretario-notmuch-make-source (query &optional keymap)
  "Convenience macro for creating a source for notmuch mail.
QUERY is a normal notmuch query.
KEYMAP is a keymap to use during review of this source"
  (el-secretario-notmuch-source
   :keymap (or keymap 'el-secretario-source-default-map)
   :query query))

(cl-defmethod el-secretario-source-activate ((obj el-secretario-notmuch-source) &optional backwards)
  (with-slots (query) obj
    (notmuch-search (or query "tag:unread")
                    t
                    nil
                    0
                    nil)
    (sit-for 0.1)
    (if backwards
        (notmuch-search-last-thread)
      (notmuch-search-first-thread))
    (el-secretario-notmuch--search-show-thread)
    (el-secretario/activate-keymap)))

(cl-defmethod el-secretario-source-next-item ((_obj el-secretario-notmuch-source))
  (el-secretario-notmuch-show-next-thread))
(cl-defmethod el-secretario-source-previous-item ((_obj el-secretario-notmuch-source))
  (el-secretario-notmuch-show-next-thread t))

;;
;; The logic for detecting when to call next-source or previous-source is quite
;; unintuitive. The text below is the content after calling `notmuch-search'.
;; The cursor is on the first line. `(el-secretario-notmuch--notmuch-show-next-thread t)' will try to
;; go up and fail because `(notmuch-search-previous-thread)' tries to up one
;; line and fails. In that case it returns nil and we should call
;; `el-secretario--previous-source'.
;;
;; The other case is when the cursor is on the second line.
;; `(el-secretario-notmuch--notmuch-show-next-thread)' will succed and place the cursor on the third
;; line. There `el-secretario-notmuch--search-show-thread' will fail because it
;; can't get a thread-id because it isn't on a line with a thread. In that case
;; we call `el-secretario--next-source'
;;
;; 2020-02-13 [1/1]   Sender foo    Subject a           (tagA)
;; 2020-06-08 [1/1]   Sender bar    Subject b           (tagB)
;; End of search results.

(defun el-secretario-notmuch-show-next-thread (&optional previous)
  "Like `notmuch-show-next-thread' but call `el-secretario-notmuch--search-show-thread' instead.

If PREVIOUS is non-nil, move to the previous item in the search
results instead."
  (interactive "P")
  (let ((parent-buffer notmuch-show-parent-buffer))
    (notmuch-bury-or-kill-this-buffer)
    (when (buffer-live-p parent-buffer)
      (switch-to-buffer parent-buffer)
      (and (if previous
	       (if (notmuch-search-previous-thread)
                   t
                 (el-secretario--previous-source)
                 nil)
	     (notmuch-search-next-thread))
	   (el-secretario-notmuch--search-show-thread previous)))))


(defun el-secretario-notmuch--search-show-thread (&optional elide-toggle)
  "Wrapper-function around `notmuch-search-show-thread'.

Like `notmuch-search-show-thread' but call
`el-secretario--next-source' if there are no more mail.
Pass ELIDE-TOGGLE to `notmuch-search-show-thread'."
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
                                     (truncate-string-to-width
                                      subject 30 nil nil t)
                                     "*"))
               (el-secretario/activate-keymap)
               (el-secretario-notmuch--open-link-for-current-email))
      (message "End of search results.")
      (el-secretario--next-source))))

(defun el-secretario-notmuch-reverse-capture (file)
  "Capture a back-linked entry to FILE.
When you review an email in a thread that you have captured to
with this function you will see that capture in the status
window.

You can use your own template by calling `el-secretario-notmuch--reverse-capture' yourself."
  (el-secretario-notmuch--reverse-capture
   file
   (concat "\n* WAITING for email: [[notmuch:id:" message-id "][Email: "
           subject "]]"
           "\n:PROPERTIES:"
           "\n:ID: " notmuch-show-thread-id
           "\n:From: \"" from
           "\"\n:END:\n")))


(defun el-secretario-notmuch-capture-get-thread-link ()
  "Get a link to the thread of the captured email.

To be used in a capture template."
  (with-current-buffer (org-capture-get :original-buffer)
    (concat "[[notmuch:" notmuch-show-thread-id "][Thread]]")))

(defun el-secretario-notmuch/open-link-for-current-email ()
  (interactive)
  (el-secretario--status-buffer-activate)
  (el-secretario-notmuch--open-link-for-current-email))

(defun el-secretario-notmuch--open-link-for-current-email ()
  (let ((id notmuch-show-thread-id))
    (let ((entries (org-ql-select '("~/org/orgzly/InboxComputer.org") `(link ,id)
                     :action 'element-with-markers))
          (prev-mesg (when (notmuch-show-goto-message-previous)
                       (let ((x (org-ql-select '("~/org/orgzly/InboxComputer.org")
                                  `(link ,(notmuch-show-get-message-id))
                                  :action 'element-with-markers)))
                         (notmuch-show-goto-message-next)
                         x))))

      (dolist (x prev-mesg)
        (with-current-buffer (get-buffer-create el-secretario--status-buffer-name)
          (insert "\nPrevious message:\n" (org-ql-view--format-element x))))
      (dolist (x entries)
        (with-current-buffer (get-buffer-create el-secretario--status-buffer-name)
          (org-agenda-mode)
          (delete-region (point-min)
                         (point-max))
          (insert "Same thread:\n" (org-ql-view--format-element
                                    x))))
      ;; TODO Abstract this into common status-buffer logic
      (when-let ((win (get-buffer-window
                       (get-buffer-create el-secretario--status-buffer-name))))
        (with-selected-window win
          (fit-window-to-buffer))) )))


(provide 'el-secretario-notmuch)
;;; el-secretario-notmuch.el ends here
