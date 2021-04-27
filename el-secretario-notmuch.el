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

(defun el-secretario-notmuch-make-source (query &optional next-item-hook hydra)
  "Convenience macro for creating a source for notmuch mail.
QUERY is a normal notmuch query.
NEXT-ITEM-HOOk is called on each heading.
HYDRA is an hydra to use during review of this source"
  (make-el-secretario-source
    :init-function  (lambda () (el-secretario--notmuch-init query))
    :next-function  #'el-secretario--notmuch-show-next-thread
    :prev-function  #'notmuch-show-previous-thread-show
    :hydra-body (or hydra #'el-secretario-default-hydra/body)
    :finished-hook (lambda ())
    :next-item-hook (or next-item-hook (lambda ()))) )

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
                                     "*"))
               (funcall (el-secretario-source-hydra-body
                         (car el-secretario-current-source-list)))
               (el-secretario-notmuch--open-link-for-current-email))
      (message "End of search results.")
      (el-secretario--next-source))))

(defun el-secretario-notmuch-reverse-capture (file)
  "Capture a back-linked entry to FILE.
When you review an email in a thread that you have captured to
with this function you will see that capture in the status
window.

You can use your own template by calling `el-secretario-notmuch--reverse-capture' yourself.
 "
  (el-secretario-notmuch--reverse-capture file (concat "\n* WAITING for email: [[notmuch:id:" message-id "][Email: " subject "]]"
                                  "\n:PROPERTIES:"
                                  "\n:ID: " notmuch-show-thread-id
                                  "\n:From: \"" from
                                  "\"\n:END:\n")))

(defmacro el-secretario-notmuch--reverse-capture (file template)
  "Capture a back-linked entry to FILE using TEMPLATE.
See `el-secretario-notmuch-reverse-capture' for more details.

TEMPLATE should be a s-exp that returns the entry you want to
capture. The template is expanded in the buffer this is
called (which should be the notmuch-show buffer you want to
capture). The following special variables are recognized:

- message-id :: The id of the capture message
- subject :: The subject line
- to :: The to: adress
- from :: The from: adress
- date :: The date the email was sent

NOTE that the entry MUST have the :ID: property set to
'notmuch-show-thread-id'. See
`el-secretario-notmuch-reverse-capture' for an example.

"
  `(let* ((message-id (notmuch-show-get-message-id t))
          (subject (notmuch-show-get-subject))
          (to (notmuch-show-get-to))
          (from (notmuch-show-get-from))
          (date (org-trim (notmuch-show-get-date)))

          (s ,template ))
     (with-current-buffer (find-file-noselect ,file)
       (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-max))
           (insert s)
           (save-buffer))))))

(defun el-secretario-notmuch--open-link-for-current-email ()
  (let* ((location (org-id-find notmuch-show-thread-id))
         (buf (find-file-noselect (car location))))
    (display-buffer-in-side-window buf
                                   '((side . top)))
    (with-current-buffer buf
      (goto-char (cdr location))
      (org-narrow-to-subtree))))
(provide 'el-secretario-notmuch)
;;; el-secretario-notmuch.el ends here
