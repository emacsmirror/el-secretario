;;; el-secretario-message.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Leo
;;
;; Author: Leo Okawa Ericson <http://github/Zetagon>
;; Maintainer: Leo <github@relevant-information.com>
;; Created: January 21, 2021
;; Modified: January 21, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://git.sr.ht/~zetagon/el-secretario
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5") (hydra "0.15.0")(org-ql "0.6-pre"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:


(defvar el-secretario-message-pre-message-marker nil
  "The marker to return to before a message has been displayed")

(defvar el-secretario-message-hydra nil
  "The hydra to be used for current message")

(defvar el-secretario-message-buffer-name "*el-secretario message*")

(defun el-secretario-message--display-message-prompt (message &optional hydra)
  "Display MESSAGE in a dedicated buffer.
If HYDRA is nil, use `el-secretario-message-message-hydra'.

If HYDRA is non-nil, use that as prompt. Keep in mind that it
should probably have one head that calls
`el-secretario-message--back-to-pre-message'"
  (setq el-secretario-message-pre-message-marker (point-marker))
  (switch-to-buffer (get-buffer-create el-secretario-message-buffer-name))
  (delete-region (point-min) (point-max))
  (insert message)
  (setq el-secretario-message-hydra (or hydra #'el-secretario-message-message-hydra/body))
  (funcall el-secretario-message-hydra))

(defun el-secretario-message--back-to-pre-message ()
  "Quit from the message and reset state."
  (interactive)
  (switch-to-buffer (marker-buffer el-secretario-message-pre-message-marker))
  (goto-char (marker-position el-secretario-message-pre-message-marker))
  (setq el-secretario-message-hydra nil))

(defhydra el-secretario-message-message-hydra nil
  "Default hydra for exiting a message.
Only one head which is for removing the message buffer."
  ("q" el-secretario-message--back-to-pre-message :exit t))

(defmacro el-secretario-message--with-pre-buffer (&rest body)
  `(if (string-equal (buffer-name)
                     el-secretario-message-buffer-name)
       (with-current-buffer (marker-buffer el-secretario-message-pre-message-marker)
         (save-excursion
           (goto-char (marker-position el-secretario-message-pre-message-marker))

           ,@body))
     ,@body))

(provide 'el-secretario-message)
;;; el-secretario-message.el ends here
