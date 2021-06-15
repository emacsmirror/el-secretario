;;; el-secretario-elfeed.el Elfeed implementation for el-secretario -*- lexical-binding: t; -*-
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
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5") (org-ql "0.6-pre"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(require 'el-secretario-source)

(defclass el-secretario-elfeed-source (el-secretario-source)
  ((query :initarg :query)))
(defvar el-secretario-elfeed-map (make-sparse-keymap))

;;;###autoload
(defun el-secretario-elfeed-make-source (&optional query keymap)
  "Convenience macro for creating a source for elfeed.
QUERY is a normal elfeed query.
KEYMAP is a keymap to use during review of this source"
  (el-secretario-elfeed-source
   :keymap (or keymap 'el-secretario-elfeed-map)
   :query query))

(cl-defmethod el-secretario-source-activate ((obj el-secretario-elfeed-source) &optional backwards)
  (with-slots (query) obj
    (setq elfeed-show-entry-delete 'elfeed-kill-buffer)
    (setq elfeed-show-entry-switch (lambda (x) (switch-to-buffer x nil t)))
    (when query
      (setq elfeed-search-filter query))

    (elfeed)

    (setq elfeed-sort-order 'ascending)
    (sit-for 0.1)
    (if backwards
        (progn
          (goto-char (point-max))
          (forward-line -1))
      (goto-char (point-min)))
    (call-interactively #'elfeed-search-show-entry)
    (el-secretario/activate-keymap)))

(cl-defmethod el-secretario-source-next-item ((obj el-secretario-elfeed-source))
  (el-secretario-elfeed--show-next))

(defun el-secretario-elfeed--show-next ()
  "Show the next item in the elfeed-search buffer."
  (interactive)
  (funcall elfeed-show-entry-delete)
  (with-current-buffer (elfeed-search-buffer)
    (when elfeed-search-remain-on-entry (forward-line 1))
    (if (string= "" (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
        (el-secretario--next-source)
      (call-interactively #'elfeed-search-show-entry))))

(cl-defmethod el-secretario-source-previous-item ((obj el-secretario-elfeed-source))
  (el-secretario-elfeed--show-prev))

(defun el-secretario-elfeed--show-prev ()
  "Show the previous item in the elfeed-search buffer."
  (funcall elfeed-show-entry-delete)
  (with-current-buffer (elfeed-search-buffer)
    (when elfeed-search-remain-on-entry (forward-line 1))
    (if (= (forward-line -2)
           -1)
        (el-secretario--previous-source))
    (call-interactively #'elfeed-search-show-entry)))

(provide 'el-secretario-elfeed)
;;; el-secretario-elfeed.el ends here
