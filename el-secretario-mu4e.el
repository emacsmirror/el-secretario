;;; el-secretario-mu4e.el Mu4e implementation for el-secretario -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Leo
;;
;; Author: Leo Okawa Ericson <http://github/Zetagon>
;; Maintainer: Leo <github@relevant-information.com>
;; Created: June 20, 2021
;; Modified: June 20, 2021
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

(defclass el-secretario-mu4e-source (el-secretario-source)
  ((query :initarg :query)))
(defvar el-secretario-mu4e-map (make-sparse-keymap))

;;;###autoload
(defun el-secretario-mu4e-make-source (query &optional keymap)
  "Convenience macro for creating a source for mu4e mail.
QUERY is a normal mu4e query.
KEYMAP is a keymap to use during review of this source"
  (el-secretario-mu4e-source
   :keymap (or keymap 'el-secretario-mu4e-map)
   :query query))

(cl-defmethod el-secretario-source-activate ((obj el-secretario-mu4e-source) &optional backwards)
  (with-slots (query) obj
    (setq el-secretario-mu4e--activate-backwards backwards)
    (setq mu4e-split-view 'single-window)
    (add-hook 'mu4e-headers-found-hook #'el-secretario-mu4e--after-search-h)
    (mu4e-headers-search (or query "flag:unread"))
    (el-secretario/activate-keymap)))

(defvar el-secretario-mu4e--activate-backwards nil)
(defun el-secretario-mu4e--after-search-h ()
  (remove-hook 'mu4e-headers-found-hook #'el-secretario-mu4e--after-search-h)
  (when el-secretario-mu4e--activate-backwards
    (goto-char (point-max))
    (forward-line -1))
  (mu4e-headers-view-message))

(cl-defmethod el-secretario-source-next-item ((obj el-secretario-mu4e-source))
  (unless (mu4e-view-headers-next)
    (el-secretario--next-source)))


(cl-defmethod el-secretario-source-previous-item ((obj el-secretario-mu4e-source))
  (unless (mu4e-view-headers-next -1)
    (el-secretario--previous-source)))



(provide 'el-secretario-mu4e)
;;; el-secretario-mu4e.el ends here
