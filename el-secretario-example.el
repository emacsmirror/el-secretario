;;; el-secretario-example.el --- Example source for el-secretario -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Leo
;;
;; Author: Leo Okawa Ericson <http://github/Zetagon>
;; Maintainer: Leo <github@relevant-information.com>
;; Created: June 08, 2021
;; Modified: June 08, 2021
;; Version: 0.0.1
;; Keywords: convenience
;; Homepage: https://git.sr.ht/~zetagon/el-secretario
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file LICENSE.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;;  This is a trivial example for how a source is implemented.
;;
;;; Code:
(require 'el-secretario-source)
(defclass  el-secretario-example-source (el-secretario-source)
  ((current-item :initform nil)
   (items-left :initarg :items-left)
   (items-done :initform '())))
(defun el-secretario-example-get-current-val (source)
  (plist-get (oref source current-item)
             :val))

(cl-defmethod el-secretario-source-activate ((obj el-secretario-example-source) &optional backwards)
  (el-secretario-activate-keymap)
  (el-secretario-source-activate-item obj))

(cl-defmethod el-secretario-source-init ((obj el-secretario-example-source) &optional backwards)
  (with-slots (items-done items-left current-item) obj
    (setq items-left (mapcar (lambda (x)
                               (list :val x :reviewed 0))
                             items-left))
    (el-secretario-source-next-item obj)))

(cl-defmethod el-secretario-source-activate-item ((obj el-secretario-example-source))
  (with-slots (current-item) obj
    (plist-put current-item :reviewed
               (1+ (plist-get current-item
                              :reviewed)))))

(cl-defmethod el-secretario-source-next-item ((obj el-secretario-example-source))
  (with-slots (current-item items-left items-done) obj
    (if-let ((item (pop items-left)))
        (progn
          (when current-item
            (push current-item items-done))
          (setq current-item item)
          (el-secretario-source-activate-item obj))

      (message "No next item!")
      (el-secretario--next-source))))

(cl-defmethod el-secretario-source-previous-item ((obj el-secretario-example-source))
  "TODO"
  (with-slots (items-left items-done current-item) obj
    (if-let ((item (pop items-done)))
        (progn
          (when current-item
            (push current-item items-left))
          (setq current-item item)
          (el-secretario-source-activate-item obj))
      (message "No previous item!")
      (el-secretario--previous-source))))

(provide 'el-secretario-example)
;;; el-secretario-example.el ends here
