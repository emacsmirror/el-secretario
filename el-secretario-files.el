;;; el-secretario-files.el --- files source for el-secretario -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Leo Okawa Ericson
;;
;; Author: Leo Okawa Ericson <https://sr.ht/~zetagon>
;; Maintainer: Leo Okawa Ericson <git@relevant-information.com>
;; Created: Aug 26, 2021
;; Modified: Aug 26, 2021
;; Version: 0.0.1
;; Keywords: convenience
;; Homepage: https://git.sr.ht/~zetagon/el-secretario
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
;;  This is a source for going throug a list of files
;;
;;; Code:
(require 'el-secretario-source)

(defclass  el-secretario-files-source (el-secretario-source)
  ((current-file :initform nil)
   (files-left :initarg :files)
   (files-done :initform '())))

;;;###autoload
(defun el-secretario-files-make-source (files &optional keymap)
  "Make a source that has FILES as items.

FILES is a list of file names or buffers.

Use KEYMAP during the review if available, otherwise use `el-secretario-source-default-map'."
  (if keymap
      (el-secretario-files-source :files files
               :keymap keymap)
    (el-secretario-files-source :files files)))

(cl-defmethod el-secretario-source-activate ((obj el-secretario-files-source) &optional backwards)
  (el-secretario-activate-keymap)
  (el-secretario-source-activate-item obj))

(cl-defmethod el-secretario-source-init ((obj el-secretario-files-source) &optional backwards)
  (el-secretario-source-next-item obj))

(cl-defmethod el-secretario-source-activate-item ((obj el-secretario-files-source))
  (with-slots (current-file) obj
    (if (bufferp current-file)
        (switch-to-buffer current-file)
      (find-file current-file))))

(cl-defmethod el-secretario-source-next-item ((obj el-secretario-files-source))
  (with-slots (current-file files-left files-done) obj
    (if-let ((file (pop files-left)))
        (progn
          (when current-file
            (push current-file files-done))
          (setq current-file file)
          (el-secretario-source-activate-item obj))
      (message "No next file!")
      (el-secretario--next-source))))

(cl-defmethod el-secretario-source-previous-item ((obj el-secretario-files-source))
  (with-slots (files-left files-done current-file) obj
    (if-let ((file (pop files-done)))
        (progn
          (when current-file
            (push current-file files-left))
          (setq current-file file)
          (el-secretario-source-activate-item obj))
      (message "No previous file!")
      (el-secretario--previous-source))))

(provide 'el-secretario-files)
;;; el-secretario-files.el ends here

;; Local Variables:
;; package-lint-main-file: "el-secretario.el"
;; End:
