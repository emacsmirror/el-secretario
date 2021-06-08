;;; el-secretario-test.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Leo
;;
;; Author: Leo Okawa Ericson <http://github/Zetagon>
;; Maintainer: Leo <github@relevant-information.com>
;; Created: June 08, 2021
;; Modified: June 08, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/leo/el-secretario-test
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'el-secretario-source)
(defclass  el-secretario-test-source (el-secretario-source)
  ((current-item :initform nil)
   (items-left :initarg :items-left)
   (items-done :initform '())))

(cl-defmethod el-secretario-source-init ((obj el-secretario-test-source) &optional backwards)
  (with-slots (items-done items-left current-item) obj
    (setq items-left (mapcar (lambda (x)
                               (list :val x :reviewed 0))
                             items-left))
    (el-secretario-source-next-item obj)))

(cl-defmethod el-secretario-source-activate-item ((obj el-secretario-test-source))
  (with-slots (current-item) obj
    (plist-put current-item (1+
                             (plist-get current-item
                                        :reviewed)))
    (plist-get current-item :val)))

(cl-defmethod el-secretario-source-next-item ((obj el-secretario-test-source))
  (with-slots (current-item items-left items-done) obj
    (if-let ((item (pop items-left)))
        (progn
          (when current-item
            (push current-item items-done))
          (setq current-item item)
          (el-secretario-source-activate-item obj))

      (message "No next item!")
      (el-secretario--next-source))))

(cl-defmethod el-secretario-source-previous-item ((obj el-secretario-test-source))
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

(provide 'el-secretario-test)
;;; el-secretario-test.el ends here
