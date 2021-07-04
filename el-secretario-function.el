;;; el-secretario-function.el Source for just calling one function -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Leo
;;
;; Author: Leo <https://github.com/Zetagon>
;; Maintainer: Leo <github@relevant-information.com>
;; Created: June 18, 2021
;; Modified: June 18, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/zetagon/el-secretario
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(require 'el-secretario)

;;;###autoload
(defclass el-secretario-function-source (el-secretario-source)
  ((func :initarg :func)))
(cl-defmethod el-secretario-source-activate ((obj el-secretario-function-source) &optional backwards)
  (funcall (oref obj func)))

(cl-defmethod el-secretario-source-next-item ((obj el-secretario-function-source))
  (with-slots (current-item items-left items-done) obj
    (el-secretario--next-source)))

(cl-defmethod el-secretario-source-previous-item ((obj el-secretario-function-source))
  (with-slots (current-item items-left items-done) obj
    (el-secretario--previous-source)))

(provide 'el-secretario-function)
;;; el-secretario-function.el ends here
