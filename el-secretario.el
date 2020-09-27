;;; el-secretario.el General interface for el-secretario -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Leo
;;
;; Author: Leo <http://github/leo>
;; Maintainer: Leo <leo@leo-B85-HD3>
;; Created: September 20, 2020
;; Modified: September 20, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/leo/el-secretario
;; Package-Requires: ((emacs 26.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(cl-defstruct el-secretario-source
  init-function
  next-function
  prev-function
  hydra-body
  finished-hook
  next-item-hook)

(defvar el-secretario-current-source-list nil
  "TODO")
(defvar el-secretario-current-source-list-done nil
  "TODO")

(defun el-secretario-start-session (source-list)
  (setq el-secretario-current-source-list source-list)
  (funcall (el-secretario-source-init-function (car source-list)))
  (el-secretario-next-item))

(defun el-secretario-next-item ()
  (interactive)
  (while (and el-secretario-current-source-list
              (not (funcall (el-secretario-source-next-function
                             (car el-secretario-current-source-list)))))

    (funcall (el-secretario-source-finished-hook
              (car el-secretario-current-source-list)))

    (push el-secretario-current-source-list-done
          (car el-secretario-current-source-list))
    (pop el-secretario-current-source-list)

    (when (car el-secretario-current-source-list)
      (funcall (el-secretario-source-init-function (car el-secretario-current-source-list)))))

  (when el-secretario-current-source-list
    (funcall (el-secretario-source-hydra-body (car el-secretario-current-source-list)))
    (funcall (el-secretario-source-next-item-hook
              (car el-secretario-current-source-list)))))

(provide 'el-secretario)
;;; el-secretario.el ends here
