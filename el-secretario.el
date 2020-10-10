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




(defhydra el-secretario-default-hydra (:foreign-keys run)
   ("n" el-secretario-next-item "next")
   ("q" (switch-to-buffer el-secretario--original-buffer) "Quit" :exit t))


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
(defvar el-secretario--original-buffer nil
  "The buffer the user was in before activating el-secretario.")

(defhydra el-secretario--hydra-quit (:exit t
                                     :foreign-keys run)
  ("q"  (when el-secretario--original-buffer
          (switch-to-buffer el-secretario--original-buffer)) "Quit"))

(defun el-secretario-start-session (source-list)
  (setq el-secretario--original-buffer (current-buffer))
  (setq el-secretario-current-source-list source-list)
  (with-current-buffer (get-buffer-create "*el-secretario-en*")
    (delete-region (point-min) (point-max)))
  (funcall (el-secretario-source-init-function (car source-list))))

(defun el-secretario-next-item ()
  (interactive)
  (when el-secretario-current-source-list
    (funcall (el-secretario-source-next-function
              (car el-secretario-current-source-list)))))


(defun el-secretario--next-source ()
  (if el-secretario-current-source-list
      (progn
        (push el-secretario-current-source-list-done
              (car el-secretario-current-source-list))
        (pop el-secretario-current-source-list)
        (if el-secretario-current-source-list
            (funcall (el-secretario-source-init-function
                      (car el-secretario-current-source-list)))
          (with-current-buffer (get-buffer-create "*el-secretario-en*")
            (insert "Done!"))
          (switch-to-buffer (get-buffer-create "*el-secretario-en*"))))
    (message "Done!")))

(provide 'el-secretario)
;;; el-secretario.el ends here
