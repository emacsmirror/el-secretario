;;; el-secretario.el General interface for el-secretario -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Leo
;;
;; Author: Leo Okawa Ericson <http://github/Zetagon>
;; Maintainer: Leo <github@relevant-information.com>
;; Created: September 20, 2020
;; Modified: October 17, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/Zetagon/el-secretario
;; Package-Requires: ((emacs 26.1) (cl-lib "0.5") (hydra "0.15.0")(org-ql "0.6-pre"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(require 'hydra)
(require 'org-ql)
(require 'el-secretario-org)
(require 'el-secretario-message)
(require 'el-secretario-notmuch)
(require 'el-secretario-space)
(require 'el-secretario-tasks)
(defvar el-secretario-is-testing nil
  "Determines if code is running in testing mode.

When a user is interacting with el-secretario this should always
be nil. Set it to `t' if in testing
")


(defhydra el-secretario-default-hydra ()
  ("n" el-secretario-next-item "next" :exit t)
  ("q" (el-secretario-end-sesion) "Quit" :exit t)
  ("/" nil "disable hydra"  :exit t))

(defun el-secretario-activate-hydra ()
  (interactive)
  (when el-secretario-current-source-list
    (funcall (el-secretario-source-hydra-body
              (car el-secretario-current-source-list)))))


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

(defvar el-secretario-status-buffer-name "*el-secretario-status*"
  "TODO")
(defvar el-secretario--original-buffer nil
  "The buffer the user was in before activating el-secretario.")

(defhydra el-secretario--hydra-quit (:exit t
                                     :foreign-keys run)
  ("q"  (when el-secretario--original-buffer
          (switch-to-buffer el-secretario--original-buffer)) "Quit"))

;;;###autoload
(defun el-secretario-start-session (source-list)
  (setq el-secretario--original-buffer (current-buffer))
  (setq el-secretario-current-source-list source-list)
  (with-current-buffer (get-buffer-create "*el-secretario-en*")
    (delete-region (point-min) (point-max)))
  (funcall (el-secretario-source-init-function (car source-list)))
  (el-secretario-status-buffer-activate))

(defun el-secretario-end-sesion ()
  (switch-to-buffer el-secretario--original-buffer)
  (el-secretario-status-buffer-deactivate))

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
    (el-secretario-end-sesion)))


(defun el-secretario-status-buffer-activate ()
  (el-secretario-status-buffer-deactivate)
  (display-buffer-in-side-window (get-buffer-create el-secretario-status-buffer-name)
                                 '((side . top))))

(defun el-secretario-status-buffer-deactivate ()
  (-some-> (get-buffer-window el-secretario-status-buffer-name)
    (delete-window)))

;;; Utility functions


(defvar el-secretario--y-or-no-p-input-list nil
  "The list `el-secretario-y-or-no-p' will take from if in testing mode")

(defun el-secretario--y-or-n-p (prompt)
  (if el-secretario-is-testing
      (pop el-secretario--y-or-no-p-input-list)
    (y-or-n-p prompt)))
;; (let ((el-secretario-is-testing t)
;;       (el-secretario--y-or-no-p-input-list '(n n y)))
;;   (el-secretario--y-or-n-p "foo")
;;   (el-secretario--y-or-n-p "foo")
;;   (el-secretario--y-or-n-p "foo")
;;   (el-secretario--y-or-n-p "foo"))
(provide 'el-secretario)
;;; el-secretario.el ends here
