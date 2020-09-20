;;; all-reviewer.el General interface for all-reviewer -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Leo
;;
;; Author: Leo <http://github/leo>
;; Maintainer: Leo <leo@leo-B85-HD3>
;; Created: September 20, 2020
;; Modified: September 20, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/leo/all-reviewer
;; Package-Requires: ((emacs 26.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(cl-defstruct all-reviewer-source
  init-function
  next-function
  prev-function
  hydra-body
  finished-hook
  next-item-hook)

(defvar all-reviewer-current-source-list nil
  "TODO")
(defvar all-reviewer-current-source-list-done nil
  "TODO")

(defun all-reviewer-start-session (source-list)
  (setq all-reviewer-current-source-list source-list)
  (funcall (all-reviewer-source-init-function (car source-list)))
  (funcall (all-reviewer-source-hydra-body (car source-list))))

(defun all-reviewer-next-item ()
  (interactive)
  (while (and all-reviewer-current-source-list
              (not (funcall (all-reviewer-source-next-function
                             (car all-reviewer-current-source-list)))))

    (funcall (all-reviewer-source-finished-hook
              (car all-reviewer-current-source-list)))

    (push all-reviewer-current-source-list-done
          (car all-reviewer-current-source-list))
    (pop all-reviewer-current-source-list))

  (when all-reviewer-current-source-list
      (funcall (all-reviewer-source-next-item-hook
                (car all-reviewer-current-source-list)))))

(provide 'all-reviewer)
;;; all-reviewer.el ends here
