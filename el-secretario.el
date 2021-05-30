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
;; Homepage: https://git.sr.ht/~zetagon/el-secretario
;; Package-Requires: ((emacs 26.1) (cl-lib "0.5") (hydra "0.15.0")(org-ql "0.6-pre"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(require 'eieio)
(require 'eieio-base)
(require 'cl-lib)
(require 'hydra)
(require 'org-ql)
(defclass el-secretario-source (eieio-named)
  ((hydra :initarg :hydra
          :protection :protected)
   (is-initialized :initform nil))
  :abstract t
  :documentation
  "Base class for sources. All sources should inherit from this
one.

It makes sure that the invariant \"A source is initialized (via
the `el-secretario-source-init') only once\" is kept.


")
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

(cl-defmethod el-secretario--source-hydra ((obj el-secretario-source))
  "Activate hydra of OBJ."
  (funcall (oref obj hydra)))

(defun el-secretario-activate-hydra ()
  "Activate the hydra of the currently active source."
  (interactive)
  (when el-secretario-current-source-list
    (el-secretario--source-hydra (car el-secretario-current-source-list))))

(defvar el-secretario-current-source-list nil
  "TODO")

(defvar el-secretario-current-source-list-done nil
  "TODO")

(defvar el-secretario-status-buffer-name "*el-secretario-status*"
  "TODO")
(defvar el-secretario--original-buffer nil
  "The buffer the user was in before activating el-secretario.")

(defvar el-secretario--sesion-active nil
  "t when a session is active")

(defvar el-secretario-sources '())

(defhydra el-secretario--hydra-quit (:exit t
                        :foreign-keys run)
  ("q"  (when el-secretario--original-buffer
          (switch-to-buffer el-secretario--original-buffer)) "Quit"))

(defun secretary ()
  (interactive)
  (if el-secretario--sesion-active
      (progn
        (el-secretario-activate-hydra))
    (el-secretario-start-session
     (alist-get (completing-read "Choose what to do" el-secretario-sources)
                el-secretario-sources nil nil #'equal))))

;;;###autoload
(defun el-secretario-start-session (source-list)
  "Start session specified by SOURCE-LIST.

SOURCE-LIST should be a list of newly instantiated sources, or
SOURCE-LIST is a function that returns a list of newly instantiated sources."
  (setq el-secretario--sesion-active t)
  (setq el-secretario--original-buffer (current-buffer))
  (setq el-secretario-current-source-list
        (--> (if (functionp source-list)
                 (funcall source-list)
               source-list)
          (if (listp it)
              it
            (list it))))
  (setq el-secretario-current-source-list-done nil)
  (with-current-buffer (get-buffer-create "*el-secretario-en*")
    (delete-region (point-min) (point-max)))
  (el-secretario-status-buffer-activate)
  (el-secretario-source-init (car el-secretario-current-source-list)))

(defun el-secretario-end-sesion ()
  (setq el-secretario--sesion-active nil)
  (switch-to-buffer el-secretario--original-buffer)
  (el-secretario-status-buffer-deactivate))

(defun el-secretario-next-item ()
  "Go to the next item of this session."
  (interactive)
  (when el-secretario-current-source-list
    (el-secretario-source-next-item
     (car el-secretario-current-source-list))))

(defun el-secretario-previous-item ()
  "Go to the previous item of this session."
  (interactive)
  (when el-secretario-current-source-list
    (el-secretario-source-previous-item
     (car el-secretario-current-source-list))))

(cl-defmethod el-secretario-source-initialized-p ((obj el-secretario-source))
  "Return `t' if OBJ is initialized"
  (oref obj is-initialized))

(cl-defmethod el-secretario-source-init :after ((obj el-secretario-source) &optional backwards)
  "Set the `is-initialized' flag after a source has been initialized."
  (with-slots (is-initialized) obj
    (setq is-initialized t)))

(cl-defmethod el-secretario-source-init :around ((obj el-secretario-source) &optional backwards)
  "Make sure that a source is only initialized once. If a source
is already initialized and this method is called, call `el-secretario-source-activate' instead."
  (if (el-secretario-source-initialized-p obj)
      (el-secretario-source-activate obj backwards)
    (cl-call-next-method)))



(cl-defmethod el-secretario-source-next-item ((obj el-secretario-source))
  "Go to the next item of source OBJ.

It should call `el-secretario--next-source' if there are no more items.

Example:
 For the notmuch module, this method goes to the next email."
  (display-warning "This source doesn't implement the next-item method!"))

(cl-defmethod el-secretario-source-previous-item ((obj el-secretario-source))
  "Go to the previous item of source OBJ.

It should call `el-secretario--previous-source' if there are no more items."
  (display-warning "This source doesn't implement the previous-item method!"))

(cl-defmethod el-secretario-source-init ((obj el-secretario-source))
  "Initialize source OBJ.

This method is called only once, the first time source OBJ is
activated. Subsequenct calls will redirect to
`el-secretario-source-activate'.

Implement this method if you need to setup state etc. for your
source that only needs to happen once. The default
behaviour (i.e. if your source doesn 't implement this method) is
to call `el-secretario-source-activate'.

It should also do whatever is needed to bring up the relevant item to the user."
  (el-secretario-source-activate obj backwards))

(cl-defmethod el-secretario-source-activate ((obj el-secretario-source))
  "Activate source OBJ.

This method is called when el-secretario switches to source
OBJ (for example when the user calls `el-secretario-next-item'
with no items left, so el-secretario switches to source OBJ).

For example, the org module implements this method to bring up
the correct org buffer, and go to the correct heading."
  (display-warning "This source doesn't implement the activate method"))

(defun el-secretario--next-source ()
  "Switch to the next source in this session."
  (if el-secretario-current-source-list
      (progn
        (push (pop el-secretario-current-source-list)
              el-secretario-current-source-list-done)
        (if el-secretario-current-source-list
            (el-secretario-source-init (car el-secretario-current-source-list))
          (with-current-buffer (get-buffer-create "*el-secretario-en*")
            (insert "Done!"))
          (switch-to-buffer (get-buffer-create "*el-secretario-en*"))))
    (el-secretario-status-buffer-deactivate)
    (el-secretario-end-sesion)))

(defun el-secretario--previous-source ()
  "Switch to the previous source in this session."
  (if el-secretario-current-source-list-done
      (progn
        (push (pop el-secretario-current-source-list-done)
              el-secretario-current-source-list)
        (if el-secretario-current-source-list
            (el-secretario-source-init (car el-secretario-current-source-list) 'backward)
          (message "ooflakjdlkf")))
    (message "No more previous sources!")))


(defun el-secretario-status-buffer-activate ()
  "Activate the status buffer."
  (el-secretario-status-buffer-deactivate)
  (display-buffer-in-side-window (get-buffer-create el-secretario-status-buffer-name)
                                 '((side . top))))

(defun el-secretario-status-buffer-deactivate ()
  "Deactivate the status buffer."
  (-some-> (get-buffer-window el-secretario-status-buffer-name)
    (delete-window)))

;;; Utility functions


(defvar el-secretario--y-or-no-p-input-list nil
  "The list `el-secretario-y-or-no-p' will take from if in testing mode")

(defun el-secretario--y-or-n-p (prompt)
  "A version of `y-or-n-p' that is testable."
  (if el-secretario-is-testing
      (pop el-secretario--y-or-no-p-input-list)
    (y-or-n-p prompt)))

;; Shuffling a list. Credit: https://kitchingroup.cheme.cmu.edu/blog/2014/09/06/Randomize-a-list-in-Emacs/
(defun el-secretario--swap (LIST x y)
  "Swap item X and Y in LIST."
  (cl-psetf (elt LIST y) (elt LIST x)
            (elt LIST x) (elt LIST y)))

(defun el-secretario--shuffle (LIST)
  "Shuffle the elements in LIST.
shuffling is done in place."
  (cl-loop for i in (reverse (number-sequence 1 (1- (length LIST))))
           do (let ((j (random (+ i 1))))
                (el-secretario--swap LIST i j)))
  LIST)

(provide 'el-secretario)
;;; el-secretario.el ends here
