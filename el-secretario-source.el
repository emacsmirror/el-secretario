;;; el-secretario-source.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Leo
;;
;; Author: Leo Okawa Ericson <http://github/Zetagon>
;; Maintainer: Leo <github@relevant-information.com>
;; Created: June 07, 2021
;; Modified: June 07, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://git.sr.ht/~zetagon/el-secretario
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5") (org-ql "0.6-pre"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'eieio)

(defclass el-secretario-source (eieio-named)
  ((keymap :initarg :keymap
           :initform #'el-secretario-default-map
           :protection :protected)
   (is-initialized :initform nil))
  :abstract t
  :documentation
  "Base class for sources. All sources should inherit from this
one.

It makes sure that the invariant \"A source is initialized (via
the `el-secretario-source-init') only once\" is kept.


")

(provide 'el-secretario-source)
;;; el-secretario-source.el ends here
