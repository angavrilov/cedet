;;; ede-proj-info.el --- EDE Generic Project texinfo support

;;;  Copyright (C) 1998, 1999, 2000  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-proj-info.el,v 1.7 2000/09/24 15:37:39 zappo Exp $

;; This software is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Handle texinfo in and EDE Project file.

(require 'ede-pmake)

;;; Code:
(defclass ede-proj-target-makefile-info (ede-proj-target-makefile)
  ((menu :initform nil)
   (keybindings :initform nil)
   (availablecompilers :initform (ede-makeinfo-compiler))
   (sourcetype :initform (ede-makeinfo-source))
   (mainmenu :initarg :mainmenu
	     :initform ""
	     :type string
	     :custom string
	     :documentation "The main menu resides in this file.
All other sources should be included independently."))
  "Target for a single info file.")

(defvar ede-makeinfo-source
  (ede-sourcecode "ede-makeinfo-source"
		  :name "Texinfo"
		  :sourcepattern "\\.texi?$"
		  :garbagepattern '("*.info"))
  "Texinfo source code definition.")

(defvar ede-makeinfo-compiler
  (ede-compiler
   "ede-makeinfo-compiler"
   :name "makeinfo"
   :variables '(("MAKEINFO" . "makeinfo"))
   :commands '("makeinfo -o $@ $<")
   :autoconf '(("AC_CHECK_PROG" . "MAKEINFO, makeinfo"))
   :sourcetype '(ede-makeinfo-source)
   ;; the extention keys off if we want intermediate files or not,
   ;; not what the actual extention is.
   :objectextention ".info"
   )
  "Compile texinfo files into info files.")

;;; Makefile generation
;;
(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target-makefile-info))
  "Return the variable name for THIS's sources."
  (concat (ede-pmake-varname this) "_INFOS"))

(defmethod ede-proj-makefile-target-name ((this ede-proj-target-makefile-info))
  "Return the name of the main target for THIS target."
  (let ((n (ede-name this)))
    (if (string-match "\\.info$" n)
	n
      (concat n ".info"))))

(defmethod object-write ((this ede-proj-target-makefile-info))
  "Before commiting any change to THIS, make sure the mainmenu is first."
   (let ((mm (oref this mainmenu))
	 (s (oref this source))
	 (nl nil))
     (if (or (string= mm "") (not mm) (string= mm (car s)))
	 nil
       ;; Make sure that MM is first in the list of items.
       (setq nl (cons mm (delq mm s)))
       (oset this source nl)))
   (call-next-method))

(defmethod ede-documentation ((this ede-proj-target-makefile-info))
  "Return a list of files that provides documentation.
Documentation is not for object THIS, but is provided by THIS for other
files in the project."
  (oref this source))

(provide 'ede-proj-info)

;;; ede-proj-info.el ends here
