;;; semantic-find.el --- Search routines

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-find.el,v 1.13 2003/05/29 01:05:06 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
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
;; Routines for searching through lists of tags.
;; There are several groups of tag search routines:
;;
;; 1) semantic-brute-find-tag-by-*
;;    These routines use brute force hierarchical search to scan
;;    through lists of tags.  They include some parameters
;;    used for compatibility with the semantic 1.x search routines.
;;
;; 1.5) semantic-brute-find-first-tag-by-*
;;    Like 1, except seraching stops on the first match for the given
;;    information.
;;
;; 2) semantic-find-tag-by-*
;;    These prefered search routines attempt to scan through lists
;;    in an intelligent way based on questions asked.
;;
;; 3) semantic-find-*-overlay
;;    These routines use overlays to return tags based on a buffer position.
;;
;; 4) ...

(require 'semantic-tag)

;;; Code:

;;; Overlay Search Routines
;;
;; These routines provide fast access to tokens based on a buffer that
;; has parsed tokens in it.  Uses overlays to perform the hard work.
;;
;;;###autoload
(defun semantic-find-tag-by-overlay (&optional positionormarker buffer)
  "Find all tags covering POSITIONORMARKER by using overlays.
If POSITIONORMARKER is nil, use the current point.
Optional BUFFER is used if POSITIONORMARKER is a number, otherwise the current
buffer is used.  This finds all tags covering the specified position
by checking for all overlays covering the current spot.  They are then sorted
from largest to smallest via the start location."
  (save-excursion
    (when positionormarker
      (if (markerp positionormarker)
	  (set-buffer (marker-buffer positionormarker))
	(if (bufferp buffer)
	    (set-buffer buffer))))
    (let ((ol (semantic-overlays-at (or positionormarker (point))))
	  (ret nil))
      (while ol
	(let ((tmp (semantic-overlay-get (car ol) 'semantic)))
	  (when (and tmp
		     ;; We don't need with-position because no tag w/out
		     ;; a position could exist in an overlay.
		     (semantic-tag-p tmp))
	    (setq ret (cons tmp ret))))
	(setq ol (cdr ol)))
      (sort ret (lambda (a b) (< (semantic-tag-start a)
				 (semantic-tag-start b)))))))

;;;###autoload
(defun semantic-find-tag-by-overlay-in-region (start end &optional buffer)
  "Find all tags which exist in whole or in part between START and END.
Uses overlays to determine positin.
Optional BUFFER argument specifies the buffer to use."
  (save-excursion
    (if buffer (set-buffer buffer))
    (let ((ol (semantic-overlays-in start end))
	  (ret nil))
      (while ol
	(let ((tmp (semantic-overlay-get (car ol) 'semantic)))
	  (when (and tmp
		     ;; See above about position
		     (semantic-tag-p tmp))
	    (setq ret (cons tmp ret))))
	(setq ol (cdr ol)))
      (sort ret (lambda (a b) (< (semantic-tag-start a)
				 (semantic-tag-start b)))))))

;;;###autoload
(defun semantic-find-tag-by-overlay-next (&optional start buffer)
  "Find the next tag after START in BUFFER.
If START is in an overlay, find the tag which starts next,
not the current tag."
  (save-excursion
    (if buffer (set-buffer buffer))
    (if (not start) (setq start (point)))
    (let ((os start) (ol nil))
      (while (and os (< os (point-max)) (not ol))
	(setq os (semantic-overlay-next-change os))
	(when os
	  ;; Get overlays at position
	  (setq ol (semantic-overlays-at os))
	  ;; find the overlay that belongs to semantic
	  ;; and starts at the found position.
	  (while (and ol (listp ol))
	    (if (and (semantic-overlay-get (car ol) 'semantic)
		     (semantic-tag-p
		      (semantic-overlay-get (car ol) 'semantic))
		     (= (semantic-overlay-start (car ol)) os))
		(setq ol (car ol)))
	    (when (listp ol) (setq ol (cdr ol))))))
      ;; convert ol to a tag
      (when (and ol (semantic-tag-p (semantic-overlay-get ol 'semantic)))
	(semantic-overlay-get ol 'semantic)))))

;;;###autoload
(defun semantic-find-tag-by-overlay-prev (&optional start buffer)
  "Find the next tag before START in BUFFER.
If START is in an overlay, find the tag which starts next,
not the current tag."
  (save-excursion
    (if buffer (set-buffer buffer))
    (if (not start) (setq start (point)))
    (let ((os start) (ol nil))
      (while (and os (> os (point-min)) (not ol))
	(setq os (semantic-overlay-previous-change os))
	(when os
	  ;; Get overlays at position
	  (setq ol (semantic-overlays-at os))
	  ;; find the overlay that belongs to semantic
	  ;; and starts at the found position.
	  (while (and ol (listp ol))
	    (if (and (semantic-overlay-get (car ol) 'semantic)
		     (semantic-tag-p
		      (semantic-overlay-get (car ol) 'semantic))
		     (= (semantic-overlay-start (car ol)) os))
		(setq ol (car ol)))
	    (when (listp ol) (setq ol (cdr ol))))))
      ;; convert ol to a tag
      (when (and ol
		 (semantic-tag-p (semantic-overlay-get ol 'semantic)))
	(semantic-overlay-get ol 'semantic)))))

;;;###autoload
(defun semantic-find-tag-parent-by-overlay (tag)
  "Find the parent of TAG by overlays.
Overlays are a fast way of finding this information for active buffers."
  (let ((tag (nreverse (semantic-find-tag-by-overlay
			(semantic-tag-start tag)))))
    ;; This is a lot like `semantic-current-tag-parent', but
    ;; it uses a position to do it's work.  Assumes two tags don't share
    ;; the same start unless they are siblings.
    (car (cdr tag))))

;;;###autoload
(defun semantic-current-tag ()
  "Return the current tag in the current buffer.
If there are more than one in the same location, return the
smallest tag.  Return nil if there is no tag here."
  (car (nreverse (semantic-find-tag-by-overlay))))

;;;###autoload
(defun semantic-current-tag-parent ()
  "Return the current tags parent in the current buffer.
A tag's parent would be a containing structure, such as a type
containing a field.  Return nil if there is no parent."
  (car (cdr (nreverse (semantic-find-tag-by-overlay)))))

;;;###autoload
(defun semantic-current-tag-of-class (class)
  "Return the current (smallest) tags of CLASS in the current buffer.
If the smallest tag is not of type CLASS, keep going upwards until one
is found.
Uses `semantic-tag-class' for classification."
  (let ((tags (nreverse (semantic-find-tag-by-overlay))))
    (while (and tags
		(not (eq (semantic-tag-class (car tags)) class)))
      (setq tags (cdr tags)))
    (car tags)))

;;; Search Routines
;;
;; These are routines that search a single tags table.
;;
;; The original API (see COMPATIBILITY section below) in semantic 1.4
;; had these usage statistics:
;;
;; semantic-find-nonterminal-by-name 17
;; semantic-find-nonterminal-by-name-regexp 8  - Most doing completion
;; semantic-find-nonterminal-by-position 13
;; semantic-find-nonterminal-by-token 21
;; semantic-find-nonterminal-by-type 2
;; semantic-find-nonterminal-standard 1
;;
;; semantic-find-nonterminal-by-function (not in other searches)  1
;;
;; New API: As above w/out `search-parts' or `search-includes' arguments.
;; Extra fcn: Specific to completion which is what -name-regexp is
;;            mostly used for
;;
;; As for the sarguments "search-parts" and "search-includes" here
;; are stats:
;;
;; search-parts: 4  - charting x2, find-doc, senator (sans db)
;;
;; Implement command to flatten a tag table.  Call new API Fcn w/
;; flattened table for same results.
;;
;; search-include: 2 - analyze x2 (sans db)
;;
;; Not used effectively.  Not to be re-implemented here.

(defsubst semantic--find-tags-by-function (predicate &optional table)
  "Find tags for which PREDICATE is non-nil in TABLE.
PREDICATE is a lambda expression which accepts on TAG.
TABLE is a semantic tags table.  See `semantic-something-to-tag-table'."
  (let ((tags (semantic-something-to-tag-table table))
	(result nil))
;    (mapc (lambda (tag) (and (funcall predicate tag)
;			     (setq result (cons tag result))))
;	  tags)
    ;; A while loop is actually faster.  Who knew
    (while tags
      (and (funcall predicate (car tags))
	   (setq result (cons (car tags) result)))
      (setq tags (cdr tags)))
    (nreverse result)))

;; I can shave off some time by removing the funcall (see above)
;; and having the question be inlined in the while loop.
;; Strangely turning the upper level fcns into macros had a larger
;; impact.
(defmacro semantic--find-tags-by-macro (form &optional table)
  "Find tags for which FORM is non-nil in TABLE.
TABLE is a semantic tags table.  See `semantic-something-to-tag-table'."
  `(let ((tags (semantic-something-to-tag-table ,table))
         (result nil))
     (while tags
       (and ,form
            (setq result (cons (car tags) result)))
       (setq tags (cdr tags)))
     (nreverse result)))

;;; Top level Searches
;;
;;;###autoload
(defsubst semantic-find-first-tag-by-name (name &optional table)
  "Find the first tag with NAME in TABLE.
NAME is a string.
TABLE is a semantic tags table.  See `semantic-something-to-tag-table'.
This routine uses `assoc' to quickly find the first matching entry."
  (assoc-string name (semantic-something-to-tag-table table)
		semantic-case-fold))

;;;###autoload
(defmacro semantic-find-tags-by-name (name &optional table)
  "Find all tags with NAME in TABLE.
NAME is a string.
TABLE is a tag table.  See `semantic-something-to-tag-table'."
  `(let ((case-fold-search semantic-case-fold))
     (semantic--find-tags-by-macro
      (string= ,name (semantic-tag-name (car tags)))
      ,table)))

;;;###autoload
(defmacro semantic-find-tags-for-completion (prefix &optional table)
  "Find all tags whos name begins with PREFIX in TABLE.
PREFIX is a string.
TABLE is a tag table.  See `semantic-something-to-tag-table'.
While it would be nice to use `try-completion' or `all-completions',
those functions do not return the tags, only a string.
Uses `compare-strings' for fast comparison."
  `(let ((l (length ,prefix)))
     (semantic--find-tags-by-macro
      (eq (compare-strings ,prefix 0 nil
			   (semantic-tag-name (car tags)) 0 l
			   semantic-case-fold)
	  t)
      ,table)))

;;;###autoload
(defmacro semantic-find-tags-by-name-regexp (regexp &optional table)
  "Find all tags with name matching REGEXP in TABLE.
REGEXP is a string containing a regular expression,
TABLE is a tag table.  See `semantic-something-to-tag-table'.
Consider using `semantic-find-tags-for-completion' if you are
attempting to do completions."
  `(let ((case-fold-search semantic-case-fold))
     (semantic--find-tags-by-macro
      (string-match ,regexp (semantic-tag-name (car tags)))
      ,table)))

;;;###autoload
(defmacro semantic-find-tags-by-class (class &optional table)
  "Find all tags of class CLASS in TABLE.
CLASS is a symbol representing the class of the token, such as
'variable, of 'function..
TABLE is a tag table.  See `semantic-something-to-tag-table'."
  `(semantic--find-tags-by-macro
    (eq ,class (semantic-tag-class (car tags)))
    ,table))

;;;###autoload
(defmacro semantic-find-tags-by-type (type &optional table)
  "Find all tags of with a type TYPE in TABLE.
TYPE is a string or tag representing a data type as defined in the
language the tags were parsed from, such as \"int\", or perhaps
a tag whose name is that of a struct or class.
TABLE is a tag table.  See `semantic-something-to-tag-table'."
  `(semantic--find-tags-by-macro
    (semantic-tag-of-type-p (car tags) ,type)
    ,table))

;;;###autoload
(defsubst semantic-find-tags-included (&optional table)
  "Find all tags in TABLE that are of the 'include class.
TABLE is a tag table.  See `semantic-something-to-tag-table'."
  (semantic-find-tags-by-class 'include table))

;;; Deep Searches
;;

;;;###autoload
(defmacro semantic-deep-find-tags-by-name (name &optional table)
  "Find all tags with NAME in TABLE.
Search in top level tags, and their components, in TABLE.
NAME is a string.
TABLE is a tag table.  See `semantic-flatten-tags-table'.
See also `semantic-find-tags-by-name'."
  `(semantic-find-tags-by-name
    ,name (semantic-flatten-tags-table ,table)))

;;;###autoload
(defmacro semantic-deep-find-tags-for-completion (prefix &optional table)
  "Find all tags whos name begins with PREFIX in TABLE.
Search in top level tags, and their components, in TABLE.
TABLE is a tag table.  See `semantic-flatten-tags-table'.
See also `semantic-find-tags-for-completion'."
  `(semantic-find-tags-for-completion
    ,prefix (semantic-flatten-tags-table ,table)))

;;;###autoload
(defmacro semantic-deep-find-tags-by-name-regexp (regexp &optional table)
  "Find all tags with name matching REGEXP in TABLE.
Search in top level tags, and their components, in TABLE.
REGEXP is a string containing a regular expression,
TABLE is a tag table.  See `semantic-flatten-tags-table'.
See also `semantic-find-tags-by-name-regexp'.
Consider using `semantic-deep-find-tags-for-completion' if you are
attempting to do completions."
  `(semantic-find-tags-by-name-regexp
    ,regexp (semantic-flatten-tags-table ,table)))

;;; Specialty Searches
;;
(defun semantic-find-tags-external-children-of-type (type &optional table)
  "Find all tags in whose parent is TYPE in TABLE.
These tags are defined outside the scope of the original TYPE declaration.
TABLE is a tag table.  See `semantic-something-to-tag-table'."
  (semantic--find-tags-by-macro
   (equal (semantic-nonterminal-external-member-parent (car tags))
	  type)
   table))

;;
;; ************************** Compatibility ***************************
;;

;;; Old Style Brute Force Search Routines
;;
;; These functions will search through tags lists explicity for
;; desired information.

;; The -by-name nonterminal search can use the built in fcn
;; `assoc', which is faster than looping ourselves, so we will
;; not use `semantic-brute-find-tag-by-function' to do this,
;; instead erroring on the side of speed.

;;;###autoload
(defun semantic-brute-find-first-tag-by-name
  (name streamorbuffer &optional search-parts search-include)
  "Find a tag NAME within STREAMORBUFFER.  NAME is a string.
If SEARCH-PARTS is non-nil, search children of tags.
If SEARCH-INCLUDE is non-nil, search include files.

Use `semantic-find-first-tag-by-name' instead."
  (let* ((stream (semantic-something-to-tag-table streamorbuffer))
         (assoc-fun (if semantic-case-fold
                        #'assoc-ignore-case
                      #'assoc))
	 (m (funcall assoc-fun name stream)))
    (if m
	m
      (let ((toklst stream)
	    (children nil))
	(while (and (not m) toklst)
	  (if search-parts
	      (progn
		(setq children (semantic-tag-components-with-overlays
				(car toklst)))
		(if children
		    (setq m (semantic-brute-find-first-tag-by-name
			     name children search-parts search-include)))))
	  (setq toklst (cdr toklst)))
	(if (not m)
	    ;; Go to dependencies, and search there.
	    nil)
	m))))

;;;###autoload
(defmacro semantic-brute-find-tag-by-class
  (class streamorbuffer &optional search-parts search-includes)
  "Find all tags with a class CLASS within STREAMORBUFFER.
CLASS is a symbol representing the class of the tags to find.
See `semantic-tag-class'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'.

Use `semantic-find-tag-by-class' instead."
  `(semantic-brute-find-tag-by-function
    (lambda (tag) (eq ,class (semantic-tag-class tag)))
    ,streamorbuffer ,search-parts ,search-includes))

;;;###autoload
(defmacro semantic-brute-find-tag-standard
  (streamorbuffer &optional search-parts search-includes)
  "Find all tags in STREAMORBUFFER which define simple class types.
See `semantic-tag-class'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'."
  `(semantic-brute-find-tag-by-function
    (lambda (tag) (member (semantic-tag-class tag)
			  '(function variable type)))
    ,streamorbuffer ,search-parts ,search-includes))

;;;###autoload
(defun semantic-brute-find-tag-by-type
  (type streamorbuffer &optional search-parts search-includes)
  "Find all tags with type TYPE within STREAMORBUFFER.
TYPE is a string which is the name of the type of the tags returned.
See `semantic-tag-type'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'."
  (semantic-brute-find-tag-by-function
   (lambda (tag)
     (let ((ts (semantic-tag-type tag)))
       (if (and (listp ts)
		(or (= (length ts) 1)
		    (eq (semantic-tag-class ts) 'type)))
	   (setq ts (semantic-tag-name ts)))
       (equal type ts)))
   streamorbuffer search-parts search-includes))

;;;###autoload
(defun semantic-brute-find-tag-by-type-regexp
  (regexp streamorbuffer &optional search-parts search-includes)
  "Find all tags with type matching REGEXP within STREAMORBUFFER.
REGEXP is a regular expression  which matches the  name of the type of the
tags returned.  See `semantic-tag-type'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'."
  (semantic-brute-find-tag-by-function
   (lambda (tag)
     (let ((ts (semantic-tag-type tag)))
       (if (listp ts)
	   (setq ts
		 (if (eq (semantic-tag-class ts) 'type)
		     (semantic-tag-name ts)
		   (car ts))))
       (and ts (string-match regexp ts))))
   streamorbuffer search-parts search-includes))

;;;###autoload
(defun semantic-brute-find-tag-by-name-regexp
  (regex streamorbuffer &optional search-parts search-includes)
  "Find all tags whose name match REGEX in STREAMORBUFFER.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'."
  (semantic-brute-find-tag-by-function
   (lambda (tag) (string-match regex (semantic-tag-name tag)))
    streamorbuffer search-parts search-includes)
  )

;;;###autoload
(defun semantic-brute-find-tag-by-property
  (property value streamorbuffer &optional search-parts search-includes)
  "Find all tags with PROPERTY equal to VALUE in STREAMORBUFFER.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'."
  (semantic-brute-find-tag-by-function
   (lambda (tag) (equal (semantic--tag-get-property tag property) value))
   streamorbuffer search-parts search-includes)
  )

;;;###autoload
(defun semantic-brute-find-tag-by-attribute
  (attr streamorbuffer &optional search-parts search-includes)
  "Find all tags with a given ATTR in STREAMORBUFFER.
ATTR is a symbol key into the attributes list.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'."
  (semantic-brute-find-tag-by-function
   (lambda (tag) (semantic-tag-get-attribute tag attr))
   streamorbuffer search-parts search-includes)
  )

;;;###autoload
(defun semantic-brute-find-tag-by-attribute-value
  (attr value streamorbuffer &optional search-parts search-includes)
  "Find all tags with a given ATTR equal to VALUE in STREAMORBUFFER.
ATTR is a symbol key into the attributes list.
VALUE is the value that ATTR should match.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'."
  (semantic-brute-find-tag-by-function
   (lambda (tag) (equal (semantic-tag-get-attribute tag attr) value))
   streamorbuffer search-parts search-includes)
  )

;;;###autoload
(defun semantic-brute-find-tag-by-function
  (function streamorbuffer &optional search-parts search-includes)
  "Find all tags for which FUNCTION's value is non-nil within STREAMORBUFFER.
FUNCTION must return non-nil if an element of STREAM will be included
in the new list.

If optional argument SEARCH-PARTS is non-nil, all sub-parts of tags
are searched.  The overloadable function `semantic-tag-componenets' is
used for the searching child lists.  If SEARCH-PARTS is the symbol
'positiononly, then only children that have positional information are
searched.

If SEARCH-INCLUDES is non-nil, then all include files are also
searched for matches.  This parameter hasn't be active for a while
and is obsolete."
  (let ((streamlist (list
		     (semantic-something-to-tag-table streamorbuffer)))
	(includes nil)			;list of includes
	(stream nil)			;current stream
        (tag  nil)                    ;current tag
	(sl nil)			;list of tag children
	(nl nil)			;new list
        (case-fold-search semantic-case-fold))
    (if search-includes
	(setq includes (semantic-brute-find-tag-by-class
			'include (car streamlist))))
    (while streamlist
      (setq stream     (car streamlist)
            streamlist (cdr streamlist))
      (while stream
        (setq tag  (car stream)
              stream (cdr stream))
	(if (not (semantic-tag-p tag))
            ;; `semantic-tag-components-with-overlays' can return invalid
            ;; tags if search-parts is not equal to 'positiononly
            nil ;; Ignore them!
          (if (funcall function tag)
              (setq nl (cons tag nl)))
          (and search-parts
               (setq sl (if (eq search-parts 'positiononly)
			    (semantic-tag-components-with-overlays tag)
			  (semantic-tag-components tag))
		     )
               (setq nl (nconc nl
                               (semantic-brute-find-tag-by-function
                                function sl
                                search-parts search-includes)))))))
    (setq nl (nreverse nl))
;;;    (while includes
;;;      (setq nl (append nl (semantic-brute-find-tag-by-function
;;;			   
;;;			   ))))
    nl))

;;;###autoload
(defun semantic-brute-find-first-tag-by-function
  (function streamorbuffer &optional search-parts search-includes)
  "Find the first nonterminal which FUNCTION match within STREAMORBUFFER.
FUNCTION must return non-nil if an element of STREAM will be included
in the new list.

The following parameters were never implemented.

If optional argument SEARCH-PARTS, all sub-parts of tags are searched.
The overloadable function `semantic-tag-components' is used for
searching.
If SEARCH-INCLUDES is non-nil, then all include files are also
searched for matches."
  (let ((stream (semantic-something-to-tag-table streamorbuffer))
	(found nil)
        (case-fold-search semantic-case-fold))
    (while (and (not found) stream)
      (if (funcall function (car stream))
	  (setq found (car stream)))
      (setq stream (cdr stream)))
    found))


;;; Old Positional Searches
;;
;; Are these useful anymore?
;;
;;;###autoload
(defun semantic-brute-find-tag-by-position (position streamorbuffer
						     &optional nomedian)
  "Find a nonterminal covering POSITION within STREAMORBUFFER.
POSITION is a number, or marker.  If NOMEDIAN is non-nil, don't do
the median calculation, and return nil."
  (save-excursion
    (if (markerp position) (set-buffer (marker-buffer position)))
    (let* ((stream (if (bufferp streamorbuffer)
		       (save-excursion
			 (set-buffer streamorbuffer)
			 (semantic-bovinate-toplevel))
		     streamorbuffer))
	   (prev nil)
	   (found nil))
      (while (and stream (not found))
	;; perfect fit
	(if (and (>= position (semantic-tag-start (car stream)))
		 (<= position (semantic-tag-end (car stream))))
	    (setq found (car stream))
	  ;; Median between to objects.
	  (if (and prev (not nomedian)
		   (>= position (semantic-tag-end prev))
		   (<= position (semantic-tag-start (car stream))))
	      (let ((median (/ (+ (semantic-tag-end prev)
				  (semantic-tag-start (car stream)))
			       2)))
		(setq found
		      (if (> position median)
			  (car stream)
			prev)))))
	;; Next!!!
	(setq prev (car stream)
	      stream (cdr stream)))
      found)))

;;;###autoload
(defun semantic-brute-find-innermost-tag-by-position
  (position streamorbuffer &optional nomedian)
  "Find a list of tags covering POSITION within STREAMORBUFFER.
POSITION is a number, or marker.  If NOMEDIAN is non-nil, don't do
the median calculation, and return nil.
This function will find the topmost item, and recurse until no more
details are available of findable."
  (let* ((returnme nil)
	 (current (semantic-brute-find-tag-by-position
		   position streamorbuffer nomedian))
	 (nextstream (and current
			  (if (eq (semantic-tag-class current) 'type)
			      (semantic-tag-type-members current)
			    nil))))
    (while nextstream
      (setq returnme (cons current returnme))
      (setq current (semantic-brute-find-tag-by-position
		     position nextstream nomedian))
      (setq nextstream (and current
			    ;; NOTE TO SELF:
			    ;; Looking at this after several years away,
			    ;; what does this do???
			    (if (eq (semantic-tag-class current) 'token)
				(semantic-tag-type-members current)
			      nil))))
    (nreverse (cons current returnme))))

;;; Compatibility Aliases
;;;###autoload
(semantic-alias-obsolete 'semantic-find-nonterminal-by-overlay
			 'semantic-find-tag-by-overlay)

;;;###autoload
(semantic-alias-obsolete 'semantic-find-nonterminal-by-overlay-in-region
			 'semantic-find-tag-by-overlay-in-region)

;;;###autoload
(semantic-alias-obsolete 'semantic-find-nonterminal-by-overlay-next
			 'semantic-find-tag-by-overlay-next)

;;;###autoload
(semantic-alias-obsolete 'semantic-find-nonterminal-by-overlay-prev
			 'semantic-find-tag-by-overlay-prev)

;;;###autoload
(semantic-alias-obsolete 'semantic-find-nonterminal-parent-by-overlay
			 'semantic-find-tag-parent-by-overlay)

;;;###autoload
(semantic-alias-obsolete 'semantic-current-nonterminal
			 'semantic-current-tag)

;;;###autoload
(semantic-alias-obsolete 'semantic-current-nonterminal-parent
			 'semantic-current-tag-parent)

;;;###autoload
(semantic-alias-obsolete 'semantic-current-nonterminal-of-type
			 'semantic-current-tag-of-class)

;;;###autoload
(semantic-alias-obsolete 'semantic-find-nonterminal-by-name
			 'semantic-brute-find-first-tag-by-name)

;;;###autoload
(semantic-alias-obsolete 'semantic-find-nonterminal-by-token
			 'semantic-brute-find-tag-by-class)

;;;###autoload
(semantic-alias-obsolete 'semantic-find-nonterminal-standard
			 'semantic-brute-find-tag-standard)

;;;###autoload
(semantic-alias-obsolete 'semantic-find-nonterminal-by-type
			 'semantic-brute-find-tag-by-type)

;;;###autoload
(semantic-alias-obsolete 'semantic-find-nonterminal-by-type-regexp
			 'semantic-brute-find-tag-by-type-regexp)

;;;###autoload
(semantic-alias-obsolete 'semantic-find-nonterminal-by-name-regexp
			 'semantic-brute-find-tag-by-name-regexp)

;;;###autoload
(semantic-alias-obsolete 'semantic-find-nonterminal-by-property
			 'semantic-brute-find-tag-by-property)

;;;###autoload
(semantic-alias-obsolete 'semantic-find-nonterminal-by-extra-spec
			 'semantic-brute-find-tag-by-attribute)

;;;###autoload
(semantic-alias-obsolete 'semantic-find-nonterminal-by-extra-spec-value
			 'semantic-brute-find-tag-by-attribute-value)

;;;###autoload
(semantic-alias-obsolete 'semantic-find-nonterminal-by-function
			 'semantic-brute-find-tag-by-function)

;;;###autoload
(semantic-alias-obsolete 'semantic-find-nonterminal-by-function-first-match
			 'semantic-brute-find-first-tag-by-function)

;;;###autoload
(semantic-alias-obsolete 'semantic-find-nonterminal-by-position
			 'semantic-brute-find-tag-by-position)

;;;###autoload
(semantic-alias-obsolete 'semantic-find-innermost-nonterminal-by-position
			 'semantic-brute-find-innermost-tag-by-position)

;;; TESTING
;;
(defun semantic-find-benchmark ()
  "Run some simple benchmarks to see how we are doing.
Optional argument ARG is the number of iterations to run."
  (interactive)
  (require 'benchmark)
  (let ((f-name nil)
	(b-name nil)
	(f-comp)
	(b-comp)
	(f-regex)
	)
    (garbage-collect)
    (setq f-name
	  (benchmark-run-compiled
	      1000 (semantic-find-first-tag-by-name "class3"
						    "test/test.cpp")))
    (garbage-collect)
    (setq b-name
	  (benchmark-run-compiled
	      1000 (semantic-brute-find-first-tag-by-name "class3"
							  "test/test.cpp")))
    (garbage-collect)
    (setq f-comp
	  (benchmark-run-compiled
	      1000 (semantic-find-tags-for-completion "method"
						      "test/test.cpp")))
    (garbage-collect)
    (setq b-comp
	  (benchmark-run-compiled
	      1000 (semantic-brute-find-tag-by-name-regexp "^method"
							   "test/test.cpp")))
    (garbage-collect)
    (setq f-regex
	  (benchmark-run-compiled
	      1000 (semantic-find-tags-by-name-regexp "^method"
						      "test/test.cpp")))
    
    (message "Name [new old] [ %.3f %.3f ] Complete [newc/new old] [ %.3f/%.3f %.3f ]"
	     (car f-name) (car b-name)
	     (car f-comp) (car f-regex)
	     (car b-comp))
  ))


(provide 'semantic-find)

;;; semantic-find.el ends here
