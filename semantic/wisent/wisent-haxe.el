;;; wisent-haxe.el --- haxe parser support

;; Copyright (C) 2009 Alexander Gavrilov
;;
;; Derived from javascript parser
;; Copyright (C) 2005 Eric M. Ludlam

;; Author: Eric Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: wisent-haxe.el,v 1.2 2009/09/11 18:51:55 zappo Exp $

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Parser support for haxe language.


;;; Code:
(require 'semantic-java)
(require 'semantic-wisent)
(require 'wisent-haxe-wy)

(defun wisent-haxe-expand-tag (tag)
  "Expand TAG into a list of equivalent tags, or nil.
Expand multiple variable declarations in the same statement, that is
tags of class `variable' whose name is equal to a list of elements of
the form (NAME VALUE START . END).  NAME is a variable name.  VALUE is
an initializer START and END are the bounds in the declaration, related
to this variable NAME."
  (let (elts elt value clone start end xpand)
    (when (and (eq 'variable (semantic-tag-class tag))
               (consp (setq elts (semantic-tag-name tag))))
      ;; There are multiple names in the same variable declaration.
      (while elts
        ;; For each name element, clone the initial tag and give it
        ;; the name of the element.
        (setq elt   (car elts)
              elts  (cdr elts)
              clone (semantic-tag-clone tag (car elt))
	      value (car (cdr elt))
              start (if elts  (caddr elt) (semantic-tag-start tag))
              end   (if xpand (cdddr elt) (semantic-tag-end   tag))
              xpand (cons clone xpand))
	;; Set the definition of the cloned tag
	(semantic-tag-put-attribute clone :default-value value)
        ;; Set the bounds of the cloned tag with those of the name
        ;; element.
        (semantic-tag-set-bounds clone start end))
      xpand)))

;;; Override Methods
;;
;; These methods override aspects of how semantic-tools can access
;; the tags created by the haxe parser.
;; Local context
(define-mode-overload-implementation semantic-get-local-variables
  haxe-mode ()
  "Get local values from a specific context.
This function overrides `get-local-variables'."
  ;; Does haxe have identifiable local variables?
  nil)


;;; Setup Function
;;
;; This sets up the haxe parser

(defun reparse-haxe ()
  (interactive)
  (wisent-haxe-setup-parser)
  (senator-force-refresh))

;;;###autoload
(defun wisent-haxe-setup-parser ()
  "Setup buffer for parse."
  (wisent-haxe-wy--install-parser)
  (setq
   ;; Lexical Analysis
   semantic-lex-analyzer 'haxe-lexer
   semantic-lex-number-expression semantic-java-number-regexp
   ;; semantic-lex-depth nil ;; Full lexical analysis
   ;; Parsing
   semantic-tag-expand-function 'wisent-haxe-expand-tag
   ;; Environment
   semantic-imenu-summary-function 'semantic-format-tag-prototype
   imenu-create-index-function 'semantic-create-imenu-index
   semantic-type-relation-separator-character '(".")
   semantic-command-separation-character ";"
   ;; speedbar and imenu buckets name
   semantic-symbol->name-assoc-list-for-type-parts
   ;; in type parts
   '((type     . "Classes")
     (variable . "Variables")
     (function . "Methods"))
   semantic-symbol->name-assoc-list
   ;; everywhere
   (append semantic-symbol->name-assoc-list-for-type-parts
           '((include  . "Imports")
             (package  . "Package")))
   ;; navigation inside 'type children
   senator-step-at-tag-classes '(function variable)
   ))

;;;###autoload
(add-hook 'haxe-mode-hook 'wisent-haxe-setup-parser)

(provide 'wisent-haxe)

;;; wisent-haxe.el ends here
