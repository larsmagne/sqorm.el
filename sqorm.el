;;; sqorm.el --- An ORM for sqlite -*- lexical-binding: t -*-
;; Copyright (C) 2020-2024 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: movies

;; This file is not part of GNU Emacs.

;; sqorm.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sqorm.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The Emacs has to be pretty new -- anything older than Emacs 29
;; probably won't work.

;;; Code:

(require 'cl-lib)

(defvar sqorm-regexp nil
  "If non-nil, sqorm supports regexps in selects.")

(defvar sqorm-db nil
  "The default db.")

(defun sqorm-open (file)
  (let ((db (sqlite-open file)))
    (setq sqorm-db db)
    db))

(defun sqorm-create-tables (tables)
  (cl-loop for (table . columns) in tables
	   do (sqlite-execute
	       sqorm-db
	       (format
		"create table if not exists %s (%s)"
		(sqorm-dehyphenate table)
		(mapconcat
		 #'identity
		 (cl-loop for elem in columns
			  collect (format
				   "%s %s%s%s"
				   (sqorm-dehyphenate (car elem))
				   (if (equal (cadr elem) 'bool)
				       "text"
				     (cadr elem))
				   (if (memq :primary elem)
				       " primary key"
				     "")
				   (let ((references
					  (cadr (memq :references elem))))
				     (if references
					 (format " references %s"
						 references)
				       ""))))
		 ", ")))))

(defun sqorm-exec (statement values)
  (sqlite-execute sqorm-db statement values))

(defun sqorm-column-name (column)
  (replace-regexp-in-string ":" "" (sqorm-dehyphenate column)))

(defun sqorm-insert (object)
  (sqorm-exec
   (format "insert into %s(%s) values(%s)"
	   (sqorm-dehyphenate (cl-getf object :_type))
	   (mapconcat
	    #'identity
	    (cl-loop for (column nil) on (cddr object) by #'cddr
		     collect (sqorm-column-name column))
	    ",")
	   (mapconcat
	    #'identity
	    (cl-loop repeat (/ (length (cddr object)) 2)
		     collect "?")
	    ","))
   (cl-loop for (nil value) on (cddr object) by #'cddr
	    collect value)))

(defun sqorm-select (table &rest values)
  (apply 'sqorm-find table (cl-loop for (key val) on values by #'cddr
				    append (list key '= val))))

(defun sqorm-column (column)
  (intern (format ":%s" (replace-regexp-in-string "[^-a-zA-Z0-9]" ""
						  (sqorm-hyphenate column)))
	  obarray))

(defun sqorm-select-where (statement &rest values)
  (sqorm--transform-result (sqlite-select sqorm-db statement values 'full)))

(defun sqorm--transform-result (result)
  (cl-loop with columns = (pop result)
	   for row in result
	   collect (cl-loop for column in columns
			    for value in row
			    append (list (sqorm-column column) value))))

(defun sqorm-find (table &rest values)
  (sqorm--transform-result
   (sqlite-select
    sqorm-db
    (format
     "select * from %s where %s"
     (sqorm-dehyphenate table)
     (mapconcat
      #'identity
      (cl-loop for (column predicate nil) on values by #'cdddr
	       collect (format "%s %s ?"
			       (sqorm-column-name column)
			       predicate))
      " and "))
    (cl-loop for (nil nil value) on values by #'cdddr
	     collect value)
    'full)))

(defun sqorm-dehyphenate (elem)
  (replace-regexp-in-string "-" "_" (symbol-name elem)))

(defun sqorm-hyphenate (elem)
  (replace-regexp-in-string "_" "-" elem))

(defun sqorm-make (table values tables)
  (nconc (list :_type table)
	 (cl-loop for column in (cdr (assq table tables))
		  for value in values
		  for type = (cadr column)
		  append (list (intern (format ":%s" (car column)) obarray)
			       (cond
				((and value
				      (or (eq type 'integer)
					  (eq type 'number)
					  (eq type 'float)))
				 (string-to-number value))
				((eq type 'bool)
				 (if (equal value "0")
				     "N"
				   "Y"))
				(t
				 value))))))

(provide 'sqorm)

;;; sqorm.el ends here
