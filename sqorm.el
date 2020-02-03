;;; sqorm.el --- An ORM for sqlite3 -*- lexical-binding: t -*-
;; Copyright (C) 2020 Lars Magne Ingebrigtsen

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

;; To work, this needs the sqlite3 module
;; https://github.com/syohex/emacs-sqlite3
;; which needs:

;; sudo apt install sqlite3-pcre libsqlite3-dev

;; The Emacs has to be pretty new -- anything older than Emacs 26
;; probably won't work.  And it has to be built with module support.

;;; Code:

(require 'sqlite3)
(require 'cl)

(defvar sqorm-regexp nil
  "If non-nil, sqorm supports regexps in selects.")

(defvar sqorm-db nil
  "The default db.")

(defun sqorm-open (file)
  (let ((db (sqlite3-new (file-truename file))))
    (setq sqorm-db db)
    (when (sqlite3-load-extension db "/usr/lib/sqlite3/pcre.so")
      (setq sqorm-regexp t))
    db))

(defun sqorm-create-tables (tables)
  (loop for (table . columns) in tables
	do (sqlite3-execute-batch
	    sqorm-db (format
		      "create table if not exists %s (%s)"
		      (sqorm-dehyphenate table)
		      (mapconcat
		       #'identity
		       (loop for elem in columns
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
  (sqlite3-execute-batch sqorm-db statement values))

(defun sqorm-column-name (column)
  (replace-regexp-in-string ":" "" (sqorm-dehyphenate column)))

(defun sqorm-insert (object)
  (sqorm-exec
   (format "insert into %s(%s) values(%s)"
	   (sqorm-dehyphenate (getf object :_type))
	   (mapconcat
	    #'identity
	    (loop for (column nil) on (cddr object) by #'cddr
		  collect (sqorm-column-name column))
	    ",")
	   (mapconcat
	    #'identity
	    (loop repeat (/ (length (cddr object)) 2)
		  collect "?")
	    ","))
   (coerce
    (loop for (nil value) on (cddr object) by #'cddr
	  collect value)
    'vector)))

(defun sqorm-select (table &rest values)
  (apply 'sqorm-find table (loop for (key val) on values by #'cddr
				append (list key '= val))))

(defun sqorm-column (column)
  (intern (format ":%s" (replace-regexp-in-string "[^-a-zA-Z0-9]" ""
						  (sqorm-hyphenate column)))
	  obarray))

(defun sqorm-select-where (statement &rest values)
  (let ((result nil))
    (sqlite3-execute
     sqorm-db
     statement
     (coerce values 'vector)
     (lambda (row names)
       (push (nconc (loop for value in row
			  for column in names
			  append (list (sqorm-column column) value)))
	     result)))
    (nreverse result)))

(defun sqorm-find (table &rest values)
  (let ((result nil))
    (sqlite3-execute
     sqorm-db
     (format
      "select * from %s where %s"
      (sqorm-dehyphenate table)
      (mapconcat
       #'identity
       (loop for (column predicate nil) on values by #'cdddr
	     collect (format "%s %s ?"
			     (sqorm-column-name column)
			     predicate))
       " and "))
     (coerce
      (loop for (nil nil value) on values by #'cdddr
	    collect value)
      'vector)
     (lambda (row names)
       (push (nconc (list :_type table)
		    (loop for value in row
			  for column in names
			  append (list
				  (intern (format ":%s" (sqorm-hyphenate column))
					  obarray)
				  value)))
	     result)))
    (nreverse result)))

(defun sqorm-dehyphenate (elem)
  (replace-regexp-in-string "-" "_" (symbol-name elem)))

(defun sqorm-hyphenate (elem)
  (replace-regexp-in-string "_" "-" elem))

(defun sqorm-make (table values tables)
  (nconc (list :_type table)
	 (loop for column in (cdr (assq table tables))
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
