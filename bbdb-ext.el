;;; bbdb-ext.el --- Some extension functions of BBDB

;; Copyright (C) 2010  

;; Author: 
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  This file contains some extension functions for BBDB.

;;  `bbdb-google-map' will search current record's address field using google map.
;;  `bbdb-recursive-search' will search based on all fields of recordes that are
;;   in current *BBDB* buffer.
;;  Also there are functions that search records based on different fields (name,
;;  company, etc):
;;  `bbdb-recursive-search-name'
;;  `bbdb-recursive-search-company'
;;  `bbdb-recursive-search-net'
;;  `bbdb-recursive-search-notes'
;;  `bbdb-recursive-search-phones'

;;  All the recursive search functions' keybinding are prefixed by '/-', and similar
;;  to ibuffer's limit selecting, there is a function `bbdb-disable-all-limits' (also
;;  bound to '/ /'), which will show all the records in BBDB database.

;;; Code:
(require 'bbdb)

(defun bbdb-ext-hook ()
  (define-key bbdb-mode-map [(g)] 'bbdb-google-map)
  (define-key bbdb-mode-map (kbd "/ S") 'bbdb-recursive-search)
  (define-key bbdb-mode-map (kbd "/ n") 'bbdb-recursive-search-name)
  (define-key bbdb-mode-map (kbd "/ c") 'bbdb-recursive-search-company)
  (define-key bbdb-mode-map (kbd "/ a") 'bbdb-recursive-search-net)
  (define-key bbdb-mode-map (kbd "/ o") 'bbdb-recursive-search-notes)
  (define-key bbdb-mode-map (kbd "/ p") 'bbdb-recursive-search-phones)
  (define-key bbdb-mode-map (kbd "/ /") 'bbdb-disable-all-limits))

(add-hook 'bbdb-mode-hook 'bbdb-ext-hook)

;;;###autoload
(defun bbdb-google-map (&optional rec)
  "Search REC's address field using Goole Map.
If REC is `nil', current record will be used.
If there is no address filed for REC, an message will be given in minibuffer.
If there are several addresses for REC, the address nearest point will be used."
  (interactive)
  (or rec (setq rec (bbdb-current-record)))

  (unless rec
    (error "No current record"))

  ;; now we should get the address of REC
  (let ((address (bbdb-gm-address rec)))
    (if address
	(bbdb-gm-search (bbdb-gm-build-url address))
      (message "No address for current entry!"))))

(defun bbdb-gm-address (rec)
  "Get the address that will be used by google map for REC.
If there is no address filed for rec, `nil' will be returned.
If there are several addresses for REC, the address nearset point will be used."
  (let ((addresses (bbdb-record-addresses rec)))
    (when addresses
      (save-excursion
	(let ((prop (bbdb-current-field))
	      (p (point))
	      (i 0))
	  
	  (while (and prop (not (eq 'name (car prop))))
	    (bbdb-next-field -1)
	    (setq prop (bbdb-current-field)))
	  
	  (while (<= (point) p)
	    (setq prop (bbdb-current-field))
	    (if (eq 'address (car prop))
		(progn
		  ;; For some records, `bbdb-next-field' doesn't work properly
		  ;; when (= 2 (length prop)) and `bbdb-next-field' is called
		  ;; it doesn't move to the next field, it's still in the same record
		  ;; but (= 3 (length prop)).
		  ;; So when it's an address field, (= 2 (length prop)) marks a real
		  ;; address field.
		  (if (= 2 (length prop))
		      (setq i (1+ i)))))
	    (bbdb-next-field))
	  
	  (if (zerop i)
	      (car addresses)
	    (nth (1- i) addresses)))))))

(defun bbdb-gm-search (url)
  "Search the URL."
  (browse-url url))

(defun bbdb-gm-build-url (addr)
  "Build the url string according to ADDR."
  (let* ((base "http://maps.google.com/maps?q=")
	 (streets (mapconcat (lambda (arg) arg) (bbdb-address-streets addr) " "))
	 (city (bbdb-address-city addr))
	 (state (bbdb-address-state addr))
	 (country (bbdb-address-country addr))
	 (url (concat base (url-hexify-string (mapconcat (lambda (arg) arg)
							 (remove-if (lambda (str)
								      (string-match "^[ \t\n]*$" str))
								    (list streets city state country)) ",")))))
    url))

;;;###autoload
(defun bbdb-recursive-search (string elidep)
  "Display all entries in the *BBDB* buffer matching the regexp STRING
in either the name(s), company, network address, or notes."
  (interactive
   (list (bbdb-search-prompt "Search records %m regexp: ")
	 current-prefix-arg))
  (let* ((bbdb-display-layout (bbdb-grovel-elide-arg elidep))
	 (notes (cons '* string))
	 (records
	  (bbdb-search (bbdb-ext-displayed-records) string string string notes
		       nil)))
    (if records
	(bbdb-display-records records)
      ;; we could use error here, but it's not really an error.
      (message "No records matching '%s'" string))))

;;;###autoload
(defun bbdb-recursive-search-name (string elidep)
  "Display all entries in the *BBDB* buffer matching the regexp STRING in the name
\(or ``alternate'' names\)."
  (interactive
   (list (bbdb-search-prompt "Search records with names %m regexp: ")
	 current-prefix-arg))
   (let ((bbdb-display-layout (bbdb-grovel-elide-arg elidep)))
    (bbdb-display-records (bbdb-search (bbdb-ext-displayed-records) string))))

;;;###autoload
(defun bbdb-recursive-search-company (string elidep)
  "Display all entries in *BBDB* buffer matching STRING in the company field."
  (interactive
   (list (bbdb-search-prompt "Search records with company %m regexp: ")
	 current-prefix-arg))
  (let ((bbdb-display-layout (bbdb-grovel-elide-arg elidep)))
    (bbdb-display-records (bbdb-search (bbdb-ext-displayed-records) nil string))))

;;;###autoload
(defun bbdb-recursive-search-net (string elidep)
  "Display all entries in *BBDB* buffer matching regexp STRING in the network address."
  (interactive
   (list (bbdb-search-prompt "Search records with net address %m regexp: ")
	 current-prefix-arg))
  (let ((bbdb-display-layout (bbdb-grovel-elide-arg elidep)))
    (bbdb-display-records (bbdb-search (bbdb-ext-displayed-records) nil nil string))))

;;;###autoload
(defun bbdb-recursive-search-notes (which string elidep)
  "Display all entries in *BBDB* buffer matching STRING in the named notes field."
  (interactive
   (let (field)
     (list (setq field (completing-read "Notes field to search (RET for all): "
					(append '(("notes")) (bbdb-propnames))
					nil t))
	   (bbdb-search-prompt "Search records with %s %m regexp: "
			       (if (string= field "")
				   "one field"
				 field))
	   current-prefix-arg)))
  (let ((bbdb-display-layout (bbdb-grovel-elide-arg elidep))
	(notes (if (string= which "")
		   (cons '* string)
		 (cons (intern which) string))))
    (bbdb-display-records (bbdb-search (bbdb-ext-displayed-records) nil nil nil notes))))

;;;###autoload
(defun bbdb-recursive-search-phones (string elidep)
  "Display all entries in *BBDB* buffer matching the regexp STRING in the phones field."
  (interactive
   (list (bbdb-search-prompt "Search records with phone %m regexp: ")
	 current-prefix-arg))
  (let ((bbdb-display-layout (bbdb-grovel-elide-arg elidep)))
    (bbdb-display-records
     (bbdb-search (bbdb-ext-displayed-records) nil nil nil nil string))))

(defun bbdb-disable-all-limits ()
  "Display all entries in BBDB database."
  (interactive)
  (bbdb "" current-prefix-arg))

(defun bbdb-ext-displayed-records ()
  "Return a list of all bbdb records in *BBDB* buffer."
  (mapcar (lambda (rec) (car rec)) bbdb-records))

(provide 'bbdb-ext)
;;; bbdb-ext.el ends here
