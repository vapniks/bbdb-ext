;;; bbdb-ext.el --- Some extension functions of BBDB

;; Copyright (C) 2010

;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Keywords: bbdb

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
;;  `bbdb-recursive-search-address'

;;  `bbdb-address' will search based on the address fields of all the records in BBDB.
;;  It's bound to "S d". Similar to other search functions (bbdb-name, bbdb-notes, etc)
;;  you can use ^$ to find the records that don't have addresses.

;;  All the recursive search functions' keybinding are prefixed by '/-', and similar
;;  to ibuffer's limit selecting, there is a function `bbdb-disable-all-limits' (also
;;  bound to '/ /'), which will show all the records in BBDB database.

;;; Code:
(require 'bbdb)

(defun bbdb-ext-hook ()
  (define-key bbdb-mode-map [(g)] 'bbdb-google-map)
  (define-key bbdb-mode-map (kbd "S d") 'bbdb-address)
  (define-key bbdb-mode-map (kbd "/ S") 'bbdb-recursive-search)
  (define-key bbdb-mode-map (kbd "/ n") 'bbdb-recursive-search-name)
  (define-key bbdb-mode-map (kbd "/ c") 'bbdb-recursive-search-company)
  (define-key bbdb-mode-map (kbd "/ a") 'bbdb-recursive-search-net)
  (define-key bbdb-mode-map (kbd "/ o") 'bbdb-recursive-search-notes)
  (define-key bbdb-mode-map (kbd "/ p") 'bbdb-recursive-search-phones)
  (define-key bbdb-mode-map (kbd "/ d") 'bbdb-recursive-search-address)
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
;; FIXME: this doesn't search based on phone fields, why?
(defun bbdb-recursive-search (string elidep)
  "Display all entries in the *BBDB* buffer matching the regexp STRING
in either the name(s), company, network address, or notes."
  (interactive
   (list (bbdb-search-prompt "Search records %m regexp: ")
	 current-prefix-arg))
  (let* ((bbdb-display-layout (bbdb-grovel-elide-arg elidep))
	 (notes (cons '* string))
	 (records
	  (bbdb-ext-search (bbdb-ext-displayed-records) string string string notes
			   string string)))
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

;;;###autoload
(defun bbdb-recursive-search-address (string elidep)
  "Display all entries in the *BBDB* buffer matching the regexp STRING in the address fields."
  (interactive
   (list (bbdb-search-prompt "Search records with address %m regexp: ")
	 current-prefix-arg))
  (let ((bbdb-display-layout (bbdb-grovel-elide-arg elidep)))
    (bbdb-display-records (bbdb-ext-search (bbdb-ext-displayed-records) nil nil nil nil nil string))))

(defun bbdb-disable-all-limits ()
  "Display all entries in BBDB database."
  (interactive)
  (bbdb "" current-prefix-arg))

(defun bbdb-ext-displayed-records ()
  "Return a list of all bbdb records in *BBDB* buffer."
  (mapcar (lambda (rec) (car rec)) bbdb-records))

;;;###autoload
(defun bbdb-address (string elidep)
  "Display all entries in the BBDB matching the regexp STRING in the address field."
  (interactive
   (list (bbdb-search-prompt "Search records with address %m regexp: ")
	 current-prefix-arg))
  (let ((bbdb-display-layout (bbdb-grovel-elide-arg elidep)))
    (bbdb-display-records (bbdb-ext-search (bbdb-records) nil nil nil nil nil string))))

(defmacro bbdb-ext-search (records &optional name company net notes phone address)
  "Search RECORDS for optional arguments NAME, COMPANY, NET, NOTES, PHONE, ADDRESS.
This macro only emits code for those things being searched for;
literal nils at compile-time cause no code to be emitted.

If you want to reverse the search, bind `bbdb-search-invert' to t."
  (let (clauses)
    ;; I didn't protect these vars from multiple evaluation because that
    ;; actually generates *less efficient code* in elisp, because the extra
    ;; bindings can't easily be optimized away without lexical scope.  fmh.
    (or (stringp name) (symbolp name) (error "name must be atomic"))
    (or (stringp company) (symbolp company) (error "company must be atomic"))
    (or (stringp net) (symbolp net) (error "net must be atomic"))
    (or (stringp notes) (symbolp notes) (error "notes must be atomic"))
    (or (stringp phone) (symbolp phone) (error "phone must be atomic"))
    (or (stringp address) (symbolp address) (error "address must be atomic"))
    (if address
	(setq clauses
	      (cons
	       (` (let ((address-fields (bbdb-address-fields record))
			(done nil))
		    (if address-fields
			(while (and address-fields (not done))
			  (setq done (string-match (, address)
						   (car address-fields))
				address-fields (cdr address-fields)))
		      ;; so that "^$" can be used to find entries that
		      ;; have no address
		      (setq done (string-match (, address) "")))
		    done))
	       clauses)))
    (if phone
	(setq clauses
	      (cons
	       (` (let ((rest-of-phones (bbdb-record-phones record))
			(done nil))
		    (if rest-of-phones
			(while (and rest-of-phones (not done))
			  (setq done (string-match (, phone)
						   ;; way way wasteful...
						   (bbdb-phone-string
						    (car rest-of-phones)))
				rest-of-phones (cdr rest-of-phones)))
		      ;; so that "^$" can be used to find entries that
		      ;; have no phones
		      (setq done (string-match (, phone) "")))
		    done))
	       clauses)))
    (if notes
	(setq clauses
	      (cons
	       (` (if (stringp (, notes))
		      (string-match (, notes)
				    (or (bbdb-record-notes record) ""))
		    (if (eq (car (, notes)) '*)
			(let ((fields all-fields) done tmp)
			  (if (bbdb-record-raw-notes record)
			      (while (and (not done) fields)
				(setq tmp (bbdb-record-getprop
					   record (car fields))
				      done (and tmp (string-match
						     (cdr (, notes))
						     tmp))
				      fields (cdr fields)))
			    ;; so that "^$" can be used to find entries that
			    ;; have no notes
			    (setq done (string-match (cdr (, notes)) "")))
			  done)
		      (string-match (cdr (, notes))
				    (or (bbdb-record-getprop
					 record (car (, notes))) "")))))
	       clauses)))
    (if name
	(setq clauses
	      (append
	       (` ((string-match (, name) (or (bbdb-record-name record) ""))
		   (let ((rest-of-aka (bbdb-record-aka record))
			 (done nil))
		     (while (and rest-of-aka (not done))
		       (setq done (string-match (, name) (car rest-of-aka))
			     rest-of-aka (cdr rest-of-aka)))
		     done)))
	       clauses)))
    (if net
	(setq clauses
	      (cons
	       (` (let ((rest-of-nets (bbdb-record-net record))
			(done nil))
		    (if rest-of-nets
			(while (and rest-of-nets (not done))
			  (setq done (string-match (, net) (car rest-of-nets))
				rest-of-nets (cdr rest-of-nets)))
		      ;; so that "^$" can be used to find entries that
		      ;; have no net addresses.
		      (setq done (string-match (, net) "")))
		    done))
	       clauses)))
    (if company
	(setq clauses
	      (cons
	       (` (string-match (, company)
				(or (bbdb-record-company record) "")))
	       clauses)))

    (` (let ((matches '())
	     (,@ (if notes
		     '((all-fields (cons 'notes
					 (mapcar (lambda (x) (intern (car x)))
						 (bbdb-propnames)))))
		   nil))
	     (case-fold-search bbdb-case-fold-search)
	     (records (, records))
	 (invert (bbdb-search-invert-p))
	     record)
	 (while records
	   (setq record (car records))
       (if (or (and invert
	    (not (or (,@ clauses))))
	   (and (not invert)
	    (or (,@ clauses))))
	   (setq matches (cons record matches)))
       (setq records (cdr records)))
	 (nreverse matches)))))

(defun bbdb-address-fields (record)
  "Get all the address fields of RECORD.
A list of string will be returned."
  (let ((addrs (bbdb-record-addresses record))
	(fields nil))
    (while addrs
      (let* ((addr (car addrs))
	     (location (bbdb-address-location addr))
	     (streets (bbdb-address-streets addr))
	     (city (bbdb-address-city addr))
	     (state (bbdb-address-state addr))
	     (zip (bbdb-address-zip addr))
	     (country (bbdb-address-country addr)))
	(when location
	  (push location fields))
	(when streets			; streets is a list of strings
	  (setq fields (append streets fields)))
	(when city
	  (push city fields))
	(when state
	  (push state fields))
	(when zip
	  (push zip fields))
	(when country
	  (push country fields)))
      (setq addrs (cdr addrs)))
    (remove "" fields)))
(provide 'bbdb-ext)
;;; bbdb-ext.el ends here
