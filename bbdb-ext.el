;;; bbdb-ext.el --- Extra commands for BBDB

;; Filename: bbdb-ext.el
;; Description: Extra commands for BBDB
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: 2010
;; Version: 0.1
;; Package-Version: 20130513.1152
;; Last-Updated: 2015-12-20 18:00:00
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/bbdb-ext
;; Keywords: extensions
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: ((bbdb "2.36"))
;;
;; Features that might be required by this library:
;;
;; bbdb
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 1GWis4cd4aokQrWxAd3ZASrsye5CGq5LJY
;;
;;  This file contains some extra commands for BBDB. Here is a list of the keybindings:

;; "g"    : Search address of current record on google maps
;; "S d"  : Show records with address field matching regexp (use "^$" to show records with no address)
;; "/ S"  : Show records that match regexp in either name, company, network, address or notes fields
;; "/ n"  : Show records with name field matching regexp
;; "/ c"  : Show records with company field matching regexp
;; "/ a"  : Show records with network address field matching regexp
;; "/ o"  : Show records with notes field matching regexp
;; "/ p"  : Show records with phones field matching regexp
;; "/ d"  : Show records with address field matching regexp (use "^$" to show records with no address)
;; "/ /"  : Show all records

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `bbdb-google-map'
;;    Search REC's address field using Google Maps.
;;  `bbdb-recursive-search'
;;    Display all entries in the *BBDB* buffer matching the REGEX in either the name(s), company, network address, or notes.
;;  `bbdb-recursive-search-name'
;;    Display all entries in the *BBDB* buffer matching the REGEX in the name (or ``alternate'' names) field.
;;  `bbdb-recursive-search-company'
;;    Display all entries in *BBDB* buffer matching REGEX in the company field.
;;  `bbdb-recursive-search-net'
;;    Display all entries in *BBDB* buffer matching regexp REGEX in the network address.
;;  `bbdb-recursive-search-xfields'
;;    Display all entries in *BBDB* buffer matching REGEX in the named xfields field.
;;  `bbdb-recursive-search-phones'
;;    Display all entries in *BBDB* buffer matching the REGEX in the phones field.
;;  `bbdb-recursive-search-address'
;;    Display all entries in the *BBDB* buffer matching the REGEX in the address fields.
;;  `bbdb-disable-all-limits'
;;    Display all entries in BBDB database.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;


;;; Installation:
;;
;; Put bbdb-ext.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'bbdb-ext)

;;; Change log:
;;	
;; 2013/05/13
;;      * Update documentation, tidy code, and post to Marmalade.
;; 
;; 2010
;;      * First released

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;;
;; Fix `bbdb-recursive-search' to work with phone field aswell
;;

;;; Require

(require 'bbdb)

;;; Code:
(require 'bbdb)

(defun bbdb-ext-hook ()
  (define-key bbdb-mode-map (kbd "G") 'bbdb-google-map)
  (define-key bbdb-mode-map (kbd "S d") 'bbdb-search-address)
  (define-key bbdb-mode-map (kbd "/ S") 'bbdb-recursive-search)
  (define-key bbdb-mode-map (kbd "/ n") 'bbdb-recursive-search-name)
  (define-key bbdb-mode-map (kbd "/ c") 'bbdb-recursive-search-company)
  (define-key bbdb-mode-map (kbd "/ e") 'bbdb-recursive-search-net)
  (define-key bbdb-mode-map (kbd "/ x") 'bbdb-recursive-search-xfields)
  (define-key bbdb-mode-map (kbd "/ p") 'bbdb-recursive-search-phones)
  (define-key bbdb-mode-map (kbd "/ a") 'bbdb-recursive-search-address)
  (define-key bbdb-mode-map (kbd "/ /") 'bbdb-disable-all-limits)
  (define-key bbdb-mode-map (kbd "g") 'bbdb-disable-all-limits))

(add-hook 'bbdb-mode-hook 'bbdb-ext-hook)

;;;###autoload
(defun bbdb-google-map (&optional rec)
  "Search REC's address field using Google Maps.
If REC is nil, the current record will be used.
If there is no address filed for REC, a message will be given in minibuffer.
If there are several addresses for REC, the address nearest point will be used."
  (interactive)
  (or rec (setq rec (bbdb-current-record)))
  (unless rec (error "No current record"))
  ;; now we should get the address of REC
  (let ((address (bbdb-gm-address rec)))
    (if address
	(browse-url (bbdb-gm-build-url address))
      (message "No address for current entry!"))))

(defun bbdb-gm-address (rec)
  "Get the address that will be used by google maps for REC.
If there is no address filed for rec, nil will be returned.
If there are several addresses for REC, the address nearest point will be used."
  ;;  (let ((addresses (bbdb-record-address rec)))
  (let ((addresses (bbdb-record-address rec)))
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
	    (bbdb-next-field 1))
	  (if (zerop i)
	      (car addresses)
	    (nth (1- i) addresses)))))))

(defun bbdb-gm-build-url (addr)
  "Build the url string according to ADDR."
  (let* ((base "http://maps.google.com/maps?q=")
	 (streets (mapconcat (lambda (arg) arg) (bbdb-address-streets addr) " "))
	 (city (bbdb-address-city addr))
	 (state (bbdb-address-state addr))
	 (country (bbdb-address-country addr))
	 (url (concat base (url-hexify-string
                            (mapconcat (lambda (arg) arg)
                                       (remove-if (lambda (str)
                                                    (string-match "^[ \t\n]*$" str))
                                                  (list streets city state country)) ",")))))
    url))

;;;###autoload
;; FIXME: this doesn't search based on phone fields, why?
(defun bbdb-recursive-search (regex)
  "Display all entries in the *BBDB* buffer matching the REGEX in either the name(s), company, network address, or notes."
  (interactive (list (bbdb-search-read "")))
  (let* ((notes (cons '* regex))
	 (records (bbdb-search (bbdb-ext-displayed-records) regex regex regex notes regex regex)))
    (if records
	(bbdb-display-records records)
      (message "No records matching '%s'" regex))))

;;;###autoload
(defun bbdb-recursive-search-name (regex)
  "Display all entries in the *BBDB* buffer matching the REGEX in the name \(or ``alternate'' names\) field."
  (interactive (list (bbdb-search-read "names")))
  (bbdb-display-records (bbdb-search (bbdb-ext-displayed-records) regex)))

;;;###autoload
(defun bbdb-recursive-search-company (regex)
  "Display all entries in *BBDB* buffer matching REGEX in the company field."
  (interactive (list (bbdb-search-read "company")))
  (bbdb-display-records (bbdb-search (bbdb-ext-displayed-records) nil regex)))

;;;###autoload
(defun bbdb-recursive-search-net (regex)
  "Display all entries in *BBDB* buffer matching regexp REGEX in the network/email address."
  (interactive (list (bbdb-search-read "net address")))
  (bbdb-display-records (bbdb-search (bbdb-ext-displayed-records) nil nil regex)))

;;;###autoload
(defun bbdb-recursive-search-xfields (field regexp &optional layout)
  "Display all BBDB records for which xfield FIELD matches REGEXP."
  (interactive
   (let ((field (completing-read "Xfield to search (RET for all): "
                                 (mapcar 'list bbdb-xfield-label-list) nil t)))
     (list (if (string= field "") '* (intern field))
           (bbdb-search-read (if (string= field "")
				 "any xfield"
			       field))
           (bbdb-layout-prefix))))
  (bbdb-display-records (bbdb-search (bbdb-ext-displayed-records) nil nil nil
                                     (cons field regexp))
                        layout))

;;;###autoload
(defun bbdb-recursive-search-phones (regex)
  "Display all entries in *BBDB* buffer matching the REGEX in the phones field."
  (interactive (list (bbdb-search-read "phone")))
  (bbdb-display-records (bbdb-search (bbdb-ext-displayed-records) nil nil nil nil regex)))

;;;###autoload
(defun bbdb-recursive-search-address (regex)
  "Display all entries in the *BBDB* buffer matching the REGEX in the address fields."
  (interactive (list (bbdb-search-read "address")))
  (bbdb-display-records (bbdb-search (bbdb-ext-displayed-records) nil nil nil nil nil regex)))

(defun bbdb-disable-all-limits nil
  "Display all entries in BBDB database."
  (interactive)
  (bbdb ""))

(defun bbdb-ext-displayed-records nil
  "Return a list of all bbdb records in *BBDB* buffer."
  (mapcar (lambda (rec) (car rec)) bbdb-records))

(provide 'bbdb-ext)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "bbdb-ext.el" (buffer-name) (buffer-string) "update")

;;; bbdb-ext.el ends here
