;;;;
;;;; Query the ERIC database with powersets of search terms for the number of articles it has
;;;;
;;;;

;;;
;;; Libraries and parallel init
;;;
(ql:quickload '("trivial-download" "cl-ppcre" "lparallel" "dexador" "plump" "lquery"))
(setf lparallel:*kernel* (lparallel:make-kernel 4))

;;; 
;;; Begin function declarations
;;; 
(defun powerset (xs)
  "Powerset a list without the null set in there."
  (delete '()
  (loop for i below (expt 2 (length xs)) collect
       (loop for j below i for x in xs if (logbitp j i) collect x))))

(defun add-booleans (terms)
  "Insert boolean logic for eric query."
  (loop for term in terms
		     collect  (format nil "AND(\"~{~a~^\" OR \"~}\")" term)))

(let ((url-regex (cl-ppcre:create-scanner "[^a-zA-Z0-9_\\-.]")))
  (defun url-encode (string)
    "URL-encodes a string."
    (flet ((convert (target-string start end match-start match-end reg-starts reg-ends)
             (declare (ignore start end match-end reg-starts reg-ends))
             (format nil "%~2,'0x" (char-code (char target-string match-start)))))
      (cl-ppcre:regex-replace-all url-regex string #'convert))))

(defun make-eric-url (terms)
  "Generate the url to post to eric, with query and options encoded."
  (concatenate 'string
	       "https://eric.ed.gov/?q="
	       (cl-ppcre:regex-replace-all
		" " (url-encode (subseq (format nil "~{~a~^ ~}" (add-booleans terms)) 3)) "+")
	       "&pr=on&ff1=dtySince_2017"))

(defun download-eric-page (query index)
  "Download the resulting page from ERIC for caching."
  (defvar *url* (make-eric-url query))
  (trivial-download:download *url* (format nil "/tmp/eric~a.html" index)))
  

(defun number-of-eric-papers (query)
  "Return the number of papers eric has that fits a query"
  (defvar *url* (make-eric-url query))
  (defvar *request* (dex:get *url*))
  (setq *parsed-content* (lquery:$ (initialize (dex:get (make-eric-url query)))))
  (setq results (aref (lquery:$ *parsed-content* "#rr0" (text)) 0))
  ;; (print *url*)
  ;; (print results)
  (if (search "No results matched your query." results)
      (list query 0)
      (let* ((start-index
	       (+ 19 (or 
		      (search "Showing 1 to 15 of " results)
		      (search "Showing one result " results))))
	     (end-index (search " " results :start2 start-index)))
	(list query (subseq results start-index end-index)))))
;;; 
;;; End function declarations
;;; 


;;; Search queries, list elements are or'd together and lists are and'd between eachother
;;; ((a b) (1 2)) -> (a OR b) AND (1 OR 2)
(setq levels '("higher education" "college" "postsecondary education"))
(setq pandemic '("pandemic" "COVID-19") )
(setq hands '("hands on" "laboratory") )
(setq tech '("online courses" "online learning" "educational technology" "distance education" "electronic learning") )
(setq aviation '("FAA" "part 147"))

(setq all-queries
(loop for i in (powerset levels) nconcing
;; (loop for j in (powerset pandemic) collect (list i j))))
(loop for j in (powerset pandemic) nconcing
(loop for k in (powerset hands) nconcing
(loop for l in (powerset tech) nconcing
(loop for m in (powerset aviation) collect (list i j k l m)))))))
;(setq all-urls (mapcar #'make-eric-url all-queries))

; (setq test '(("college" "postsecondary education") ("pandemic") ("hands on" "laboratory") ("online courses" "distance education") ("FAA")))
; (setq all-queries
;       (loop for i in (powerset tech) collect (list i)))

;;; Download a bunch to disk for later processing
;;(loop for query in all-queries 
;;      for i from 0 do
;;      (download-eric-page query i))
;;    (number-of-eric-papers query))


;;; Save the results of a parallel hit to ERIC to a file
(with-open-file (stream (merge-pathnames #p"eric-data.txt"
					 (user-homedir-pathname))
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
  (format stream "~S" (lparallel:pmapcar #'number-of-eric-papers all-queries)))
