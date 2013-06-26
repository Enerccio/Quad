(in-package :quad-server)

(defparameter *web-package* :quad-server
  "Default package used when parsing and assembling the htcl webpages with 
embedded cl.")

(defun transform-for-web (form)
  (let (result)
    (dolist (f form)
      (if (typep f 'string)
	  (push (let ((g (gensym)))
		  `(let ((,g ,f))
		     (format t "~A" ,g) ,g)) result)
	  (push f result)))
    (cons 'progn result)))

(defun web-read (istream &optional (eof-error-p nil) (eof-value nil) (recursive nil))
  (log:trace)
  (let (form 
	(str (make-array 1 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop 
       (let ((char (read-char istream nil eof-value recursive)))
	 (when (equal char eof-value)
	   (push str form)
	   (return-from web-read (transform-for-web form)))
	 (cond ((char= char #\<)
		(let ((second (read-char istream nil eof-value recursive)))
		  (when (equal second eof-value)
		    (return-from web-read eof-value))
		  (cond ((char= second #\%)
			 (when recursive
			   (push str form)
			   (return-from web-read (transform-for-web form)))
			 (push str form)
			 (setf str (make-array 1 :element-type 'character :adjustable t 
					       :fill-pointer 0))
			 (let ((*readtable* (copy-readtable)))
			   (set-macro-character #\% (lambda (stream char)
						      (let ((c (read-char stream)))
							(if (char= c #\>)
							    (web-read stream nil nil t)
							    (progn
							      (unread-char c)
							      (unread-char char))))))
			   (log:trace "Calling normal reader")
			   (push (read istream t nil t) form)))
			(t
			 (vector-push-extend char str)
			 (vector-push-extend second str)))))
	       ((char= char #\%)
		(let ((second (read-char istream nil eof-value recursive)))
		  (cond ((char= second #\>))
			(t (vector-push-extend char str)
			   (vector-push-extend second str)))))
	       (t (vector-push-extend char str)))))))

(defparameter *is-inc* nil
  "Is non nil when code is being included")

(defun run-lisp-interpret (istream ostream)
  "Runs the lisp interpret with form."
  (log:trace "Starting lisp interpret " istream ostream)
  (let ((*standard-output* ostream)
	(*debug-output* ostream)
	(*package* (find-package *web-package*)))
    (log:trace "Starting eval & read")
    (handler-case 
	(let ((form (web-read istream)))
	  (log:trace "Compiled form ready")
	  (log:trace form)
	  (eval form))
      (error (e) 
	(format ostream "<b>~A</u></i></b>"
		e))))
  (log:trace "Finished"))

; lisp api
; These are functions heavily used by the api of inline lisp
(defun get-post-arg (arg)
  "Returns post arg from *request*"
  (unless (request-post *request*)
    nil)
  (when (typep (request-post *request*) 'array)
    (setf (request-post *request*)
	  (parse-args (stringify (request-post *request*)))))
  (cdr (find arg (request-post *request*) :key #'car :test #'string=)))

(defun get-get-args (arg)
  "Returns get arg which can be nil, so use second value if you want to 
check if argument was present."
  (let ((farg (find arg (request-args *request*) :key #'car :test #'string=)))
    (values (cdr farg) (car farg))))

(defun get-header-arg (arg)
  "Returns header arg or nil."
  (let ((val
	 (cdr (find arg (request-h-args *request*) :key #'car :test #'string=))))
    (if (> (length val) 0)
	(subseq val 1)
	val)))

(defun write-to-header (field value)
  "Adds field and value to header."
  (format *header* "~A: ~A" field value)
  (terpri-http *header*))

(defun redirect-denied ()
  "Immediatelly signals access denied condition"
  (error 'access-denied :code :403))

(defun parse-cookies ()
  "Return parsed cookies."
  (let ((cookies (get-header-arg "Cookie")))
    (when cookies
      (parse-args cookies #\;))))

(defun include (path)
  "Includes / prefixed path as if it was loaded as htcl."
  (let ((path (parse-path *server* path)) (*in-inc* t))
    (with-open-file (istream path :direction :input)
      (parse-inline-from-stream istream *standard-output*))))

(let ((loaded-pages (make-hash-table))
      (lock (mp:make-lock :name 'include-lock)))
  (defun include-once (path)
    "Includes the selected path (which should be / prefixed) but only once"
    (let ((path (parse-path *server* path)))
      (mp:with-lock (lock)
	(let ((res (gethash path loaded-pages)) (*in-inc* t))
	  (unless res
	    (setf (gethash path loaded-pages) t)
	    (with-open-file (istream path :direction :input)
	      (parse-inline-from-stream istream *standard-output*)))))))
  (defun return-loaded-pages-map ()
    "Debug only function"
    loaded-pages))

(defun is-included ()
  "Returns true when page is being included, nil if not."
  *in-inc*)    
