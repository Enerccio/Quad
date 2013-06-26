;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(eval-when (:compile-toplevel :load-toplevel)
  (load "package.lisp"))

(in-package :quad-server)

;;;;;;;;;;;;;
; Locks     ;
;;;;;;;;;;;;;
(defparameter *session-lock*
  (mp:make-lock))

;;;;;;;;;;;;;;
; Structures ;
;;;;;;;;;;;;;;

; structure containing request information 
(defstruct request
  path  ; file path on the disk to the resource
  file  ; file itself, as string, with extension
  extension ; extension in string without .
  mime-type ; mime type, if resolved
  h-args    ; arguments extracted from header as alist
  args      ; arguments from url?xxx as alist
  post      ; post arguments, as binary stream
  sessid    ; session id
  type)     ; type of structure, :get :post are possible values

; structure containing session information
(defstruct session
  timestamp ; contains timestamp of the session
  datastamp ; contains datastamp of the session
  data)     ; contains hash map of the data

;;;;;;;;;;;;;;;;
; Server class ;
;;;;;;;;;;;;;;;;

; Server class is wrapper around server socket with configuration inside
(defclass server (inet-socket)
  ((port  :initarg :port     :reader port            :initform 8080)
   (home  :initarg :home-dir :reader home-dir        :initform "/www/public_html/")
   (sys   :initarg :sys-dir  :reader sys-dir         :initform "/www/") 
   (vers  :initarg :version  :reader server-version  :initform "QUAD-HTCL/0.0.1")
   (mimes :initarg :mime     :reader mime            :initform (standard-mime-types))
   (sess  :initarg :sessions :reader sessions        :initform (make-hash-table 
								:test #'equal))
   (sest  :initarg :sesstime :reader session-timeout :initform 600)
   (sadmm :initarg :adminm   :reader server-admin    :initform "webmaster@example.com")))

; Server generics
(defgeneric get-session-from-request (server socket request)
  (:documentation "Returns sessions info for current connection, if any. Nil if none"))

(defgeneric process-get (server socket request header-list)
  (:documentation "Process request line (top) into a get request"))

(defgeneric process-port (server socket request header-list)
  (:documentation "Process request line (top) into a post request (reading also
post value as byte array from header-list last stream)"))

(defgeneric process-request (server socket request-structure)
  (:documentation "Process request of any type."))

(defgeneric add-post-info (server post socket-stream)
  (:documentation "Parses info from post part of socket stream
and stores is as bytearray inside request-post slot."))

(defgeneric get-request-header (server socket)
  (:documentation "Parses header lines out of http request in socket."))

(defgeneric parse-request (server request-structure)
  (:documentation "Parses resource part of the first line of http
header. Then replaces empty ones with index.html and also fills all other
parts of the structure.

Signals access-denied condition if trying to access any .clconfig file."))

(defgeneric parse-path (server path)
  (:documentation "Parses path from local request to global file path based on 
server settings (home directory)."))

(defgeneric get-public-page (server path)
  (:documentation "Returns path if path exists and is a file.

Signals file-not-found condition if file can't be found."))

(defgeneric get-error-page (server type-of-error)
  (:documentation "Returns generated error page based on type as string stream"))

(defgeneric header (server stream type)
  (:documentation "Writes header info to a stream based on the type keyword (:code)"))

(defgeneric dispatch-header (server socket string-stream)
  (:documentation "Writes header contained in string-stream output stream into 
the socket out to the connection."))

(defgeneric dispatch-htcl (server socket request-structure)
  (:documentation "Loads the htcl page from disk, runs the interpreter of lisp for 
any inline <?cl ?> blocks, emits the output to the socket as the output."))

(defgeneric dispatch-page (server socket request-structure)
  (:documentation "Loads the page from disk, sends it out to the connection."))

(defgeneric dispatch-error-page (server socket error-code-keyword)
  (:documentation "Dispatchs the error code page."))

(defgeneric dispatch-static-page (server socket header file-address mime-type)
  (:documentation "Dispatches single static website with file address and 
specified mime-type to the connection."))

(defgeneric dispatch-string-page (server socket header string-data mime)
  (:documentation "Dispatches string resource as specified mime-type."))

(defgeneric dispatch (server socket output-resource)
  (:documentation "Dispatch generic output resource to the socket."))

(defgeneric dispatch-resource (server socket request)
  (:documentation "Dispatch non page related resource located on path."))

; MOD ECL link
(defgeneric parse-htcl (server socket request string-stream)
  (:documentation "Parses htcl file according to standard rules."))

; Other thread
(defgeneric clean-old-sessions (server))

; Server methods
(defmethod get-session-from-request ((s server) socket request)
  (mp:with-lock (*session-lock*)
    (log:trace "Getting session data")
    (let* ((cookies (parse-cookies))
	   (session-name (cdr (find "Session" cookies
				   :key #'car :test #'string=))))
      (log:trace cookies)
      (log:trace session-name)
      (let ((ses (gethash session-name (sessions s))))
	(cond ((and session-name
		    ses
		    (> (session-timestamp ses) (get-universal-time)))
	       (incf (session-timestamp ses) 60)
	       (setf (request-sessid request) ses)
	       (session-data ses))
	    (t (log:trace "Generating new session id")
	       (when ses
		 (remhash session-name (sessions s)))
	       (let ((new-session-id (format nil "~4,'0X~4,0X~4,0X~4,0X" 
					     (random 4294967296) 
					     (random 4294967296)
					     (random 4294967296)
					     (random 4294967296))))
		 (log:trace new-session-id)
		 (log:trace "Creating new session structure")
		 (let ((new-session (make-session)))
		   (log:trace "new session")
		   (setf (session-datastamp new-session) new-session-id)
		   (setf (session-timestamp new-session)
			 (+ (get-universal-time) (session-timeout s)))
		   (setf (session-data new-session) (make-hash-table))
		   (setf (request-sessid request) new-session)
		   (setf (gethash new-session-id (sessions s)) new-session)
		   (log:debug (gethash new-session-id (sessions s)))
		   (log:trace new-session)
		   (session-data new-session)))))))))

; running
(defmethod process-get ((s server) socket request header)
  (log:trace "Processing get request")
  (let ((get (parse-request s request)))
    (setf (request-type get) :get)
    (add-header-info get header)
    (process-request s socket get)))

(defmethod process-post ((s server) socket request header)
  (log:trace "Processing post request")
  (let ((post (parse-request s request)))
    (setf (request-type post) :post)
    (add-header-info post header)
    (add-post-info s post (car (last header)))
    (process-request s socket post)))

(defmethod process-request ((s server) socket get)
  (log:trace "Processing request")
  (let ((mtype (split (request-mime-type get) #\/)))
    (log:trace get)
    (cond ((string= (first mtype) "text")
	   (if (string= (request-extension get) "htcl")
	       (dispatch-htcl s socket get)
	       (dispatch-page s socket get)))
	  (t (dispatch-resource s socket get)))))

(defmethod add-post-info ((s server) post sstream)
  (log:trace "Adding post info")
  (let ((n (parse-integer (cdr (find "Content-Length" (request-h-args post)
			        :key #'car :test #'string=)) :junk-allowed t)))
    (let ((data (make-array 0 :element-type '(unsigned-byte 8)
			    :adjustable t :fill-pointer 0)))
      (dotimes (x n)
	(vector-push-extend (read-byte sstream) data))
      (setf (request-post post) data))))

; receiving
(defmethod get-request-header ((s server) socket)
  (log:trace "Getting request header")
  (let ((sstream (socket-make-stream socket :input t :output t)))
    (append (read-until-empty-line sstream) (list sstream))))

(defmethod parse-request ((s server) r)
  (log:trace "Parsing request")
  (destructuring-bind (request &optional arguments)
      (split r #\?)
    (setf request (unsanitize request))
    (let ((req (make-request)))
      (setf (request-file req)
	    (car (last (split request #\/))))
      (when (string= (request-file req) "")
	(setf (request-file req) "index.html"))
      (when (string= (request-file req) ".clconfig")
	(error 'access-denied :page ".clconfig" :code :403))
      (setf (request-path req) 
	    (parse-path s request))
      (setf (request-extension req)
	    (car (last (split (request-file req) #\.))))
      (setf (request-mime-type req)
	    (cdr (find (request-extension req)
		       (mime s) :key 'car :test 'string=)))  
      (unless (request-mime-type req)
	(setf (request-mime-type req) "application/octet-stream"))
      (when arguments
	(setf (request-args req)
	      (parse-args arguments)))
      req)))

(defmethod parse-path ((s server) p)
  (let ((p2 (merge-pathnames (parse-namestring (subseq p 1)) 
			     (parse-namestring (home-dir s)))))
    (if (directory-pathname-p p)
	(merge-pathnames (parse-namestring "index.html") p2)
	p2)))

; sending
(defmethod get-public-page ((s server) fpath)
  (unless (probe-file fpath)
    (error 'file-not-found :code :404 :page fpath))
  fpath)

(defmethod get-error-page ((s server) p-type)
  (let ((stream (make-string-output-stream)))
    (case p-type
      (:403 (generate-403 stream s))
      (:404 (generate-404 stream s)))
    stream))

(defmethod header ((s server) stream type)
  (log:trace "Building header")
  (ecase type
    (:200
     (write-string "HTTP/1.0 200 OK" stream) (terpri-http stream))
    (:403
     (write-string "HTTP/1.0 403 Forbidden" stream) (terpri-http stream))
    (:404
     (write-string "HTTP/1.0 404 Not found " stream) (terpri-http stream)))
  (format stream "Server: ~A" (server-version s)) (terpri-http stream))

(defmethod dispatch-header ((s server) sock stringstream)
  (log:trace "Dispatching header")
  (terpri-http stringstream)
  (socket-send sock (s->bv (get-output-stream-string stringstream)) nil))

(defmethod dispatch-htcl ((s server) (socket socket) req)
  (log:trace "Dispatching htcl")
  (let ((code :200)
	(header (make-string-output-stream)))
    (header s header code)
    (let ((content (get-output-stream-string (parse-htcl s socket req header))))
      (log:trace "Dispatching parsed htcl")
      (format header "Set-Cookie: Session=~A; Expires=~A" 
	      (session-datastamp (request-sessid req))
	      (write-time-to-str (session-timestamp (request-sessid req))))
      (terpri-http header)
      (dispatch-string-page s socket header content 
			    (request-mime-type req)))))

(defmethod dispatch-page ((s server) (socket socket) req)
  (log:trace "Dispatching page")
  (let (content 
	(code :200)
	(header (make-string-output-stream)))
    (setf content (get-public-page s (request-path req)))
    (header s header code)
    (dispatch-static-page s socket header content (request-mime-type req))))

(defmethod dispatch-error-page ((s server) (socket socket) code)
  (log:trace "Dispatching error page")
  (let* ((content-stream (get-error-page s code))
	 (header (make-string-output-stream))
	 (output (get-output-stream-string content-stream)))
    (header s header code)
    (add-content-info header "text/html" :len (length output))
    (dispatch-header s socket header)
    (socket-send socket (s->bv output) nil)))

(defmethod dispatch-static-page ((s server) socket header addr mime)
  (log:trace "Dispatching static page")
  (with-open-file (istream addr :direction :input)
    (add-content-info header mime :faddr addr :fstr istream)
    (dispatch-header s socket header)
    (dispatch s socket istream)))

(defmethod dispatch-string-page ((s server) socket header data mime)
  (log:trace "Dispatching string page")
  (add-content-info header mime :len (length data))
  (dispatch-header s socket header)
  (dispatch s socket data))

(defmethod dispatch ((s server) (socket socket) (stream stream))
  (log:trace "Pumping data")
  (etypecase stream
    (string-stream
     (socket-send socket (s->bv (get-output-stream-string stream)) nil))
    (stream
     (let ((sstream (socket-make-stream socket :input t :output t
					:element-type 
					(stream-element-type stream))))
       (copy-stream stream sstream)))))

(defmethod dispatch ((s server) (socket socket) (charseq string))
  (log:trace "Pumping actual data")
  (socket-send socket (s->bv charseq) nil))

(defmethod dispatch-resource ((s server) (socket socket) request)
  (log:trace "Dispatching resource")
  (if (not (probe-file (request-path request)))
      (error 'file-not-found :code :404 :page (request-path request))
      (progn
	(let ((header (make-string-output-stream)))
	  (header s header :200)
	  (with-open-file (istream (request-path request) :direction :input 
				   :element-type '(unsigned-byte 8))
	    (add-content-info header 
			      (request-mime-type request)
			      :faddr (request-path request)
			      :fstr istream)
	    (dispatch-header s socket header)
	    (let ((sstream (socket-make-stream socket :input t :output t
					       :element-type '(unsigned-byte 8))))
	      (copy-stream istream sstream)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions related to the webserver functionality and operations ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun standard-mime-types ()
  "Returns alist of standard mime types. Only used in quick web server."
  `(("htm"."text/html")
    ("html"."text/html")
    ("htmls"."text/html")
    ("htcl"."text/html")

    ("js"."text/javascript")
    
    ("ico"."image/x-icon")
    ("png"."image/png")
    ("jpg"."image/jpeg")
    ("jpeg"."image/jpeg")
    ("ogv"."video/ogv")

    ("wav"."audio/wav")))

(defun add-header-info (request header)
  (log:trace "Adding header info")
  (let ((header (cdr header)))
    (setf (request-h-args request)
	  (loop for arg in header collect
	       (when (typep arg 'string)
		 (split-two arg #\:))))))
		        
(defun add-content-info (str mime &key faddr fstr len (add-last-mod t))
  (format str "Content-Type: ~A" mime) (terpri-http str)
  (when fstr
    (format str "Content-Lenght: ~A" (file-length fstr)) (terpri-http str))
  (when len
    (format str "Content-Lenght: ~A" len) (terpri-http str))
  (when (and faddr add-last-mod)
    (format str "Last-Modified: ~A" (file-time faddr)) (terpri-http str)))

(defun file-time (f)
  (write-time-to-str (file-write-date f)))

(defun write-time-to-str (utime)
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time utime 0)
    (format nil "~A, ~D ~A ~D ~2,'0d:~2,'0d:~2,'0d GMT"
	    (nth day-of-week +day-names+)
	    date
	    (nth month +mon-names+)
	    year
	    hour
	    minute
	    second)))

;;;;;;
;; Reading functions
;;;;;;
(defun read-until-empty-line (stream)
  (let (result)
    (loop 
      (let ((line (read-clrf-line stream)))
	(when (or (null line)
		  (string= line ""))
	  (return-from read-until-empty-line (reverse result)))
	(push line result)))))

(defun read-clrf-line (stream)
  (let ((str "")
	endsoon)
    (loop 
      (let ((char (read-byte stream nil)))
	(unless char
	  (return-from read-clrf-line nil))
	(when (> char 127)
	  (return-from read-clrf-line nil))
	(let ((char (code-char char)))
	  (cond ((char= char (code-char 13))
		 (setf endsoon t))
		((char= char (code-char 10))
		 (if endsoon
		     (return-from read-clrf-line str)))
	      (t (setf oldchar nil)
		 (setf str (concatenate 'string str (string char))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Utils for server class only ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun s->bv (str)
  "Writes string into byte array with standard ascii encoding."
  (let ((v (make-array (length str) 
		       :element-type '(unsigned-byte 8) :adjustable nil)))
    (loop for i from 0 to (1- (length str)) do
	 (setf (aref v i)
	       (char-code (char str i))))
    v))

(defun b->bv (b)
  "Wraps single byte as byte array of length 1"
  (let ((ret-arr (make-array 1 :element-type '(unsigned-byte 8) :adjustable nil)))
    (setf (aref ret-arr 0) b)
    ret-arr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Static webpage generator section ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-404 (s server-instance)
  "Generates resource not found page."
  (generate-error-page s server-instance
		       404 "Page Not Found" "Page or resource couldn't be found!"))

(defun generate-403 (s server-instance)
  "Generates forbidden page"
  (generate-error-page s server-instance
		       403 "Forbidden" "Server refuses to do your bidding!"))

(defun generate-error-page (s ser code title message)
  "Generates simple error page stream s."
  (let ((*standard-output* s))
    (format t "<HTML><HEAD><TITLE>(~A ~A)</TITLE></HEAD>~%<BODY>" code title)
    (format t "<H1><B>(ERROR ~A) - ~A</B></H1><BR>~%" code title)
    (format t "<I>~A</I><BR>~%" message)
    (format t "<DIV>If you feel this is an error, please mail administrator at
<A HREF=mailto://~A>~A</A></DIV><BR><HR>~%" (server-admin ser) 
(server-admin ser))
    (format t "~A: ~A</BODY></HTML>" (server-version ser) (write-time-to-str
							   (get-universal-time)))))

; Shared functions
(defun terpri-http (stream)
  "Specified terpri for \n\r endlines."
  (let ((type (stream-element-type stream)))
    (cond ((eql type '(unsigned-byte 8))
	   (write-byte 13 stream)
	   (write-byte 10 stream))
	  (t (write-string (make-array 2 :element-type 'character
				       :initial-contents `(,(code-char 13)
							   ,(code-char 10)))
			   stream)))))

(defun parse-args (args &optional (delimiter #\&))
  "Parses args into alist."
  (let ((pairs (split args delimiter)))
    (build-pair-list pairs)))

(defun build-pair-list (pairs)
  "Builds alist from key=value pairs in list of strings."
  (loop for p in pairs collect
    (destructuring-bind (field &optional val)
	(split p #\=)
      (cons (string-trim " " (unsanitize field))
	    (if val (string-trim " " (unsanitize val)) val)))))

(defun unsanitize (string)
  "Unsanitizes string as per http specifications."
  (setf string (replace-all string "+" " ")) 
  (let ((result (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dotimes (i (length string))
      (let ((c (char string i)))
	(cond ((char= c #\%)
	       (vector-push-extend (parse-integer 
				    (make-array 2 :element-type 'character 
						:initial-contents 
						`(,(char string (incf i))
						   ,(char string (incf i)))) 
				    :radix 16)
				   result))
	      (t (vector-push-extend (char-code c) result)))))
    (with-open-stream (s 
		       (ext:make-sequence-input-stream result 
						       :external-format :utf-8))
      (let ((res 
	     (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
	(loop
	  (let ((c (read-char s nil)))
	    (unless c
	      (return-from unsanitize res))
	    (vector-push-extend c res)))))))

(defun stringify (barray)
  "Returns byte array of coded chars as array."
  (let ((str (make-array 0 :element-type 'Character :fill-pointer 0 :adjustable t)))
    (dotimes (x (length barray) str)
      (vector-push-extend (code-char (aref barray x)) str))))

; Specials
(defvar *server*  "This special variable contains current server instance.")
(defvar *socket*  "This special variable contains current socket instance.")
(defvar *request* "This special variable contains current request instance.")
(defvar *header*  "This special variable contains string stream where additional 
header info can be stored.")
(defvar *session* "This special variable contains hashmap that is shared between 
sessions.")


; Methods
(defmethod parse-htcl ((s server) (sock socket) request header)
  (log:trace "Parsing htcl page")
  (let ((source (get-public-page s (request-path request))))
    (let* ((*server* s)
	   (*in-inc* nil)
	   (*socket sock)
	   (*header* header)
	   (*request* request)
	   (*session* (get-session-from-request s sock request)))
      (with-open-file (istream source :direction :input)
	(let ((output (make-string-output-stream)))
	  (log:trace "Starting embedded lisp reader")
	  (run-lisp-interpret istream output)
	  output)))))
	       
(defmethod clean-old-sessions ((s server))
  (loop (progn
	  (loop for x being the hash-keys in (sessions s) do
	       (mp:with-lock (*session-lock*)
		 (when (> (get-universal-time)
			  (session-timestamp (gethash x (sessions s))))
		   (log:trace "Cleaning old session ~A" (gethash x (session s)))
		   (remhash x (session s))))))))
