;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(eval-when (:compile-toplevel :load-toplevel)
  (load "package.lisp"))

(in-package :quad-server)

;;;;;;;;;;;;;
; Constants ;
;;;;;;;;;;;;;
(defconstant +day-names+
  '("Mon" "Tue" "Wed"
    "Thu" "Fri" "Sat"
    "Sun"))

(defconstant +mon-names+
  '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

;;;;;;;;;;;;;;
; Conditions ;
;;;;;;;;;;;;;;

; This defines special condition
; that is used to force redirect into error pages
; if it is errored from anywhere, control is immediately 
; brought to handler which is after reading the header info
; which will dispatch correct error page (generated)
(define-condition request-error ()
  ((page 
    :initarg :page
    :accessor page-error
    :documentation "Page which has signaled error.")
   (code
    :initarg :code
    :accessor code-error
    :documentation "Key with error code (:404)")))

(define-condition file-not-found (request-error)
  ())

(define-condition access-denied (request-error)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utilities and macros                                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun copy-stream (from to)
  "Copies the string from one stream into another with 4096 long buffer."
  (let ((buf (make-array 4096 :element-type (stream-element-type from))))
    (do ((pos (read-sequence buf from) (read-sequence buf from)))
        ((= 0 pos) nil)
      (write-sequence buf to :end pos))))

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 

(defun split (string char)
  "Splits the string by char and returns it as list."
  (let (container 
	(cstring (make-array 0 
			     :element-type 'character
			     :fill-pointer 0
			     :adjustable t)))
    (loop for c across string do
      (cond 
	((and (char= c char) (> (length cstring) 0)) 
	 (push cstring container)
	 (setf cstring (make-array 0 :element-type 'character :fill-pointer 0 
				   :adjustable t)))
	((and (char= c char) (= (length cstring) 0)) :nop)
	(t (vector-push-extend c cstring))))
    (push cstring container)
    (nreverse container)))

(defun split-two (string char)
  "Splits the string into two strings by first occurence of char. Returned as pair."
  (let (to-second
	(first 
	 (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
	(second
	 (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop for c across string do
	 (cond
	   ((and (char= c char) (null to-second))
	    (setf to-second t))
	   (to-second
	    (vector-push-extend c second))
	   (t 
	    (vector-push-extend c first))))
    (cons first second)))

(defun component-present-p (value)
  "Returns if component is present."
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p  (p)
  "Returns if pathname is directory."
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))
