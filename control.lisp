;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(eval-when (:compile-toplevel :load-toplevel)
  (load "package.lisp"))

(in-package :quad-server)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions related to running the server ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quick-web-server ()
  "Creates instance of simple server without any special settings."
  (log:trace "Making quickserver")
  (make-instance 'server :type :stream :protocol :tcp))

(defun run-web-server (server)
  "Runs instance of server."
  (log:info "Starting server ~A" server)
  (setf (sockopt-reuse-address server) t)
  (socket-bind server #(0 0 0 0) (port server))
  (socket-listen server 100)
  (log:info "Listening on " (port server))
  (let ((cleanup-thread (mp:process-run-function (gensym "cleanup") 
						 #'clean-old-session server)))
    (unwind-protect
	 (loop
	    (multiple-value-bind (sock addr)
	      (socket-accept server)
	      (log:debug "New client connection socket: " sock addr)
	      (mp:process-run-function (gensym "client")
				       #'dispatch-client
				       sock server addr)))
      (log:trace "Closing server")
      (mp:process-kill cleanup-thread)
      (socket-close server))))

(defun dispatch-client (socket server address)
  "Handles single client. This method is run in a thread."
  (log:trace "Handling connection from " socket server address)
  (handler-case 
      (unwind-protect 
	   (let* ((header (get-request-header server socket))
		  (top (split (when (typep (first header) 'sequence)
				(first header))
			      #\space)))
	     (log:info "Incoming connection from ~A. Request ~A~%" address top)
	     (handler-case 
		 (cond ((string= (first top) "GET")
			(process-get server socket (second top) header))
		       ((string= (first top) "POST")
			(process-post server socket (second top) header)))
	       (request-error (e)
		 (log:error "Failed request " e)
		 (dispatch-error-page server socket (code-error e)))))
	(socket-close socket))
    ;(error (e) (log:error e)))) 
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Testing, debug & development tools ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let (srv)
  (defun run-test ()
    "Runs the instance of server in new thread, so repl is not blocked."
    (unwind-protect 
	 (kill-test)
      (setf srv
	    (mp:process-run-function (gensym "TEST")
				     #'run-web-server
				     (quick-web-server)))))
  (defun kill-test ()
    "Stops the already running thread with server, returns T if it was succesful."
    (when srv
      (prog1
	  (mp:process-kill srv)
	(setf srv nil)))))
