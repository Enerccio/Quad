;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :quad-server
  (:use :cl :sb-bsd-sockets)
  (:export

   ; common
   +day-names+
   +mon-names+
   request-error
   page-error
   code-error
   file-not-found
   access-denied
   copy-stream
   replace-all
   split
   split-two
   component-present-p
   directory-pathname-p
   
   ; mod lisp
   web-read
   *web-package*
   *is-inc*
   run-lisp-interpret

   ; lisp api
   get-session-from-request
   get-post-arg
   get-get-args
   get-header-arg
   write-to-header
   redirect-denied
   parse-cookies
   include
   include-once
   return-loaded-pages-map
   is-included

   ; server.lisp
   request
   request-path
   request-file
   request-extension
   request-mime-type
   request-h-args
   request-args
   request-post
   request-sessid
   request-type
   session
   session-timestamp
   session-datastamp
   session-data

   server
   port
   home-dir
   sys-dir
   server-version
   mime
   server-admin
   session-timeout
   
   get-session-from-request
   process-get
   process-port
   process-request
   add-post-info
   get-request-header
   parse-request
   parse-path
   get-public-page
   get-error-page
   header
   dispatch-header
   dispatch-htcl
   dispatch-page
   dispatch-error-page
   dispatch-static-page
   dispatch-string-page
   dispatch
   dispatch-resource
   
   parse-htcl

   clean-old-sessions
   
   standard-mime-types
   add-header-info
   add-content-info
   file-time
   write-time-to-str
   
   read-until-empty-line
   read-clrf-line
   s->bv
   b->bv
   
   generate-404
   generate-403
   generate-error-page

   terpri-http
   parse-args
   build-pair-list
   unsanitize
   stringify
   
   *server*
   *socket*
   *request*
   *header*
   *session*

   ; control.lisp
   quick-web-server
   run-web-server
   dispatch-client
   run-test
   kill-test))
