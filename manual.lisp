;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(require 'sockets)
(require 'asdf)

(ql:quickload "log4CL")

; Does not work with ecl..
;(ql:quickload "clsql")
;(asdf:operate 'asdf:load-op 'clsql)

(load "package.lisp")
(load "common.lisp")
(load "mod_lisp.lisp")
(load "server.lisp")
(load "control.lisp")
