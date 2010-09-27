;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.zeromq.implementation; -*-
;;;
;;; This file is the part of 'de.setf.zeromq'.
;;;
;;;  Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
;;;
;;; This file defines zeromq operators which operates with lisp objects for those arguments 
;;; or results which are intended to be converted through the ffi.

(in-package :de.setf.zeromq.implementation)

(define-condition zeromq:error (error)
  ((errno
    :initarg :errno :initform (zmq:%errno)
    :reader zeromq:error-errno
    :documentation "initialized to the respective zeromq error number current when
     the condition is created."))
  (:report (lambda (condition stream)
             (write-string (zeromq:strerror (zeromq:error-errno condition)) stream))))


(defgeneric zeromq:bind (%socket endpoint)
  (:method ((%socket t) (endpoint string))
    (cffi:with-foreign-string (%endpoint endpoint)
      (zmq:%bind %socket %endpoint)))
  (:method ((%socket t) (%endpoint t))
    (zmq:%bind %socket %endpoint)))


(defgeneric zeromq:connect (%socket endpoint)
  (:method ((%socket t) (endpoint string))
    (cffi:with-foreign-string (%endpoint endpoint)
      (zmq:%connect %socket %endpoint)))
  (:method ((%socket t) (%endpoint t))
    (zmq:%connect %socket %endpoint)))


(defun zeromq:error (&key (errno (zmq:%errno)))
  (error 'zeromq:error :errno errno))


(defun zeromq:strerror (rc)
  (foreign-string-to-lisp (zmq:%strerror rc)))
