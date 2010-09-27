;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-
;;;
;;;  This file loads external libraries for 'de.setf.zeromq'
;;;
;;;  Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
;;;
;;;  'de.setf.zeromq' is free software: you can redistribute it and/or modify
;;;  it under the terms of version 3 of the GNU Lesser General Public License as published by
;;;  the Free Software Foundation.
;;;
;;;  'de.setf.zeromq' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;;;  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;;  See the GNU Lesser General Public License for more details.
;;;
;;;  A copy of the GNU Lesser General Public License should be included with 'de.setf.zeromq, as `lgpl.txt`.
;;;  If not, see the GNU [site](http://www.gnu.org/licenses/).

(in-package :common-lisp-user)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library zeromq
    (:mcl "zeromq")
    (:unix (:or "libzmq.so.0.0.0" "libzmq.so"))
    (:windows "libzmq.dll")
    (t "libzmq")))

(cffi:use-foreign-library zeromq)
