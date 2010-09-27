;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-
;;;
;;;  This file is the system definition for 'de.setf.zeromq'
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

(asdf:defsystem :de.setf.zeromq
  :author "james anderson <james.anderson@setf.de>"
  :licence "LGPLv3"
  :description "minimal zero mq bindings"
  :depends-on (:cffi)
  :serial t
  :components ((:file "libraries")
               (:file "package")
               (:file "zmq")
               (:file "zeromq"))
  :long-description "This library defines minimal foreign function interface to zeromq.
 It derives from the cz.or.repo.cl-zmq bindings. ")


#|
a few things to keep in mind:

despite the pronouncements about the benefits of simplicity, and the appropriateness of
layering, the discussions[1] point out that a layered protocol which includes
acknowledgements is necessary in order to guarantee delivery when close and send
interact.

the msg record, which is passed in calls to zmq_send, can be destroyed immeidately upon return.
although the api documentation[2] does not state that, their helper definition
for s_send uses a msg record with dynamic extent.

[1] : http://www.mail-archive.com/zeromq-dev@lists.zeromq.org/msg00713.html
[2] : http://github.com/imatix/zguide/blob/master/examples/C/zhelpers.h

|#