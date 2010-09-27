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
;; known as the LLGPL.

(defpackage :zmq
  (:use )
  (:documentation "This is the home package for the zeromq foreign-function bindings -
 operators and structures. the '%' prefix indicates that the arguments, respective the
 structure type itself, is a foreign object.")
  (:export :%bind
           :%close
           :%connect
           :%device
           :%errno
           :%free-callback
           :%getsockopt
           :%init
           :%msg
           :%msg-init
           :%msg-init-size
           :%msg-init-data
           :%msg-close
           :%msg-data
           :%msg-size
           :%msg-copy
           :%msg-move
           :%poll
           :%pollitem
           :%recv
           :%send
           :%setsockopt
           :%socket
           :%strerror
           :%term
           :%version

           :$affinity
           :$delimiter
           :$downstream
           :$efsm
           :$emthread
           :$enocompatproto
           :$forwarder
           :$hausnumero
           :$hwm
           :$identity
           :$max-vsm-size
           :$mcast-loop
           :$msg-more
           :$msg-shared
           :$msg-tbc
           :$noblock
           :$p2p
           :$pair
           :$poll
           :$pollerr
           :$pollin
           :$pollout
           :$pub
           :$pull
           :$push
           :$queue
           :$rate
           :$raw
           :$rcvbuf
           :$rcvmore
           :$recovery-ivl
           :$sndbuf
           :$sndmore
           :$streamer
           :$sub
           :$subscribe
           :$swap
           :$unsubscribe
           :$upstream
           :$vsm
           :$req
           :$rep
           :$xreq
           :$xrep
           ))

(defpackage :zeromq
  (:use )
  (:documentation "This is the home package for the zeromq api.")
  (:export :bind
           :connect
           :error
           :error-errno
           :init
           :term
           :stream
           :push-stream
           :pair-stream
           :pub-stream
           :pull-stream
           :rep-stream
           :req-stream
           :sub-stream
           :strerror

           :*output-buffer-length*))

(defpackage :de.setf.zeromq.implementation
  (:use :common-lisp :cffi)
  #+digitool
  (:import-from :ccl
                :stream-close))


