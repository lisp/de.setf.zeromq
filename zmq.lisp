;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.zeromq.implementation; -*-
;;;
;;; This file is the part of 'de.setf.zeromq'
;;;
;;;  Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
;;;
;;; This file was part of CL-ZMQ.
;;; This, the de.setf.zeromq version is simplified to implement just the ffi bindings,
;;; distinguish foreign entires and arguments via '%', and is arranged analogous to the 
;;; zmq root documentation page.

;;
;; Vitaly Mayatskikh grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :de.setf.zeromq.implementation)

;;; define an extended foreignentry definition operator which performs the requisite
;;; check of the return value. this generates an unchecked operator - prefixed with an additional '%',
;;; and wraps the call to it with a check for the return value. in order to apply also to operators
;;; which return integer results, that constraint is 'non-negative' rather than 'zero'.

(defmacro defzmqfun ((c-name l-name &key (errchk t)) return-type &rest args)
  (let* ((docstring (when (stringp (car args)) (pop args)))
         (ret (gensym))
         (names (mapcar #'first args))
         (ffi-name (if errchk (cffi::format-symbol (symbol-package l-name) "%~a" l-name) l-name)))
    `(progn
       (defcfun (,c-name ,ffi-name) ,return-type
         ,(format nil "The unchecked foreign entry point for '~a'. See also ~a." c-name l-name)
         ,@args)
       (defun ,l-name (,@names)
         ,@(when docstring (list docstring))
         (let ((,ret (,ffi-name ,@names)))
           (if ,(if (eq return-type :pointer)
                  `(zerop (pointer-address ,ret))
                  `(minusp ,ret))
             (zeromq:error)
             ,ret))))))


;;;
;;; constants

(defconstant zmq:$hwm 1)
(defconstant zmq:$swap 3)
(defconstant zmq:$affinity 4)
(defconstant zmq:$identity 5)
(defconstant zmq:$subscribe 6)
(defconstant zmq:$unsubscribe 7)
(defconstant zmq:$rate 8)
(defconstant zmq:$recovery-ivl 9)
(defconstant zmq:$mcast-loop 10)
(defconstant zmq:$sndbuf 11)
(defconstant zmq:$rcvbuf 12)
(defconstant zmq:$rcvmore 13)

(defconstant zmq:$noblock 1)
(defconstant zmq:$sndmore 2)

(defconstant zmq:$hausnumero 156384712)

;;  Native 0MQ error codes.
(defconstant zmq:$emthread (+ zmq:$hausnumero 50))
(defconstant zmq:$efsm (+ zmq:$hausnumero 51))
(defconstant zmq:$enocompatproto (+ zmq:$hausnumero 52))

(defconstant zmq:$p2p 0)
(defconstant zmq:$pair 0)
(defconstant zmq:$pub 1)
(defconstant zmq:$sub 2)
(defconstant zmq:$req 3)
(defconstant zmq:$rep 4)
(defconstant zmq:$xreq 5)
(defconstant zmq:$xrep 6)
(defconstant zmq:$pull 7)
(defconstant zmq:$push 8)

(defconstant zmq:$max-vsm-size 30)

;;  Message types. These integers may be stored in 'content' member of the
;;  message instead of regular pointer to the data.
(defconstant zmq:$delimiter 31)
(defconstant zmq:$vsm 32)

;; Message flags. ZMQ_MSG_SHARED is strictly speaking not a message flag
;; (it has no equivalent in the wire format), however, making  it a flag
;; allows us to pack the stucture tigher and thus improve performance.
(defconstant zmq:$msg-more 1)
(defconstant zmq:$msg-shared 128)

(defconstant zmq:$pollin 1)
(defconstant zmq:$pollout 2)
(defconstant zmq:$pollerr 4)

(defconstant zmq:$streamer 1)
(defconstant zmq:$forwarder 2)
(defconstant zmq:$queue 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ infrastructure (a.k.a. context) initialisation & termination.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defzmqfun ("zmq_init" zmq:%init) :pointer
  (io-threads   :int))

(defzmqfun ("zmq_term" zmq:%term) :int
  (context      :pointer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ message definition.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcstruct (zmq:%msg)
  (content      :pointer)
  (shared       :uchar)
  (vsm-size     :uchar)
  (vsm-data     :uchar :count 30))      ;; FIXME max-vsm-size

(defzmqfun ("zmq_msg_init" zmq:%msg-init) :int
  (msg  zmq:%msg))

(defzmqfun ("zmq_msg_init_size" zmq:%msg-init-size) :int
  (msg  zmq:%msg)
  (size :long))

(defcallback zmq:%free-callback :void ((ptr :pointer) (hint :pointer))
  (declare (ignorable hint))
  (foreign-free ptr))

(defzmqfun ("zmq_msg_init_data" zmq:%msg-init-data) :int
  (msg  zmq:%msg)
  (data :pointer)
  (size :long)
  (ffn  :pointer)                       ; zmq_free_fn
  (hint :pointer))

(defzmqfun ("zmq_msg_close" zmq:%msg-close) :int
  (msg  zmq:%msg))

(defzmqfun ("zmq_msg_move" zmq:%msg-move) :int
  (dest zmq:%msg)
  (src  zmq:%msg))

(defzmqfun ("zmq_msg_copy" zmq:%msg-copy) :int
  (dest zmq:%msg)
  (src  zmq:%msg))

(defzmqfun ("zmq_msg_data" zmq:%msg-data) :pointer
  (msg  zmq:%msg))

(defzmqfun ("zmq_msg_size" zmq:%msg-size) :int
  (msg  zmq:%msg))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ socket definition.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defzmqfun ("zmq_socket" zmq:%socket) :pointer
  (context      :pointer)
  (type         :int))

(defzmqfun ("zmq_close" zmq:%close) :int
  (s    :pointer))

(defzmqfun ("zmq_setsockopt" zmq:%setsockopt) :int
  (s            :pointer)
  (option       :int)
  (optval       :pointer)
  (optvallen    :long))

(defzmqfun ("zmq_getsockopt" zmq:%getsockopt) :int
  (s            :pointer)
  (option       :int)
  (optval       :pointer)
  (optvallen    :pointer))

(defzmqfun ("zmq_bind" zmq:%bind) :int
  (s    :pointer)
  (addr :pointer :char))

(defzmqfun ("zmq_connect" zmq:%connect) :int
  (s    :pointer)
  (addr :pointer :char))


(defzmqfun ("zmq_send" zmq:%send) :int
  (s            :pointer)
  (msg          zmq:%msg)
  (flags        :int))

(defzmqfun ("zmq_recv" zmq:%recv) :int
  (s            :pointer)
  (msg          zmq:%msg)
  (flags        :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ errors.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("zmq_strerror" zmq:%strerror) :pointer
  (errnum       :int))

(defcfun ("zmq_errno" zmq:%errno) :int)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  I/O multiplexing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcstruct zmq:%pollitem
  (socket       :pointer)
  (fd           :int)
  (events       :short)
  (revents      :short))

(defzmqfun ("zmq_poll" zmq:%poll) :int
  (items        :pointer)
  (nitems       :int)
  (timeout      :long))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Devices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defzmqfun ("zmq_device" zmq:%device) :int
  (device       :int)
  (insocket     :pointer)
  (outsocket    :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Helper functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("zmq_version" zmq:%version) :void
  (major        :pointer)
  (minor        :pointer)
  (patch        :pointer))

