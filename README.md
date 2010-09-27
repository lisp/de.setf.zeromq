

# DE.SETF.ZEROMQ: a minimal Common Lisp FFI binding for ZeroMQ


`de.setf.zeromq` derives from `cl-zmq`[[1]].
In order to eliminate dependancies, it is reduced to just the FFI operators.

## Status

This is intended as the base for a stream-based layer on ZeroMQ.
Once I've figured out how to combine that with the various socket type combinations, it will be incorporated as a sub-system.

 
## Licensing

This version is released under Lisp Lesser GNU Public License,[[2]] as was the original `cl-zmq`.
This version depends only on cffi, for which a provisional mcl bridge is included.

- [alexandria](mailto:alexandria-devel@common-lisp.net) : Public Domain
- [cffi](mailto:loliveira@common-lisp.net) : MIT
- [babel](mailto:loliveira@common-lisp.net) : MIT
- [trivial-features](mailto:loliveira@common-lisp.net) : MIT

 [1]: http://repo.or.cz/w/cl-zmq.git
 [2]: preamble.html

--------
![made with mcl](http://www.digitool.com/img/mcl-made-1.gif "Made With MCL")


