# External Data Representation

Usage example:

```racket
(require unstable/socket
         (prefix-in xdr: xdr))

(define-values (in out)
  (unix-socket-connect "/var/run/some-nice-socket"))

(define message-t
  (xdr:structure xdr:uint (xdr:opaque* 256)))

(xdr:dump message-t #(42 #"hello world") out)
(xdr:load message-t in)
```
