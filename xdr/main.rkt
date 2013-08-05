#lang racket/base
;
; Generic XDR Encoding Library
;

(require racket/contract
         racket/function
         racket/dict
         racket/port)

(provide type? int uint long ulong float double opaque opaque*
         array array* optional structure bool enum bytes-len/c
         dump load dump/bytes load/bytes)


;; Contract for maximum length byte string.
(define (bytes-len/c (len 2147483647))
  (lambda (v)
    (and (bytes? v)
         (<= (bytes-length v) len))))


;; Contract for fixed length list.
(define (list-len/c (len 2147483647))
  (lambda (v)
    (and (list? v)
         (= (length v) len))))


;; Contract for maximum length list.
(define (list-max-len/c (len 2147483647))
  (lambda (v)
    (and (list? v)
         (<= (length v) len))))


;; Structure with both encoding and decoding function for given type.
(define-struct type (dump load))


;; Integer types, because they are too long to be
;; included in the definitions below.
(define int/c   (integer-in -2147483648 2147483647))
(define uint/c  (integer-in 0 4294967295))
(define long/c  (integer-in -9223372036854775808 9223372036854775807))
(define ulong/c (integer-in 0 18446744073709551615))


(define/contract int type?
  ((thunk
     (define/contract (dump value)
                      (-> int/c void?)
       (void (write-bytes (integer->integer-bytes value 4 #t #t))))

     (define/contract (load)
                      (-> int/c)
       (integer-bytes->integer (read-bytes 4) #t #t))

     (make-type dump load))))


(define/contract uint type?
  ((thunk
     (define/contract (dump value)
                      (-> uint/c void?)
       (void (write-bytes (integer->integer-bytes value 4 #f #t))))

     (define/contract (load)
                      (-> uint/c)
       (integer-bytes->integer (read-bytes 4) #f #t))

     (make-type dump load))))


(define/contract long type?
  ((thunk
     (define/contract (dump value)
                      (-> long/c void?)
       (void (write-bytes (integer->integer-bytes value 8 #t #t))))

     (define/contract (load)
                      (-> long/c)
       (integer-bytes->integer (read-bytes 8) #t #t))

     (make-type dump load))))


(define/contract ulong type?
  ((thunk
     (define/contract (dump value)
                      (-> ulong/c void?)
       (void (write-bytes (integer->integer-bytes value 8 #f #t))))

     (define/contract (load)
                      (-> ulong/c)
       (integer-bytes->integer (read-bytes 8) #f #t))

     (make-type dump load))))


(define/contract float type?
  ((thunk
     (define/contract (dump value)
                      (-> real? void?)
       (void (write-bytes (real->floating-point-bytes value 4 #t))))

     (define/contract (load)
                      (-> real?)
       (floating-point-bytes->real (read-bytes) #t))

     (make-type dump load))))


(define/contract double type?
  ((thunk
     (define/contract (dump value)
                      (-> real? void?)
       (void (write-bytes (real->floating-point-bytes value 8 #t))))

     (define/contract (load)
                      (-> real?)
       (floating-point-bytes->real (read-bytes 8) #t))

     (make-type dump load))))


(define/contract (opaque len)
                 (-> (and/c positive? int/c) type?)
  ((thunk
     (define/contract (dump value)
                      (-> (bytes-len/c len) void?)
       (let ((buffer (make-bytes (* 4 (ceiling (/ len 4))))))
         (bytes-copy! buffer 0 value)
         (void (write-bytes buffer))))

     (define/contract (load)
                      (-> (bytes-len/c len))
       (let ((aligned-length (* 4 (ceiling (/ len 4)))))
         (subbytes (read-bytes aligned-length) 0 len)))

     (make-type dump load))))


(define/contract (opaque* (len 2147483647))
                 (-> (and/c positive? int/c) type?)
  ((thunk
     (define/contract (dump value)
                      (-> (bytes-len/c len) void?)
       (let* ((aligned-length (* 4 (ceiling (/ (bytes-length value) 4))))
              (buffer         (make-bytes (+ 4 aligned-length)))
              (length         (bytes-length value)))
         (bytes-copy! buffer 0 (integer->integer-bytes length 4 #t #t))
         (bytes-copy! buffer 4 value)
         (void (write-bytes buffer))))

     (define/contract (load)
                      (-> (bytes-len/c len))
       (let* ((length         (integer-bytes->integer (read-bytes 4) #t #t))
              (aligned-length (* 4 (ceiling (/ length 4)))))
         (subbytes (read-bytes aligned-length) 0 length)))

     (make-type dump load))))


(define/contract (array type len)
                 (-> type? (and/c positive? int/c) type?)
  ((thunk
     (define/contract (dump value)
                      (-> (list-len/c len) void?)
       (for ((item (in-list value)))
         ((type-dump type) item)))

     (define/contract (load)
                      (-> (list-len/c len))
       (for/list ((i (in-range len)))
         ((type-load type))))

     (make-type dump load))))


(define/contract (array* type (len 2147483647))
                 (-> type? (and/c positive? int/c) type?)
  ((thunk
     (define/contract (dump value)
                      (-> (list-max-len/c len) void?)
       (void (write-bytes (integer->integer-bytes (length value) 4 #t #t)))
       (for ((item (in-list value)))
         ((type-dump type) item)))

     (define/contract (load)
                      (-> (list-max-len/c len))
       (let ((length (integer-bytes->integer (read-bytes 4) #t #t)))
         (for/list ((i (in-range length)))
           ((type-load type)))))

     (make-type dump load))))


(define/contract (enum members)
                 (-> (listof (cons/c symbol? int/c)) type?)
  ((thunk
     (define/contract (dump value)
                      (-> (apply one-of/c (dict-keys members)) void?)
       (void (write-bytes
               (integer->integer-bytes (dict-ref members value) 4 #t #t))))

     (define/contract (load)
                      (-> (or/c symbol? #f))
       (let ((value (integer-bytes->integer (read-bytes 4) #t #t)))
         (for/or (((k v) (in-dict members)))
           (and (= v value) k))))

     (make-type dump load))))


(define/contract (optional type)
                 (-> type? type?)
  ((thunk
     (define/contract (dump value)
                      (-> any/c void?)
       (if (eq? 'null value)
         (void (write-bytes (integer->integer-bytes 0 4 #t #t)))
         (begin
           (void (write-bytes (integer->integer-bytes 1 4 #t #t)))
           ((type-dump type) value))))

     (define/contract (load)
                      (-> any/c)
       (let ((length (integer-bytes->integer (read-bytes 4) #t #t)))
         (if (= length 0)
           'null
           ((type-load type)))))

     (make-type dump load))))


(define/contract bool type?
  ((thunk
     (define/contract (dump value)
                      (-> boolean? void?)
       (void (write-bytes (integer->integer-bytes (if value 1 0) 4 #t #t))))

     (define/contract (load)
                      (-> boolean?)
       (not (= 0 (integer-bytes->integer (read-bytes 4) #t #t))))

     (make-type dump load))))


(define/contract (structure . members)
                 (->* () () #:rest (listof type?) type?)
  ((thunk
     (define/contract (dump value)
                      (-> vector? void?)
       (for ((item-type  (in-list members))
             (item-value (in-vector value)))
         ((type-dump item-type) item-value)))

     (define/contract (load)
                      (-> vector?)
       (for/vector ((item-type (in-list members)))
         ((type-load item-type))))

     (make-type dump load))))


(define/contract (dump type value (out (current-output-port)))
                 (->* (type? any/c) (output-port?) void?)
  (parameterize ((current-output-port out))
    ((type-dump type) value)))


(define/contract (load type (in (current-input-port)))
                 (->* (type?) (input-port?) any/c)
  (parameterize ((current-input-port in))
    ((type-load type))))


(define/contract (dump/bytes type value)
                 (-> type? any/c bytes?)
  (with-output-to-bytes
    (thunk (dump type value))))


(define/contract (load/bytes type bstr)
                 (-> type? bytes? any/c)
  (with-input-from-bytes bstr
    (thunk (load type))))


; vim:set ts=2 sw=2 et:
