#lang racket/base
;
; Generic XDR Encoding Library
;

(require racket/contract
         racket/dict
         racket/port)

(provide
  (contract-out
    (type? predicate/c)

    (type-value/c (-> type? contract?))
    (type-dump (-> type? (-> any/c void?)))
    (type-load (-> type? (-> any/c)))

    (int/c contract?)
    (uint/c contract?)
    (size/c contract?)
    (long/c contract?)
    (ulong/c contract?)

    (structure (->i () ()
                    #:rest (members (listof type?))
                    (result (members) (type/c (members/c members)))))

    (dump (->i ((type type?)
                (value (type) (type-value/c type)))
               ((out output-port?))
               (result void?)))

    (load (->i ((type type?))
               ((in input-port?))
               (result (type) (type-value/c type))))

    (dump/bytes (->i ((type type?)
                      (value (type) (type-value/c type)))
                     ()
                     (result bytes?)))

    (load/bytes (->i ((type type?)
                      (bstr bytes?))
                     ()
                     (result (type) (type-value/c type))))))


;; Contract for exact length byte string.
(define (bytes-len/c len)
  (procedure-rename
    (lambda (v)
      (and (bytes? v)
           (= (bytes-length v) len)))
    (string->symbol
      (format "bytes-len=~a?" len))))


;; Contract for fixed length list.
(define (list-len/c (len 2147483647))
  (procedure-rename
    (lambda (v)
      (and (list? v)
           (= (length v) len)))
    (string->symbol
      (format "list-len<=~a?" len))))


;; Contract for maximum length list.
(define (list-max-len/c (len 2147483647))
  (procedure-rename
    (lambda (v)
      (and (list? v)
           (<= (length v) len)))
    (string->symbol
      (format "list-max-len<=~a?" len))))


;; Contract for fixed length vector.
(define (vector-len/c (len 2147483647))
  (procedure-rename
    (lambda (v)
      (and (vector? v)
           (= (vector-length v) len)))
    (string->symbol
      (format "vector-len=~a?" len))))


;; Structure with both encoding and decoding function for given type.
(struct type
  (value/c dump load))


;; Integer types, because they are too long to be
;; included in the definitions below.
(define int/c   (integer-in -2147483648 2147483647))
(define size/c  (integer-in 0 2147483647))
(define uint/c  (integer-in 0 4294967295))
(define long/c  (integer-in -9223372036854775808 9223372036854775807))
(define ulong/c (integer-in 0 18446744073709551615))


;; Contract for type structure with given value contract.
(define (type/c value/c)
  (struct/c type
            contract?
            (-> value/c void?)
            (-> value/c)))


;; Shortcut to define various types and type generators.
(define-syntax define-xdr-type
  (syntax-rules (define dump load)
    ((define-xdr-type (name (arg arg/c) ...) value/c
       (define (dump v) dump-body ...)
       (define (load) load-body ...))
     (begin
       (provide
         (contract-out
           (name (->i ((arg arg/c) ...) ()
                      (result (arg ...) (type/c value/c))))))
       (define (name arg ...)
         (type value/c
               (λ (v) dump-body ... (void))
               (λ () load-body ...)))))

    ((define-xdr-type name value/c
       (define (dump v) dump-body ...)
       (define (load) load-body ...))
     (begin
       (provide
         (contract-out
           (name (type/c value/c))))
       (define name
         (type value/c
               (λ (v) dump-body ... (void))
               (λ () load-body ...)))))))


(define-xdr-type int int/c
  (define (dump v)
    (write-bytes (integer->integer-bytes v 4 #t #t)))

  (define (load)
    (integer-bytes->integer (read-bytes 4) #t #t)))

(define-xdr-type uint uint/c
  (define (dump v)
    (write-bytes (integer->integer-bytes v 4 #f #t)))

  (define (load)
    (integer-bytes->integer (read-bytes 4) #f #t)))

(define-xdr-type long long/c
  (define (dump v)
    (write-bytes (integer->integer-bytes v 8 #t #t)))

  (define (load)
    (integer-bytes->integer (read-bytes 8) #t #t)))

(define-xdr-type ulong ulong/c
  (define (dump v)
    (write-bytes (integer->integer-bytes v 8 #f #t)))

  (define (load)
    (integer-bytes->integer (read-bytes 8) #f #t)))


(define-xdr-type float real?
  (define (dump v)
    (write-bytes (real->floating-point-bytes v 4 #t)))

  (define (load)
    (floating-point-bytes->real (read-bytes 4) #t)))

(define-xdr-type double real?
  (define (dump v)
    (write-bytes (real->floating-point-bytes v 8 #t)))

  (define (load)
    (floating-point-bytes->real (read-bytes 8) #t)))


(define (round-up len)
  (* 4 (ceiling (/ len 4))))

(define-xdr-type (opaque (len size/c))
                 (bytes-len/c len)
  (define (dump v)
    (let ((buffer (make-bytes (round-up len))))
      (bytes-copy! buffer 0 v)
      (write-bytes buffer)))

  (define (load)
    (let ((aligned-length (round-up len)))
      (subbytes (read-bytes aligned-length) 0 len))))

(define-xdr-type opaque* bytes?
  (define (dump v)
    (let* ((length (bytes-length v))
           (aligned (round-up length))
           (buffer (make-bytes (+ 4 aligned))))
      (bytes-copy! buffer 0 (integer->integer-bytes length 4 #t #t))
      (bytes-copy! buffer 4 v)
      (write-bytes buffer)))

  (define (load)
    (let* ((length (integer-bytes->integer (read-bytes 4) #t #t))
           (aligned (round-up length)))
      (subbytes (read-bytes aligned) 0 length))))

(define-xdr-type utf8* string?
  (define (dump v)
    (let* ((bstr (string->bytes/utf-8 v))
           (length (bytes-length bstr))
           (aligned (round-up length))
           (buffer (make-bytes (+ 4 aligned))))
      (bytes-copy! buffer 0 (integer->integer-bytes length 4 #t #t))
      (bytes-copy! buffer 4 bstr)
      (write-bytes buffer)))

  (define (load)
    (let* ((length (integer-bytes->integer (read-bytes 4) #t #t))
           (aligned (round-up length)))
      (bytes->string/utf-8
        (subbytes (read-bytes aligned) 0 length)))))


(define-xdr-type (array (type type?) (len size/c))
                 (and/c (listof (type-value/c type))
                        (list-len/c len))
  (define (dump v)
    (for ((item v))
      ((type-dump type) item)))

  (define (load)
    (for/list ((i len))
      ((type-load type)))))

(define-xdr-type (array* (type type?) (len size/c))
                 (and/c (listof (type-value/c type))
                        (list-max-len/c len))
  (define (dump v)
    (write-bytes (integer->integer-bytes (length v) 4 #t #t))
    (for ((item v))
      ((type-dump type) item)))

  (define (load)
    (let ((length (integer-bytes->integer (read-bytes 4) #t #t)))
      (for/list ((i length))
        ((type-load type))))))


(define-xdr-type (enum (members (listof (cons/c symbol? size/c))))
                 (or/c (apply symbols (map car members))
                       exact-nonnegative-integer?)
  (define (dump v)
    (write-bytes (integer->integer-bytes (dict-ref members v v) 4 #t #t)))

  (define (load)
    (let ((v (integer-bytes->integer (read-bytes 4) #t #t)))
      (or (for/or (((label value) (in-dict members)))
            (and (= v value) label))
          v))))

(define-xdr-type (optional (type type?))
                 (or/c #f (type-value/c type))
  (define (dump v)
    (if v
        (begin
          (write-bytes (integer->integer-bytes 1 4 #t #t))
          ((type-dump type) v))
        (write-bytes (integer->integer-bytes 0 4 #t #t))))

  (define (load)
    (let ((length (integer-bytes->integer (read-bytes 4) #t #t)))
      (and (> length 0)
           ((type-load type))))))


(define-xdr-type nothing void?
  (define (dump v)
    (void))
  (define (load)
    (void)))


(define-xdr-type bool boolean?
  (define (dump v)
    (write-bytes (integer->integer-bytes (if v 1 0) 4 #t #t)))

  (define (load)
    (> (integer-bytes->integer (read-bytes 4) #f #t) 0)))


(define (members/c members)
  (apply vector/c (map type-value/c members)))

(define (structure . members)
  (type (members/c members)
        (λ (v)
          (for ((type members)
                (value v))
            ((type-dump type) value)))

        (λ ()
          (for/vector ((type members))
            ((type-load type))))))


(define (dump type value (out (current-output-port)))
  (parameterize ((current-output-port out))
    ((type-dump type) value)))

(define (load type (in (current-input-port)))
  (parameterize ((current-input-port in))
    ((type-load type))))


(define (dump/bytes type value)
  (with-output-to-bytes
    (λ () (dump type value))))

(define (load/bytes type bstr)
  (with-input-from-bytes bstr
    (λ () (load type))))


; vim:set ts=2 sw=2 et:
