#lang typed/racket/base
;
; Typed XDR Writer
;

(provide define-xdr-writer)

(require xdr/typed/common)

(provide
  (all-from-out xdr/typed/common))


(define-syntax-rule (define-xdr-writer name schema)
  (begin
    (: name (->* (schema) (Output-Port) Void))
    (define (name value (out (current-output-port)))
      (parameterize ((current-output-port out))
        (write-xdr schema value)))))


(define-syntax write-xdr
  (syntax-rules (Void Integer Natural Long-Integer Long-Natural Flonum Long-Flonum
                 Bytes String List Listof Option Boolean)
    ((_ Long-Integer value)
     (write-xdr-integer value 8 #t))

    ((_ Long-Natural value)
     (write-xdr-integer value 8 #f))

    ((_ Integer value)
     (write-xdr-integer value 4 #t))

    ((_ Natural value)
     (write-xdr-integer value 4 #f))

    ((_ Long-Flonum value)
     (write-xdr-flonum value 8))

    ((_ Flonum value)
     (write-xdr-flonum value 4))

    ((_ Boolean value)
     (write-xdr-bool value))

    ((_ Bytes value)
     (write-xdr-bytes value))

    ((_ String value)
     (write-xdr-string value))

    ((_ (List) value)
     (void))

    ((_ (List schema more ...) value)
     (let ((bound-value value))
       (write-xdr schema (car bound-value))
       (write-xdr (List more ...) (cdr bound-value))))

    ((_ (Listof schema) value)
     (write-many (λ (v) (write-xdr schema v)) value))

    ((_ (Option schema) value)
     (write-optional (λ (v) (write-xdr schema v)) value))))


(: write-xdr-integer (-> Integer (U 2 4 8) Boolean Void))
(define (write-xdr-integer value size signed?)
  (write-bytes/fail
    (integer->integer-bytes value size signed? #t)))

(: write-xdr-flonum (-> Flonum (U 4 8) Void))
(define (write-xdr-flonum value size)
  (write-bytes/fail
    (real->floating-point-bytes value size #t)))

(: write-xdr-bool (-> Boolean Void))
(define (write-xdr-bool value)
  (write-bytes/fail
    (integer->integer-bytes (if value 1 0) 4 #t #t)))

(: write-xdr-bytes (-> Bytes Void))
(define (write-xdr-bytes value)
  (let ((size (bytes-length value)))
    (write-xdr-integer size 4 #f)
    (cond
      ((= (modulo size 4) 0)
       (write-bytes/fail value))

      (else
       (let ((bstr (make-bytes (round-up size))))
         (bytes-copy! bstr 0 value)
         (write-bytes/fail bstr))))))

(: write-xdr-string (-> String Void))
(define (write-xdr-string value)
  (write-xdr-bytes
    (string->bytes/utf-8 value)))

(: write-many (All (a) (-> (-> a Void) (Listof a) Void)))
(define (write-many write-one value)
  (let ((size (length value)))
    (write-xdr-integer size 4 #f)
    (for ((item value))
      (write-one item))))

(: write-optional (All (a) (-> (-> a Void) (U False a) Void)))
(define (write-optional write-one value)
  (cond
    (value
     (write-xdr-integer 1 4 #f)
     (write-one value))

    (else
     (write-xdr-integer 0 4 #f))))


(: round-up (-> Integer Integer))
(define (round-up size)
  (* 4 (ceiling (/ size 4))))


(: write-bytes/fail (-> Bytes Void))
(define (write-bytes/fail bstr)
  (void (write-bytes bstr)))


; vim:set ts=2 sw=2 et:
