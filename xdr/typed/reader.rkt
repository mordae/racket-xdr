#lang typed/racket/base
;
; Typed XDR Reader
;

(provide define-xdr-reader)

(require xdr/typed/common)

(provide
  (all-from-out xdr/typed/common))


(define-syntax-rule (define-xdr-reader name schema)
  (define (name (in : Input-Port (current-input-port)))
    (with-handlers ((not (λ _ eof)))
      (parameterize ((current-input-port in))
        (read-xdr schema)))))


(define-syntax read-xdr
  (syntax-rules (Integer Natural Long-Integer Long-Natural Flonum Long-Flonum
                 Bytes String List Listof Option Boolean)
    ((_ Integer)
     (read-xdr-integer 4 #t))

    ((_ Natural)
     (read-xdr-integer 4 #f))

    ((_ Long-Integer)
     (read-xdr-integer 8 #t))

    ((_ Long-Natural)
     (read-xdr-integer 8 #f))

    ((_ Flonum)
     (read-xdr-flonum 4))

    ((_ Long-Flonum)
     (read-xdr-flonum 8))

    ((_ Boolean)
     (read-xdr-bool))

    ((_ Bytes)
     (read-xdr-bytes))

    ((_ String)
     (read-xdr-string))

    ((_ (List schema ...))
     (list (read-xdr schema) ...))

    ((_ (Listof schema))
     (read-many (λ () (read-xdr schema))))

    ((_ (optional schema))
     (read-optional (λ () (read-xdr schema))))))


(: read-xdr-integer (-> Natural Boolean Integer))
(define (read-xdr-integer size signed?)
  (integer-bytes->integer (read-bytes/fail size) signed? #t))

(: read-xdr-flonum (-> Natural Real))
(define (read-xdr-flonum size)
  (floating-point-bytes->real (read-bytes/fail size) #t))

(: read-xdr-bool (-> Boolean))
(define (read-xdr-bool)
  (not (= 0 (read-xdr-integer 4 #f))))

(: read-xdr-bytes (-> Bytes))
(define (read-xdr-bytes)
  (let* ((size (read-xdr-integer 4 #f))
         (bstr (read-bytes/fail (round-up size))))
    (subbytes bstr 0 size)))

(: read-xdr-string (-> String))
(define (read-xdr-string)
  (bytes->string/utf-8 (read-xdr-bytes)))

(: read-many (All (a) (-> (-> a) (Listof a))))
(define (read-many read-one)
  (let ((size (read-xdr-integer 4 #f)))
    (for/list ((i size)) : (Listof a)
      (read-one))))

(: read-optional (All (a) (-> (-> a) (U False a))))
(define (read-optional read-one)
  (and (> (read-xdr-integer 4 #f) 0)
       (read-one)))


(: round-up (-> Integer Integer))
(define (round-up size)
  (* 4 (ceiling (/ size 4))))


(: read-bytes/fail (-> Integer Bytes))
(define (read-bytes/fail amt)
  (let ((bstr (read-bytes amt)))
    (if (and (bytes? bstr)
             (= amt (bytes-length bstr)))
        (values bstr)
        (raise #f))))


; vim:set ts=2 sw=2 et:
