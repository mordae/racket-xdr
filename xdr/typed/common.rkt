#lang typed/racket/base
;
; Common Types
;

(provide
  (all-defined-out))

(define-type Long-Integer Integer)
(define-type Long-Natural Natural)
(define-type Long-Flonum Flonum)

; vim:set ts=2 sw=2 et:
