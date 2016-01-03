#lang racket

(require rnrs/arithmetic/bitwise-6)

(provide process wire wire-set! line-destruct wire-ref)

(define wire (make-hash))

(define (wire-ref w)
  (hash-ref wire w))

(define (wire-set! w signal)
  (hash-set! wire w signal))

(define instr-regex #px"(?:(NOT) )?(\\w+)(?: (AND|OR|LSHIFT|RSHIFT) (\\w+))? -> (\\w)")

; De-struct a line into a list using the instr-regex
(define (line-destruct l)
  (cdr (regexp-match instr-regex l)))

(define (process line)
  (match-let (((pregexp instr-regex (list _ :not sig1 op sig2 w)) line))
    (let ((signal
           ; If NOT x, signal is bitwise complement of sig1
           (if :not
               (+ 65536 (bitwise-not (wire-ref sig1)))
               ; If op is set, then both sig1 and sig2 must be set
               (if op
                   (match op
                     ("AND" (bitwise-and (wire-ref sig1) (wire-ref sig2)))
                     ("OR" (bitwise-ior   (wire-ref sig1) (wire-ref sig2)))
                     ("LSHIFT" (bitwise-arithmetic-shift-left (wire-ref sig1) (string->number sig2)))
                     ("RSHIFT" (bitwise-arithmetic-shift-right (wire-ref sig1) (string->number sig2)))
                     )
                   ; else input is value of sig1
                   (string->number sig1)))))
      (wire-set! w signal))))