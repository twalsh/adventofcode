#lang racket

(require rnrs/arithmetic/bitwise-6)

(provide process wire wire-set! line-destruct wire-ref do-part-one)

(define wire (make-hash))

(define (wire-ref w)
  (hash-ref wire w 0))

(define (wire-set! w signal)
  (hash-set! wire w signal))

(define instr-regex #px"(?:(NOT) )?(\\w+)(?: (AND|OR|LSHIFT|RSHIFT) (\\w+))? -> (\\w+)")

; De-struct a line into a list using the instr-regex
(define (line-destruct l)
  (cdr (regexp-match instr-regex l)))

(define (param->value p)
  (match p
    ((pregexp "\\d+") (string->number p))
    ((pregexp "[a-z]+") (wire-ref p))
    (#f p)
    ))

(define (process line)
  (match-let (((pregexp instr-regex (list _ :not p1 op p2 w)) line))
    (let* ((v1 (param->value p1))
           (v2 (param->value p2))
           (signal
            (if :not
                (+ 65536 (bitwise-not v1))
                (if op
                    (match op
                      ("AND" (bitwise-and v1 v2))
                      ("OR" (bitwise-ior v1 v2))
                      ("LSHIFT" (bitwise-arithmetic-shift-left v1 v2))
                      ("RSHIFT" (bitwise-arithmetic-shift-right v1 v2))
                      )
                    ; else input is value of sig1
                    v1))))
           (wire-set! w signal))))

(define (do-part-one lines)
  (for ((line lines))
    (process line))
  wire)