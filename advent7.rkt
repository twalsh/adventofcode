#lang racket

(require rnrs/arithmetic/bitwise-6)

(provide process wire wire-set! line-destruct wire-ref do-part-one instr-table)

(define wire (make-hash))
(define instr-table (make-hash))

; Return the value of a wire
(define (wire-ref w)
  (if (hash-has-key? wire w)
      (hash-ref wire w)
   ; If the wire has not been defined, run the instruction for it
      (instr->wire (hash-ref instr-table w))))      

(define (wire-set! w signal)
  (hash-set! wire w signal))

(define instr-regex #px"(?:(NOT) )?(\\w+)(?: (AND|OR|LSHIFT|RSHIFT) (\\w+))? -> (\\w+)")

; De-struct a line into a list using the instr-regex
(define (line-destruct l)
  (cdr (regexp-match instr-regex l)))

(define (param->value p)
  (match p
    ; Signal - 16-bit number
    ((pregexp "\\d+") (string->number p))
    ; Parameter is a wire
    ((pregexp "[a-z]+") (wire-ref p))
    ; Parameter is null
    (#f p)
    ))

(struct instr (not p1 op p2 wire) #:transparent)

(define (make-instr-table line)
  (match-let (((pregexp instr-regex (list _ :not p1 op p2 w)) line))
    (hash-set! instr-table w (instr :not p1 op p2 w))))

; Parse instruction for a wire and store value in the wire table
(define (instr->wire i)
  (let* ((v1 (param->value (instr-p1 i)))
         (v2 (param->value (instr-p2 i)))
         (op (instr-op i))
         (w (instr-wire i))
         (signal
          (if (instr-not i)
              ; NOT... instuction
              (+ 65536 (bitwise-not v1))
              (if op
                  (match op
                    ; Operation is a gate
                    ("AND" (bitwise-and v1 v2))
                    ("OR" (bitwise-ior v1 v2))
                    ("LSHIFT" (bitwise-arithmetic-shift-left v1 v2))
                    ("RSHIFT" (bitwise-arithmetic-shift-right v1 v2))
                    )
                  ; else input is value of sig1
                  v1))))
    (wire-set! w signal)
    (hash-ref wire w)
    )) 

(define (process)
  (hash-for-each instr-table
   (lambda (w instr)
     (instr->wire instr))))

(define (do-part-one lines)
  (for ((line lines))
    (make-instr-table line))
  (process)
  (wire-ref "a")
  )