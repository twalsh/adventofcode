#lang racket

(require rnrs/arithmetic/bitwise-6)

(provide process
         wire wire-set! line-destruct wire-ref
         do-part-one do-part-two
         instr-table)

; Table of wire values
(define wire (make-hash))
; Stores the instruction parsed from a line of input. Processed by instr->signal
; to calculate the signal for the corresponding wire
(struct instr (not p1 op p2 wire) #:transparent)
; Instruction table
(define instr-table (make-hash))

; Return the value of a wire
(define (wire-ref w)
  (if (hash-has-key? wire w)
      (hash-ref wire w)
      ; If the wire has not been defined, run the instruction for it, store it
      ; in the hash and return the value.
      (hash-ref! wire w (instr->signal (hash-ref instr-table w)))
      ))

(define (wire-set! w signal)
  (hash-set! wire w signal))

; Regex for parsing an instruction line.
(define instr-regex #px"(?:(NOT) )?(\\w+)(?: (AND|OR|LSHIFT|RSHIFT) (\\w+))? -> (\\w+)")

; De-struct a line into a list using the instr-regex
(define (line-destruct l)
  (cdr (regexp-match instr-regex l)))

(define (string->instr line)
  (match-let (((pregexp instr-regex (list _ :not p1 op p2 w)) line))
    (instr :not p1 op p2 w)))

; Convert a parameter from an instruction to its signal value
(define (param->value p)
  (match p
    ; Signal - 16-bit number
    ((pregexp "\\d+") (string->number p))
    ; Parameter is a wire
    ((pregexp "[a-z]+") (wire-ref p))
    ; Parameter is null
    (#f p)
    ))

; Parse instruction for a wire and return the input signal.
(define (instr->signal i)
  (let ((v1 (param->value (instr-p1 i))))
    (if (instr-not i)
        ; NOT... instuction
        (+ 65536 (bitwise-not v1))
        (let ((op (instr-op i)))
          (if op
              (let ((v2 (param->value (instr-p2 i))))
                (match op
                  ; Operation is a gate
                  ("AND" (bitwise-and v1 v2))
                  ("OR" (bitwise-ior v1 v2))
                  ("LSHIFT" (bitwise-arithmetic-shift-left v1 v2))
                  ("RSHIFT" (bitwise-arithmetic-shift-right v1 v2))
                  ))
              ; else input is value of sig1
              v1)))))

(define (process)
  (hash-for-each instr-table
                 (lambda (w instr)
                   (wire-set! w (instr->signal instr)))))

(define (do-part-one lines)
  (for ((line lines))
    (let ((i (string->instr line)))
      (hash-set! instr-table (instr-wire i) i)))
  (process)
  (wire-ref "a")
  )

(define (do-part-two part-one-output)
  ; Replace the input for b with an override equal to the output from part one
  (hash-set! instr-table "b"
             (string->instr (format "~s -> b" part-one-output)))
  ; Rerun the wiring procedure
  (hash-clear! wire)
  (process)
  (wire-ref "a"))
