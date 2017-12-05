#lang racket

(require "advent-utils.rkt")

(define test-offsets (vector 0 3 0 1 -3))

(define (find-exit offsets update-instr)
  (let loop ((ptr 0) (steps 0))
    ;(printf "~a ~a~n" offsets ptr)
    (if (or (< ptr 0) (>= ptr (vector-length offsets)))
        steps
        (let ((offset (vector-ref offsets ptr)))
          (vector-set! offsets ptr (update-instr (vector-ref offsets ptr)))
          (loop (+ ptr offset) (add1 steps))))))

(find-exit test-offsets add1)

(define puzzle-data (map string->number (read-input "day05.in")))

(find-exit (list->vector puzzle-data) add1)

(set! test-offsets (vector 0 3 0 1 -3))

(define (update-instr instr)
  (if (>= instr 3)
      (sub1 instr)
      (add1 instr)))

(find-exit test-offsets update-instr)
(find-exit (list->vector puzzle-data) update-instr)

