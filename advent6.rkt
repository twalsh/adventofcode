#lang racket

(require srfi/25)
(require "advent-utils.rkt")

(define op-table (hash
                  "turn on"  (lambda (lights x y) (array-set! lights x y #t))
                  "turn off" (lambda (lights x y) (array-set! lights x y #f))
                  "toggle"   (lambda (lights x y) (array-set! lights x y (not (array-ref lights x y))))))
                                                     
(define lights (make-array (shape 0 1000 0 1000) #f))

(define lines (call-with-input-file "input6.txt" read-lines))

(define (process-line line)
  (match-let
      (((pregexp "^(toggle|turn (on|off)) (\\d+),(\\d+) through (\\d+),(\\d+)"
                (list _ op _ x0 y0 x1 y1)) line))
    (list (hash-ref op-table op) (map string->number (list x0 y0 x1 y1)))
    ))

(for ((order (map process-line lines)))
  (match-let (((list proc (list x0 y0 x1 y1)) order))
    (for* ((x (in-range x0 (add1 x1)))
           (y (in-range y0 (add1 y1))))
      (proc lights x y))))

(for*/sum ((x (in-range 0 1000))
           (y (in-range 0 1000)))
           (if (array-ref lights x y) 1 0))
           