#lang racket

(require srfi/25)
(require "advent-utils.rkt")
                                        
(define lights (make-array (shape 0 1000 0 1000) #f))

(define lines (call-with-input-file "input6.txt" read-lines))

(define (process-line line)
  (match-let
      (((pregexp "^(toggle|turn (on|off)) (\\d+),(\\d+) through (\\d+),(\\d+)"
                (list _ op arg x0 y0 x1 y1)) line))
    (list arg (map string->number (list x0 y0 x1 y1)))
    ))

(for ((order (map process-line lines)))
  (match-let (((list arg (list x0 y0 x1 y1)) order))
    (for* ((x (in-range x0 (add1 x1)))
           (y (in-range y0 (add1 y1))))
      (let ((val (match arg
                   (#f (not (array-ref lights x y)))
                   ("on" #t)
                   ("off" #f))))
        (array-set! lights x y val)))))

(for*/sum ((x (in-range 0 1000))
           (y (in-range 0 1000)))
           (if (array-ref lights x y) 1 0))
