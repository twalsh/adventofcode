#lang racket

(require srfi/25)
(require "advent-utils.rkt")
                                        
(define lights (make-array (shape 0 1000 0 1000) #f))

(define lines (call-with-input-file "input6.txt" read-lines))

(for ((line lines))
  (match-let
      (((pregexp "^(toggle|turn (on|off)) (\\d+),(\\d+) through (\\d+),(\\d+)"
                 (list _ op arg x0 y0 x1 y1)) line))
       (for* ((x (in-range (string->number x0) (add1 (string->number x1))))
              (y (in-range (string->number y0) (add1 (string->number y1)))))
         (let ((val (match arg
                      (#f (not (array-ref lights x y)))
                      ("on" #t)
                      ("off" #f))))
           (array-set! lights x y val)))))

(for*/sum ((x (in-range 0 1000))
           (y (in-range 0 1000)))
           (if (array-ref lights x y) 1 0))
