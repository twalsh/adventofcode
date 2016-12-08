#lang racket

(require rackunit)
(require srfi/25)
(require "advent-utils.rkt")

(define screen%
  (class object%
    (init width depth)
    (define cols width)
    (define rows depth)
    
    (super-new)
    (define screen (make-array [shape 0 cols 0 rows] 0))
    
    (define/public (print)
      (for ((y (in-range rows)))
        (for ((x (in-range cols)))
          (define p (array-ref screen x y))
          (display (if (= p 1) #\# #\space)))
        (newline))
      (newline))
    
    (define/public (pref x y) (array-ref screen x y))
    (define/public (pset x y v) (array-set! screen x y v))
    
    (define/public (rect coor)
      (for* ((x (in-range (first coor))) (y (in-range (second coor))))
        (pset x y 1)))
    
    (define/public (rotate axis coor)
      (define-values (a b) (values (first coor) (second coor)))
      (define shift-len
        (if (eq? axis 'column) rows cols))
      (define shifted
        (for/vector ((i (in-range shift-len)))
          (define from (if (< i b) (+ (- shift-len b) i) (- i b)))
          (if (eq? axis 'column)
              (pref a from)
              (pref from a))
          ))
      (for ((i (in-range shift-len)))
        (if (eq? axis 'column)
            (pset a i (vector-ref shifted i))
            (pset i a (vector-ref shifted i)))))
    
    (define re #px"(rect|rotate) (?:(column|row) )?((\\d+)x(\\d+)|(?:x|y)=(\\d+) by (\\d+))")
    
    (define/public (interpret instruction)
      (match-let (((regexp re (list _ command axis _ rect-args ..2 shift-args ..2)) instruction))
        (case command
          (("rect")
           (rect (map string->number rect-args)))
          (("rotate")
           (rotate (string->symbol axis) (map string->number shift-args))))))
    
    (define/public (lit-pixels)
      (for*/sum ((x (range cols))
                 (y (range rows)))
        (array-ref screen x y)))))

(define test-instructions
  '("rect 3x2"
    "rotate column x=1 by 1"
    "rotate row y=0 by 4"
    "rotate column x=1 by 1"))

(define test-screen (new screen% [width 50] [depth 3]))

(for ((instruction test-instructions))
  (send test-screen interpret instruction))
(send test-screen print)
(define test-lit-pixels (send test-screen lit-pixels))
(check-equal? test-lit-pixels 6 "Test lit-pixels")

(define puzzle-screen (new screen% [width 50] [depth 6]))
(send puzzle-screen print)

(define puzzle-input (read-input "input8.txt"))
(for ((instruction puzzle-input))
  (send puzzle-screen interpret instruction))
(send puzzle-screen print)
(displayln (send puzzle-screen lit-pixels))