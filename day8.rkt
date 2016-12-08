#lang racket
(require srfi/25)
(require "advent-utils.rkt")

(define screen%
  (class object%
    (init width depth)
    (define cols width)
    (define rows depth)
    (define re #px"(rect|rotate column|rotate row) ((\\d+)x(\\d+)|(?:x|y)=(\\d+) by (\\d+))")
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

    (define/public (rect a b)
      (for* ((x (in-range a)) (y (in-range b)))
        (pset x y 1)))
    
    (define/public (rotate-row a b)
      (define row 
        (for/vector ((x (in-range cols)))
          (define from (if (< x b) (+ (- cols b) x) (- x b)))
          (pref from a)))
      (for ((x (in-range cols)))
        (pset x a (vector-ref row x))))
    
    (define/public (rotate-column a b)
      (define column 
        (for/vector ((y (in-range rows)))
          (define from (if (< y b) (+ (- rows b) y) (- y b)))
          (pref a from)))
      (for ((y (in-range rows)))
        (pset a y (vector-ref column y))))

    (define/public (interpret instruction)
      (match-let (((regexp re (list _ command args rect-x rect-y shift-x shift-y)) instruction))
        (case command
          (("rect") (rect (string->number rect-x) (string->number rect-y)))
          (("rotate column") (rotate-column (string->number shift-x) (string->number shift-y)))
          (("rotate row") (rotate-row (string->number shift-x) (string->number shift-y))))))
    
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
(displayln (send test-screen lit-pixels))

(define puzzle-screen (new screen% [width 50] [depth 6]))
(send puzzle-screen print)

(define puzzle-input (read-input "input8.txt"))
(for ((instruction puzzle-input))
  (send puzzle-screen interpret instruction))
(send puzzle-screen print)
(displayln (send puzzle-screen lit-pixels))