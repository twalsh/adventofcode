#lang racket

(require (for-syntax racket/match))
(require srfi/25)
(require "advent-utils.rkt")

(define-syntax (rotate2 stx)
  (match-let (((list _ axis x y) (syntax->datum stx)))
    (displayln (string? axis))
    (displayln (symbol? axis))
    (displayln x)
    (displayln y)
    (datum->syntax stx `(values ,axis ,(string->number x) ,(string->number y)))))

(rotate2 "column" "0" "1")
  
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

    (define/public (rect a b)
      (for* ((x (in-range a)) (y (in-range b)))
        (pset x y 1)))
    
    (define/public (rotate-pixel axis a b)
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

    (define re #px"(rect|rotate (column|row)) ((\\d+)x(\\d+)|(?:x|y)=(\\d+) by (\\d+))")
    
    (define/public (interpret instruction)
      (match-let (((regexp re (list _ command axis args rect-x rect-y shift-x shift-y)) instruction))
        (case command
          (("rect") (rect (string->number rect-x) (string->number rect-y)))
          (("rotate column") (rotate-pixel (string->symbol axis) (string->number shift-x) (string->number shift-y)))
          (("rotate row") (rotate-pixel (string->symbol axis) (string->number shift-x) (string->number shift-y))))))
    
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