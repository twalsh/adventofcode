#lang racket

(require rackunit)
(require "advent-utils.rkt")
                                        
(define lights (make-vector 1000000 #f))

(define (light-ref x y)
  (vector-ref lights (+ x (* y 1000))))

(define (light-set! x y v)
  (vector-set! lights (+ x (* y 1000)) v))

(define lines (call-with-input-file "input6.txt" read-lines))

(define (process switch)
  (lambda (line)
    (match-let
        (((pregexp "^(toggle|turn (on|off)) (\\d+),(\\d+) through (\\d+),(\\d+)"
                   (list _ op arg x0 y0 x1 y1)) line))
      (for* ((x (in-range (string->number x0) (add1 (string->number x1))))
             (y (in-range (string->number y0) (add1 (string->number y1)))))
        (switch x y arg)))))

(define (boolean-switch x y arg)
   (let ((val (match arg
                (#f (not (light-ref x y)))
                ("on" #t)
                ("off" #f))))
     (light-set! x y val)))

(for-each (process boolean-switch) lines)

; Answer to 6 Part One
(define part-one (for*/sum ((x (in-range 0 1000))
           (y (in-range 0 1000)))
           (if (light-ref x y) 1 0)))

(printf "Day 6 Part One: ~s lights are lit~n" part-one)
(check-equal? part-one 543903)

; 6 Part Two
(define (dimmer-switch x y arg)
   (let* ((current (light-ref x y))
          (val (match arg
                 (#f    (+ current 2))
                 ("on"  (add1 current))
                 ("off" (max 0 (sub1 current))))))
                 (light-set! x y val)))

(set! lights (make-vector 1000000 0))

(for-each (process dimmer-switch) lines)

; Answer to 6 Part Two
(define part-two (for*/sum ((x (in-range 0 1000))
           (y (in-range 0 1000)))
           (light-ref x y)))

(printf "Day 6 Part Two: ~s lights are lit~n" part-two)
(check-equal? part-one 543903)