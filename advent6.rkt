#lang racket

(require srfi/25)
(require "advent-utils.rkt")
                                        
(define lights (make-array (shape 0 1000 0 1000) #f))

(define lines (call-with-input-file "input6.txt" read-lines))

(define (process line switch)
  (match-let
      (((pregexp "^(toggle|turn (on|off)) (\\d+),(\\d+) through (\\d+),(\\d+)"
                 (list _ op arg x0 y0 x1 y1)) line))
       (for* ((x (in-range (string->number x0) (add1 (string->number x1))))
              (y (in-range (string->number y0) (add1 (string->number y1)))))
         (switch lights x y arg))))

(define (boolean-switch lights x y arg)
   (let ((val (match arg
                (#f (not (array-ref lights x y)))
                ("on" #t)
                ("off" #f))))
     (array-set! lights x y val)))

(for ((line lines)) (process line boolean-switch))

; Answer to 6 Part One
(for*/sum ((x (in-range 0 1000))
           (y (in-range 0 1000)))
           (if (array-ref lights x y) 1 0))

; 6 Part Two
(define (dimmer-switch lights x y arg)
   (let* ((current (array-ref lights x y))
          (val (match arg
                 (#f    (+ current 2))
                 ("on"  (add1 current))
                 ("off" (max 0 (sub1 current))))))
                 (array-set! lights x y val)))

(set! lights (make-array (shape 0 1000 0 1000) 0))

(for ((line lines)) (process line dimmer-switch))

; Answer to 6 Part Two
(for*/sum ((x (in-range 0 1000))
           (y (in-range 0 1000)))
           (array-ref lights x y))