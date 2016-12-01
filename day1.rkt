#lang racket

(require racket/set)
(require rackunit)
(require "advent-utils.rkt")

(define test-directions (map (lambda (s) (string-split s  ", "))
                             '("R2, L3"
                               "R2, R2, R2"
                               "R5, L5, R5, R3")))

(struct posn (y x direction) #:transparent)

(define (start) (posn 0 0 'north))

(define (next-position move p)
  (let ((y (posn-y p)) (x (posn-x p))
                       (direction (posn-direction p)))
    (define distance (string->number (substring move 1)))
    (define turn (string-ref move 0))
    (if (eq? turn #\R)
        (cond ((eq? direction 'north)
               (posn y (+ x distance) 'east))
              ((eq? direction 'east)
               (posn (- y distance) x 'south))
              ((eq? direction 'south)
               (posn y (- x distance) 'west))
              ((eq? direction 'west)
               (posn (+ y distance) x 'north)))
        (cond ((eq? direction 'north)
               (posn y (- x distance) 'west))
              ((eq? direction 'west)
               (posn (- y distance) x 'south))
              ((eq? direction 'south)
               (posn y (+ x distance) 'east))
              ((eq? direction 'east)
               (posn (+ y distance) x 'north))))))
    
(define (follow-directions moves [path (list [posn 0 0 'north])])
  (define p (car path))
  (let ((y (posn-y p)) (x (posn-x p)))
    (if (empty? moves)
        (list (+ (abs y) (abs x)) (cons p path))
        (let ((next-move (first moves)))
          (follow-directions (rest moves)
                             (cons (next-position next-move p) path))))))
  
(check-equal? (car (follow-directions (first test-directions))) 5 "Test example 1")
(check-equal? (car (follow-directions (second test-directions))) 2 "Test example 2")
(check-equal? (car (follow-directions (third test-directions))) 12 "Test example 3")

(define puzzle-data (string-split (car (read-input "input1.dat")) ", "))

(follow-directions puzzle-data)

(follow-directions (string-split "R8, R4, R4, R8" ", "))
;(displayln twice)
;(displayln (+ (posn-x twice) (posn-y twice)))

