#lang racket

(require rackunit)
(require "advent-utils.rkt")

(define test-directions (map (lambda (s) (string-split s  ", "))
                             '("R2, L3"
                               "R2, R2, R2"
                               "R5, L5, R5, R3")))

(define (follow-directions moves [n-s 0] [e-w 0] [direction 'north])
  (if (empty? moves)
      (+ (abs n-s) (abs e-w))
      (let ((next-move (first moves)))
        (define distance (string->number (substring next-move 1)))
        (if (eq? (string-ref next-move 0) #\R)
            (cond ((eq? direction 'north)
                   (follow-directions (rest moves) n-s (+ e-w distance) 'east))
                  ((eq? direction 'east)
                   (follow-directions (rest moves) (+ n-s distance) e-w 'south))
                  ((eq? direction 'south)
                   (follow-directions (rest moves) n-s (- e-w distance) 'west))
                  ((eq? direction 'west)
                   (follow-directions (rest moves) (- n-s distance) e-w 'north)))
            (cond ((eq? direction 'north)
                   (follow-directions (rest moves) n-s (- e-w distance) 'west))
                  ((eq? direction 'west)
                   (follow-directions (rest moves) (+ n-s distance) e-w 'south))
                  ((eq? direction 'south)
                   (follow-directions (rest moves) n-s (+ e-w distance) 'east))
                  ((eq? direction 'east)
                   (follow-directions (rest moves) (- n-s distance) e-w 'north)))))))

(check-equal? (follow-directions (first test-directions)) 5 "Test example 1")
(check-equal? (follow-directions (second test-directions)) 2 "Test example 2")
(check-equal? (follow-directions (third test-directions)) 12 "Test example 3")

(define puzzle-data (string-split (car (read-input "input1.dat")) ", "))
puzzle-data

(follow-directions puzzle-data)
