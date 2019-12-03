#lang racket

(require racket/set)
(require rackunit)
(require "advent-utils.rkt")

(define test-directions (map (lambda (s) (string-split s  ", "))
                             '("R2, L3"
                               "R2, R2, R2"
                               "R5, L5, R5, R3")))

(struct posn (x y direction) #:transparent)

(define (start) (posn 0 0 'north))

(define (next-position move p)
  (let ((x (posn-x p)) (y (posn-y p))
                       (direction (posn-direction p)))
    (define distance (string->number (substring move 1)))
   
    (define turn (string-ref move 0))
    (if (eq? turn #\R)
        (cond ((eq? direction 'north)
               (posn (+ x distance) y 'east))
              ((eq? direction 'east)
               (posn x (- y distance) 'south))
              ((eq? direction 'south)
               (posn (- x distance) y 'west))
              ((eq? direction 'west)
               (posn x (+ y distance) 'north)))
        (cond ((eq? direction 'north)
               (posn (- x distance) y 'west))
              ((eq? direction 'west)
               (posn x (- y distance) 'south))
              ((eq? direction 'south)
               (posn (+ x distance) x 'east))
              ((eq? direction 'east)
               (posn x (+ y distance) 'north))))))

(define (follow-directions moves [p (posn 0 0 'north)])
  (define (path-extend moves p)
    (if (empty? moves)
        '()
        (let ((next-move (first moves)))
          (define next-posn (next-position next-move p))
          (cons next-posn (path-extend (rest moves) next-posn)))))
  (define final-posn (last (path-extend moves p)))
  (+ (abs (posn-x final-posn)) (abs (posn-y final-posn))))

(first test-directions)
(follow-directions (first test-directions))
  
(check-equal? (follow-directions (first test-directions)) 5 "Test example 1")
(check-equal? (follow-directions (second test-directions)) 2 "Test example 2")
(check-equal? (follow-directions (third test-directions)) 12 "Test example 3")

;(define test-path (cadr (follow-directions (string-split "R8, R4, R4, R8" ", "))));

(define (first-visited-twice path)
  (if (empty? path)
      '()
      (let ((next (first path)))
       
        (if (member next (rest path) (lambda (p t) (and (eq? (posn-x p) (posn-x t)) (eq? (posn-y p) (posn-y t)))))
            (+ (abs (posn-x next)) (abs (posn-y next)))
            (first-visited-twice (rest path))))))

;(check-equal? (first-visited-twice test-path) 4 "Test for part 2")
  
;(displayln twice)
;(displayln (+ (posn-x twice) (posn-y twice)))

(define puzzle-data (string-split (car (read-input "input1.dat")) ", "))

;(define puzzle-result (follow-directions puzzle-data))
;(displayln (car puzzle-result))
;(displayln (first-visited-twice (cadr puzzle-result)))