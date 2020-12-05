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

(define (extend-path start end path)
  (if (equal? start end)
      path
      (let ((y (posn-y start))
            (x (posn-x start))
            (direction (posn-direction end)))
        (define next-posn
          (cond ((eq? direction 'north) (posn (add1 y) x 'north))
                ((eq? direction 'east) (posn y (add1 x) 'east))
                ((eq? direction 'south) (posn (sub1 y) x 'south))
                ((eq? direction 'west) (posn y (sub1 x) 'west))))
        (extend-path next-posn end (cons next-posn path))))) 
                          
(define (follow-directions moves [p (posn 0 0 'north)] [path (list (posn 0 0 'north))])
  (let ((y (posn-y p)) (x (posn-x p)))
    (if (empty? moves)
        (list (+ (abs y) (abs x)) (reverse path))
        (let ((next-move (first moves)))
          (define next-posn (next-position next-move p))
          (follow-directions (rest moves)
                             next-posn
                             (extend-path p next-posn path))))))
  
(check-equal? (car (follow-directions (first test-directions))) 5 "Test example 1")
(check-equal? (car (follow-directions (second test-directions))) 2 "Test example 2")
(check-equal? (car (follow-directions (third test-directions))) 12 "Test example 3")



(define test-path (cadr (follow-directions (string-split "R8, R4, R4, R8" ", "))))

(define (first-visited-twice path)
  (if (empty? path)
      '()
      (let ((next (first path)))
       
        (if (member next (rest path) (lambda (p t) (and (eq? (posn-x p) (posn-x t)) (eq? (posn-y p) (posn-y t)))))
            (+ (abs (posn-x next)) (abs (posn-y next)))
            (first-visited-twice (rest path))))))

(check-equal? (first-visited-twice test-path) 4 "Test for part 2")
  
;(displayln twice)
;(displayln (+ (posn-x twice) (posn-y twice)))

(define puzzle-data (string-split (car (read-input "input1.dat")) ", "))

(define puzzle-result (follow-directions puzzle-data))
(displayln (car puzzle-result))
(displayln (first-visited-twice (cadr puzzle-result)))