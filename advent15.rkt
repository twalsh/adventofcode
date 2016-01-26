#lang racket
(require (for-syntax racket/syntax))
(require rackunit)
(require "advent-utils.rkt")

; Cookie component - an amount of the specified ingredient
(struct component (ingredient amt) #:transparent)

; Make cookie from a list of components
(define (make-cookie components)
  (define score
    (for/fold
     ((total 1))
     ((i (range 4)))
      #:break (= total 0)
      (let ((prop
             (for/sum ((c components))
               (* (component-amt c)
                  (list-ref (component-ingredient c) i)))))
        (if (< prop 0)
            0
            (* prop total)))))
  (define calories
    (if (> score 0)
        (for/sum ((c components))
          (* (component-amt c)
             (list-ref (component-ingredient c) 4)))
        0))
  (cons score calories))

(define lines (read-input "input15.txt"))
(define ingredients
  (for/list ((l lines))
    (map string->number (regexp-match*  #px"-?\\d+" l))))

; List of cookie scores for all possible combinations of ingredients
(define cookies
  (for*/list 
      ((i (range 1 97))
       (j (range 1 (- 100 i)))
       (k (range 1 (- 100 (+ i j)))))
    (let ((l (- 100 (+ i j k))))
      (let ((components (list (component (first ingredients) i)
                              (component (second ingredients) j)
                              (component (third ingredients) k)
                              (component (fourth ingredients) l))))
        (make-cookie components)))))

; Cookie with highest score
(define part-one
  (for/fold ((score 0))
            ((c cookies))
    (max score (car c))))

(printf "Day 15. Part One: ~s~n" part-one)
(check-equal? part-one 222870)

; Highest-scoring cookie with calorie count = 500 
(define part-two
  (for/fold ((score 0))
            ((c (filter (lambda (c) (= (cdr c) 500)) cookies)))
    (max score (car c))))

(printf "Day 15. Part Two: ~s~n" part-two)
(check-equal? part-two 117936)
