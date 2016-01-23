#lang racket

(struct ingredient (capacity durability flavor texture calories) #:transparent)

(define butterscotch (ingredient -1 -2  6  3 6))
(define cinnamon     (ingredient  2  3 -2 -1 3))

(struct cookie (ingredients))

(define (make-cookie ingredients)
  (define capacity (map (lambda (i) (apply * i))))
  )