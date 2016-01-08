#lang racket
(require rackunit)

(define test-sequences
  (map (lambda (n)
         (string->list (number->string n)))
       '(1 11 21 1211 111221)))

(define (expand s)
  (let loop ((c (first s)) (r (rest s)) (n 0))
    (if (empty? r)
        (cons (add1 n) c)
        (if (char=? c (first r))
            (loop (first r) (rest r) (+ n 1))
            (cons (cons n c) (loop (first r) (rest r) 1)))))) 
    
(map expand test-sequences)

(for ((s test-sequences))
  (printf "~s -> ~s~n" s (expand s)))