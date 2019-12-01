#lang racket
(require rackunit)

(define test-sequences
  (map (lambda (n)
         (string->list (number->string n)))
       '(1 11 21 1211 111221)))

(define (expand seq)
  (flatten (map (lambda (p)
                  (cons (integer->char (+ 48 (car p))) (cdr p)))
                (rest (reverse (let loop ((r seq) (c #\_) (n 0) (p '()))
                                 (if (empty? r)
                                     (cons (cons n c) p)
                                     (if (char=? c (first r))
                                         (loop (rest r) c (+ n 1) p)
                                         (loop (rest r) (first r) 1 (cons (cons n c) p))))))))))

(define input (string->list "1113222113"))

(define (expand-iter e n)
  (for/fold
   ((e input))
   ((i (in-range n)))
    (values (expand e))))

(define expanded-input (expand-iter input 40))

(define part-one (length expanded-input))
(printf "Day 10. Part One: ~s~n" part-one)
(check-equal? part-one 252594)

(define part-two (length (expand-iter input 50)))
(printf "Day 10. Part Two: ~s~n" part-two)
(check-equal? part-one 252594)