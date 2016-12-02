#lang racket
(require rackunit)
(require "advent-utils.rkt")

(define square-kb
  #(  0 0 0 0 0
        0 1 2 3 0
        0 4 5 6 0
        0 7 8 9 0
        0 0 0 0 0
        ))

(define diamond-kb
  #(  0 0 0 0 0 0 0
        0 0 0 1 0 0 0
        0 0 2 3 4 0 0
        0 5 6 7 8 9 0 
        0 0 A B C 0 0
        0 0 0 D 0 0 0
        0 0 0 0 0 0 0
        ))

(define (kb-move-fn kb)
  (define kb-size (sqrt (vector-length kb)))
  (lambda (move button)
    (define next-button
      (case move
        ((#\U) (- button kb-size))
        ((#\D) (+ button kb-size))
        ((#\L) (- button 1))
        ((#\R) (+ button 1))))
    (case (vector-ref kb next-button)
      ((0) button)
      (else next-button))))

(define ((button-move-fn kb) start-button moves)
  (define kb-button (kb-move-fn kb))
  (for/fold ((button start-button))
            ((move moves))
    (kb-button move button)))

(define (code kb instructions)
  (define button-move (button-move-fn kb))
  (define start-button (vector-memq 5 kb))
  (define (button-map orders [button start-button])
    (if (empty? orders)
        '()
        (let ((next-button (button-move button (first orders))))
          (cons next-button (button-map (rest orders) next-button)))))
  (map (lambda (k) (vector-ref kb k)) (button-map instructions))
  )

; Tests
(define test-instructions (map string->list '("ULL"
                                              "RRDDD"
                                              "LURDL"
                                              "UUUUD")))

(check-equal? (code square-kb test-instructions) '(1 9 8 5) "Test code OK")
(check-equal? (code diamond-kb test-instructions) '(5 D B 3) "Test code OK")

(define instructions (map string->list (read-input "input2.dat")))
; Part 1
(code square-kb instructions)
; Part 2
(code diamond-kb instructions)
