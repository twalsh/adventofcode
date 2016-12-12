#lang racket

(require rackunit)
(require "advent-utils.rkt")

(define (make-computer)
  (make-hash '((a . 0) (b . 0) (c . 0) (d . 0))))

(define (load-program file-name)
  (list->vector (read-table file-name)))

(define (interpret program [computer (make-computer)])
  (define (deref arg) (if (symbol? arg) (hash-ref computer arg) arg))
  
  (let loop ((counter 0))
    (cond ((= counter (vector-length program))
           computer)
          (else
           (define command (vector-ref program counter))
           (define instruction (first command))
           (define arg1 (second command))
           (define next-counter
             (case instruction
               ((cpy)
                (hash-set! computer (third command) (deref arg1))
                (add1 counter))
               ((jnz)
                (if (not (zero? (deref arg1)))
                    (+ counter (third command))
                    (add1 counter)))
               (else
                   ; Must be an inc or dec
                (define op (if (eq? instruction 'inc) add1 sub1))
                (hash-update! computer arg1 op)
                (add1 counter))))
           (loop next-counter)))))
          
(define test-program (load-program "test12.rkt"))
(define test-computer (interpret test-program))

(check-equal? (hash-ref test-computer 'a) 42 "test interpreter")

; Part 1
(define puzzle-program (load-program "input12.txt"))
(define puzzle-computer-1 (interpret puzzle-program))
(hash-ref puzzle-computer-1 'a)
; Part 2
(define puzzle-computer-2 (make-computer))
(hash-set! puzzle-computer-2 'c 1)
(interpret puzzle-program puzzle-computer-2)
(hash-ref puzzle-computer-2 'a)