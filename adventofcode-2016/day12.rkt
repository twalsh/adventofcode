#lang racket

(require rackunit)
(require "advent-utils.rkt")

(define (make-registers)
  (make-hash '((a . 0) (b . 0) (c . 0) (d . 0))))

(define (load-program file-name)
  (list->vector (read-table file-name)))

(define (interpret program [registers (make-registers)])
  (define (deref arg) (if (symbol? arg) (hash-ref registers arg) arg))
  
  (let loop ((counter 0))
    (cond ((= counter (vector-length program))
           registers)
          (else
           (define instruction (vector-ref program counter))
           (define operation (first instruction))
           (define arg1 (second instruction))
           (define next-counter
             (case operation
               ((cpy)
                (hash-set! registers (third instruction) (deref arg1))
                (add1 counter))
               ((jnz)
                (if (not (zero? (deref arg1)))
                    (+ counter (third instruction))
                    (add1 counter)))
               (else
                (hash-update! registers arg1 (if (eq? operation 'inc) add1 sub1))
                (add1 counter))))
           (loop next-counter)))))
          
(define test-program (load-program "test12.rkt"))
(define test-registers (interpret test-program))

(check-equal? (hash-ref test-registers 'a) 42 "test interpreter")

; Part 1
(define puzzle-program (load-program "input12.txt"))
(define puzzle-registers-1 (interpret puzzle-program))
(hash-ref puzzle-registers-1 'a)
; Part 2
(define puzzle-registers-2 (make-registers))
(hash-set! puzzle-registers-2 'c 1)
(interpret puzzle-program puzzle-registers-2)
(hash-ref puzzle-registers-2 'a)