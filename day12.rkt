#lang racket

(require rackunit)
(require "advent-utils.rkt")

(define (make-computer)
  (make-hash '((a . 0) (b . 0) (c . 0) (d . 0))))

(define (load-program file-name)
  (list->vector (read-table file-name)))

(define (interpret computer program)
  (define (deref arg) (if (symbol? arg) (hash-ref computer arg) arg))
  
  (let loop ((counter 0))
    (cond ((= counter (vector-length program))
           (displayln 'stop)
           computer)
          (else
           (define command (vector-ref program counter))
          ; (printf "~a/~a ~a ~a~n" counter (vector-length program) command computer)
           (define instruction (first command))
           (define arg1 (second command))
           (case instruction
             ((cpy)
              (define copy-value (deref arg1))
              (hash-set! computer (third command) copy-value)
              (loop (add1 counter)))
             (else
              (cond ((eq? instruction 'jnz)
                     (define jump-cond (deref arg1))
                     (cond ((not (zero? jump-cond))
                            (loop (+ counter (third command))))
                           (else
                            (loop (add1 counter)))))
                    (else
                     ; Must be an inc or dec
                     (define register arg1)
                     (define op (if (eq? instruction 'inc) add1 sub1))
                     (hash-update! computer register op)
                     (loop (add1 counter))))))))))
           
          
(define test-program (load-program "test12.rkt"))
(define test-computer (make-computer))
(interpret test-computer test-program)

(check-equal? (hash-ref test-computer 'a) 42 "test interpreter")

; Part 1
(define puzzle-program (load-program "input12.txt"))
(define puzzle-computer-1 (make-computer))
(interpret puzzle-computer-1 puzzle-program)
(hash-ref puzzle-computer-1 'a)
; Part 2
(define puzzle-computer-2 (make-computer))
(hash-set! puzzle-computer-2 'c 1)
(interpret puzzle-computer-2 puzzle-program)
(hash-ref puzzle-computer-2 'a)