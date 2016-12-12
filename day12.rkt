#lang racket

(require "advent-utils.rkt")

(define (make-computer) (make-vector 4))

(define (load-program file-name)
  (list->vector (read-table file-name)))

(define (interpret computer program)
  (let loop ((counter 0))
    (cond ((= counter (vector-length program)) 'stop)
          (else
           (define command (vector-ref program counter))
           (displayln command)
           (loop (add1 counter))))))

(define test-program (load-program "test12.rkt"))
(define test-computer (make-computer))
(interpret test-computer test-program)

