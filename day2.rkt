#lang racket
(require rackunit)
(require "advent-utils.rkt")

(define square-kb
  #(0 0 0 0 0
      0 1 2 3 0
      0 4 5 6 0
      0 7 8 9 0
      0 0 0 0 0
      ))

(define diamond-kb
  #(0 0 0 0 0 0 0
      0 0 0 1 0 0 0
      0 0 2 3 4 0 0
      0 5 6 7 8 9 0 
      0 0 A B C 0 0
      0 0 0 D 0 0 0
      0 0 0 0 0 0 0
      ))

(define (make-next-move-fn kb)
  (define kb-size (sqrt (vector-length kb)))
  (lambda (move button)
    (define next-button
      (cond ((eq? move #\U) (- button kb-size))
            ((eq? move #\D) (+ button kb-size))
            ((eq? move #\L) (- button 1))
            ((eq? move #\R) (+ button 1))))
    (if (not (eq? (vector-ref kb next-button) 0))
        next-button
        button)))

(define next-move-diamond (make-next-move-fn diamond-kb))
(define next-move-square (make-next-move-fn square-kb))

(define (make-button-path-fn next-move-fn)
  (lambda (start-button moves)
    (for/fold ((button start-button))
              ((move moves))
      (next-move-fn move button))))

(define button-path-sq (make-button-path-fn next-move-square))
(define button-path-diamond (make-button-path-fn next-move-diamond))

(define (code kb button-path instructions)
  (define start-button (vector-memq 5 kb))
  (define button-map
    (let loop ((instructions instructions) (button start-button))
      (if (empty? instructions)
          '()
          (let ((next-button (button-path button (first instructions))))
            (cons next-button (loop (rest instructions) next-button))))))
  (map (lambda (k) (vector-ref kb k)) button-map)
  )

; Tests
(define test-instructions (map string->list '("ULL"
                                              "RRDDD"
                                              "LURDL"
                                              "UUUUD")))

(check-equal? (code square-kb button-path-sq test-instructions) '(1 9 8 5) "Test code OK")
(check-equal? (code diamond-kb button-path-diamond test-instructions) '(5 D B 3) "Test code OK")

(define instructions (map string->list (read-input "input2.dat")))
; Part 1
(code square-kb button-path-sq instructions)
; Part 2
(code diamond-kb button-path-diamond instructions)
