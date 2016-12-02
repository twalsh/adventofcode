#lang racket
(require rackunit)
(require "advent-utils.rkt")

(define (next-move move button)
  
  (cond ((eq? move #\U)
         (if (< button 3)
             button ; Can't move up from top row
             (- button 3)))
        ((eq? move #\D)
         (if (> button 5)
             button ; Can't move down from top row
             (+ button 3)))
        ((eq? move #\L)
         (if (member button '(0 3 6))
             button ; Can't move left from leftmost column
             (- button 1)))
        ((eq? move #\R)
         (if (member button '(2 5 8))
             button ; Can't move right from rightmost column
             (+ button 1)))))

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
    (let loop ((button start-button) (moves moves))
      (if (empty? moves)
          '()
          (let ((move (first moves)))
            (define next-button (next-move-fn move button))
            (cons next-button (loop next-button (rest moves))))))))


(define button-path-sq (make-button-path-fn next-move-square))
(define button-path-diamond (make-button-path-fn next-move-diamond))

(define (map-button-kb kb button)
  (vector-memq button kb))

(define (code kb button-path instructions)
  (define button-map
    (let ((start-button (map-button-kb kb 5)))
      (let loop ((instructions instructions) (start-button start-button))
        (if (empty? instructions)
            '()
            (let ((next-button-path (button-path start-button (first instructions))))
              (define next-button (last next-button-path))
              (cons next-button (loop (rest instructions) next-button))))))
    )
  (map (lambda (k) (vector-ref kb k)) button-map)
  )

(define test-instructions (map string->list '("ULL"
                                              "RRDDD"
                                              "LURDL"
                                              "UUUUD")))

(check-equal? (code square-kb button-path-sq test-instructions) '(1 9 8 5) "Test code OK")
(check-equal? (code diamond-kb button-path-diamond test-instructions) '(5 D B 3) "Test code OK")

(define instructions (map string->list (read-input "input2.dat")))
(code square-kb button-path-sq instructions)

(code diamond-kb button-path-diamond instructions)
