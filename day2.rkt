#lang racket

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


(define instructions (map string->list '("ULL"
                                         "RRDDD"
                                         "LURDL"
                                         "UUUUD")))

(define (button-path start-button moves)
 ; (displayln start-button)
  (if (empty? moves)
      '()
      (let ((move (first moves)))
        (define next-button (next-move move start-button))
    ;    (printf "next ~a~n" next-button)
        (cons next-button (button-path next-button (rest moves))))))

(button-path 4 (first instructions))
(button-path 0 (first instructions))

;(let loop ((instructions instructions) (start-button 4))
;  (if (empty? instructions)
;      '()
;      (let ((next-button-path (button-path start-button (first instructions))))
;        (define next-button (first next-button-path))
;        (cons next-button (loop (rest instructions) next-button)))))
