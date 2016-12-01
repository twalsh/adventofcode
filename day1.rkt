#lang racket

(define test-directions (map (lambda (s) (string-split s  ", "))
                               (list "R2, L3" "R2, R2, R2")))

test-directions 
 
;    R     L     
; N  +1,0,E  -1,0,S
; S  -1,0,W   1,0
; E  +1,0,S  -1,0
; W  -1,0,N  +1,0

(define (go moves [n-s 0] [e-w 0] [direction 1])
  (printf "~a ~a~n" n-s e-w direction)
  (if (empty? moves)
      (cons (abs n-s) (abs e-w))
      (let ((next-move (first moves)))
        (define next-direction
          (if (eq? (string-ref next-move 0) #\R)
              (if (= direction 1)
                   1
                   -1
              (if (= direction 1)
                  -1
                  1))))
        (define distance (string->number (substring next-move 1)))
        (printf "~a ~a ~a~n" next-move next-direction distance)
        (go (rest moves) (+ x (* next-direction distance) next-direction)))))

(map go test-directions)

