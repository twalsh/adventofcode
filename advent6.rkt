#lang racket

(require srfi/25)
(require "advent-utils.rkt")

(define (turn-on lights x y)
              (array-set! lights x y #t))
(define (turn-off lights x y)
              (array-set! lights x y #f))
(define (toggle lights x y)
              (array-set! lights x y (not (array-ref lights x y))))
                                                     
(define lights (make-array (shape 0 1000 0 1000) #f))

(define lines (call-with-input-file "input6.txt" read-lines))

(define (process-line line)
  (define (coord fields)
    (map string->number (flatten
     (map (lambda (s) (string-split s ","))
     (list (car fields) (caddr fields))
     )
    )))
  (display line)(newline)
  (match-let
      (((pregexp "^(toggle|turn (on|off)) (\\d+),(\\d+) through (\\d+),(\\d+)"
                (list _ instr on/off x0 y0 x1 y1)) line))
    instr))
   ; (let ((coords (map string->number (list x0 y0 x1 y1)))
    
;  #;(let ((fields (string-split line)))
 ;  (if (string=? (car fields) "toggle")
  ;;     (list toggle (coord (cdr fields)))
    ;;   (if (string=? (cadr fields) "on")
      ;;     (list turn-on  (coord (cddr fields)))
        ;   (list turn-off (coord (cddr fields)))))))

               
(for ((order (map process-line lines)))
  (match-let (((list proc (list x0 y0 x1 y1)) order))
    (for* ((x (in-range x0 (add1 x1)))
           (y (in-range y0 (add1 y1))))
      (proc lights x y))))

(for*/sum ((x (in-range 0 1000))
           (y (in-range 0 1000)))
           (if (array-ref lights x y) 1 0))
           