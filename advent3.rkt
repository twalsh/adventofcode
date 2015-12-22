#lang racket

(define input
  (string->list
   (port->string (open-input-file "input3.txt"))))

(define (visit-map order-list init-visits)
  (let loop
    ((x 0) (y 0)
     (visits init-visits)
     (orders order-list))
    (if (empty? orders)
        visits
        (let ((o (car orders)))
            (loop 
             (cond ((eq? o #\<) (- x 1))
                   ((eq? o #\>) (+ x 1))
                   (else x))
             (cond ((eq? o #\v) (- y 1))
                   ((eq? o #\^) (+ y 1))
                   (else y))
             (hash-update visits (cons x y) add1 0) 
             (cdr orders))
          ))))

; 3a answer
(hash-count (visit-map input (make-immutable-hash)))
; 3b
(define split-orders
  (let loop ((split (list '() '()))
             (orders input))
    (if (empty? orders)
        (map reverse split)
        (loop (list
               (cons (car orders) (car split))
               (cons (cadr orders) (cadr split)))
              (cddr orders)))))

(define santa-visits (visit-map (car split-orders) (make-immutable-hash)))
(define all-visits (visit-map (cadr split-orders) santa-visits))
; 3b answer
(hash-count all-visits)


      

