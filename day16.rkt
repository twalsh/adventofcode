#lang racket

(require rackunit)

(define (make-data disk-length initial-data)
  (let loop ([data (string->list initial-data)])
    ;(printf "~a ~a~n" data (length data))
    (cond ((>= (length data) disk-length)
           data)
          (else
           (define next-segment
             (for/list ((digit (reverse data)))
               (if (eq? digit #\0) #\1 #\0)))
           (loop (append data (cons #\0 next-segment)))))))

(define test-disk-length 20)
(define test-initial-data "10000")

(define test-data
  (make-data test-disk-length test-initial-data))

(check-equal? (list->string test-data) "10000011110010000111110" "make-data OK")

         