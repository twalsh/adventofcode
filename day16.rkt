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

(define (make-checksum data)
  (define checksum
    (for/list ((i (in-range 0 (length data) 2)))
      (if (eq? (list-ref data i) (list-ref data (add1 i)))
          #\1
          #\0)))
  (if (odd? (length checksum))
      checksum
      (make-checksum checksum)))

(define test-disk-length 20)
(define test-initial-data "10000")

(define test-data
  (make-data test-disk-length test-initial-data))

(check-equal? (list->string test-data) "10000011110010000111110" "make-data OK")

(define test-checksum (make-checksum (string->list "110010110100")))
(check-equal? (list->string test-checksum) "100" "make-checksum OK")

(define (data->checksum disk-length initial-data)
  (define data (make-data disk-length initial-data))
  (define checksum (make-checksum (take data disk-length)))
  (list->string checksum))

(define test-output
  (data->checksum test-disk-length test-initial-data))

(check-equal? test-output "01100")

(define puzzle-input "10111100110001111")
(define puzzle-length 272)
(define puzzle-output-1 (data->checksum puzzle-length puzzle-input))
(displayln puzzle-output-1)



  


        