#lang racket

(require data/bit-vector)
(require rackunit)

(define (expand data-length disk-length)
  (if (>= data-length disk-length)
      data-length
      (expand (add1 (* 2 data-length)) disk-length))) 

(define (make-data disk-length initial-string)
  
  (define initial-data (string->bit-vector initial-string))
  (define initial-length (bit-vector-length initial-data))
  
  (define disk (make-bit-vector (expand initial-length disk-length) #f))
  (displayln (bit-vector-length disk))
  ; Copy initial data to disk
  (for ((i (in-range initial-length)))
    (bit-vector-set! disk i (bit-vector-ref initial-data i)))
  
  (let loop ([data-length initial-length])
    (cond ((>= data-length disk-length)
           (displayln 'return)
           disk)
          (else
           (for ((i (in-range data-length)))
             (define from (- data-length i 1))
             (define to (+ data-length i 1))
             (define digit (bit-vector-ref disk from))
             (bit-vector-set! disk to (not digit)))
           (loop (add1 (* data-length 2)))))))

(define (make-checksum data)
  (define checksum
    (for/bit-vector ((i (in-range 0 (bit-vector-length data) 2)))
      (eq? (bit-vector-ref data i) (bit-vector-ref data (add1 i)))))

  (if (odd? (bit-vector-length checksum))
      checksum
      (make-checksum checksum)))

(define test-disk-length 20)
(define test-initial-data "10000")

(define test-data
  (make-data test-disk-length test-initial-data))

(check-equal? (bit-vector->string test-data) "10000011110010000111110" "make-data OK")

(define test-checksum (make-checksum (string->bit-vector "110010110100")))
(check-equal? (bit-vector->string test-checksum) "100" "make-checksum OK")

(define (data->checksum disk-length initial-data)
  (define data (make-data disk-length initial-data))
  (define checksum (make-checksum (bit-vector-copy data 0 disk-length)))
  (bit-vector->string checksum))

(define test-output
  (data->checksum test-disk-length test-initial-data))

(check-equal? test-output "01100")

(define puzzle-input "10111100110001111")
(define puzzle-length 272)
(define puzzle-output-1 (data->checksum puzzle-length puzzle-input))
(displayln puzzle-output-1)
(check-equal? puzzle-output-1 "11100110111101110" "Part 1 OK")

(define puzzle-length-2 35651584)
(define puzzle-output-2 (data->checksum puzzle-length-2 puzzle-input))
(displayln puzzle-output-2)


  


