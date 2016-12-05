#lang racket

(require file/md5)

(define door-id "abc")

(define (next-hash door-id [start 0])
  ;(displayln start)
  (when (> start 3231929)
      'fail)
  (when (= (remainder start 100000) 0)
    displayln start)
  (define md5-input (string-append door-id (number->string start)))
  (define md5-output (md5 md5-input))
  ;(displayln md5-output)
  (define md5-prefix (take (bytes->list md5-output) 5))
  (if (equal? md5-prefix '(48 48 48 48 48))
      (values md5-output start (integer->char (bytes-ref md5-output 5)))
      (next-hash door-id (add1 start))))

(next-hash door-id)


  