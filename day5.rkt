#lang racket

(require file/md5)
(require rackunit)

(define test-door-id "abc")

(define (next-hash door-id [start 0])
  (define md5-output (md5 (string-append door-id (number->string start))))
  (define md5-prefix (take (bytes->list md5-output) 5))
  (if (equal? md5-prefix '(48 48 48 48 48))
      (values md5-output start (integer->char (bytes-ref md5-output 5)))
      (next-hash door-id (add1 start))))

(define (door-code door-id)
  (define door-code-letters
    (let loop ([index 0] [letter-count 0])
      (cond ((= letter-count 8) '())
            (else 
             (define-values (output last-index letter) (next-hash door-id index))
             (cons letter (loop (add1 last-index) (add1 letter-count)))))))
  (list->string door-code-letters))
  
;(check-true (string-prefix? (door-code test-door-id) "18f"))

(define puzzle-input "reyedfim")
(define puzzle-answer (door-code puzzle-input))

(define puzzle-solution "f97c354d")