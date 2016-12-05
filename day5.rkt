#lang racket

(require file/md5)
(require rackunit)

(define test-door-id "abc")

(define (valid-hash? md5-hash)
  #t)

(define (make-hash-finder valid-hash?)
  (define (hash-finder door-id start)
    (define md5-output (md5 (string-append door-id (number->string start))))
    (define md5-prefix (take (bytes->list md5-output) 5))
    (if (and (equal? md5-prefix '(48 48 48 48 48))
             (valid-hash? md5-output))
        (values md5-output start (integer->char (bytes-ref md5-output 5)))
        (hash-finder door-id (add1 start))))
  hash-finder)

(define (make-decoder hash-validator)
  (define hash-finder (make-hash-finder hash-validator))
  (define (decoder door-id)
    (define door-code-letters
      (let loop ([index 0] [letter-count 0])
        (cond ((= letter-count 8) '())
              (else 
               (define-values (output last-index letter) (hash-finder door-id index))
               (cons letter (loop (add1 last-index) (add1 letter-count)))))))
    (list->string door-code-letters))
  decoder)

(define simple-decoder (make-decoder valid-hash?))

(check-true (string-prefix? (simple-decoder test-door-id) "18f"))

;(define puzzle-input "reyedfim")
;(define puzzle-answer (door-code puzzle-input))

;(define puzzle-solution "f97c354d")

(define complex-hash-validator? 

