#lang racket

(require file/md5)
(require racket/set)
(require rackunit)

(define test-door-id "abc")

(define (valid-hash? md5-hash)
  #t)

(define (make-hash-finder valid-hash?)
  (define (hash-finder door-id)
    (let loop ([letter-count 0] [index 0])
      (cond ((= letter-count 8) '())
            (else
             (define md5-output (md5 (string-append door-id (number->string index))))
             (define md5-prefix (take (bytes->list md5-output) 5))
             (cond ((and (equal? md5-prefix '(48 48 48 48 48)) (valid-hash? md5-output))
                      (printf "~a ~a ~a~n" md5-output letter-count index)
                    (cons md5-output (loop [add1 letter-count] [add1 index]))
                  )
                   (else
                    (loop letter-count [add1 index])))))))
  hash-finder)

(define (make-decoder hash-validator hash->letters)
  (define hash-finder (make-hash-finder hash-validator))
  (lambda (door-id)
    (hash->letters (hash-finder door-id))))
  
(define (simple-hash->letters hashes)
  (define letters
    (for/list ((h hashes))
      (integer->char (bytes-ref h 5))))
  (list->string letters))

(define simple-decoder (make-decoder valid-hash? simple-hash->letters))

;(simple-decoder test-door-id)

;(check-true (string-prefix? (simple-decoder test-door-id) "18f"))



(define (hash->position md5-hash)
   (- (bytes-ref md5-hash 5) 48))

(define (complex-hash->letters hashes)
  (define letters (make-string 8))
  (for ((h hashes))
    (define position (hash->position h))
    (define letter (integer->char (bytes-ref h 6)))
    (string-set! letters position letter))
  letters)

(define position-seen (mutable-set))

(define (complex-hash-valid? md5-output)
  ; Convert sixth byte to digit
  (define position (hash->position md5-output))
  (if (set-member? position-seen position)
      #f
      (cond ((member position (range 8))
             (set-add! position-seen position)
             #t)
            (else #f))))
            

(define complex-decoder
  (begin
    (set-clear! position-seen)
    (make-decoder complex-hash-valid? complex-hash->letters)))

;(define complex-decoder-test (complex-decoder test-door-id))
;(check-equal? complex-decoder-test "05ace8e3" "complex-decoder test")

(define puzzle-input "reyedfim")
;(define puzzle-answer-1 (simple-decoder puzzle-input))
;(displayln puzzle-answer-1)
;(define puzzle-solution "f97c354d")
(set-clear! position-seen)
(define puzzle-answer-2 (complex-decoder puzzle-input))
(displayln puzzle-answer-2)