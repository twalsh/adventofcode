#lang racket

(require rackunit)

(define test-program "1,9,10,3,2,3,11,0,99,30,40,50")

(define (load-program program)
  (list->vector
   (map string->number
        (string-split program ","))))

(define (run-program memory)
  
  (define end-program (vector-length memory))

  (define (deref p)
    (define addr (vector-ref memory p))
    (vector-ref memory addr))

  (define (get-arguments p)
    (values (deref (+ p 1))
            (deref (+ p 2))
            (vector-ref memory (+ p 3))))

  (define (put-value p v)
    (vector-set! memory p v))
   
  (let loop ((pc 0))
   
    (cond ((>= pc end-program) 'END)
          (else
           (define opcode (vector-ref memory pc))
           (cond ((= opcode 99) 'STOP)
                 (else
                  (define-values (src1 src2 dest) (get-arguments pc))
       
                  (define op (if (= opcode 1) + *))     
                  (put-value dest (op src1 src2))
                  (loop (+ pc 4))
                  )
                 )
           ))))

(define test-memory (load-program "1,0,0,0,99"))
(run-program test-memory)
(check-equal? test-memory (load-program "2,0,0,0,99"))

(set! test-memory (load-program "2,3,0,3,99"))
(run-program test-memory)
(check-equal? test-memory (load-program "2,3,0,6,99"))

(set! test-memory (load-program "2,4,4,5,99,0"))
(run-program test-memory)
(check-equal? test-memory (load-program "2,4,4,5,99,9801"))

(set! test-memory (load-program "1,1,1,4,99,5,6,0,99"))
(run-program test-memory)
(check-equal? test-memory (load-program "30,1,1,4,2,5,6,0,99"))

(define program "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,6,23,1,23,6,27,1,13,27,31,2,13,31,35,1,5,35,39,2,39,13,43,1,10,43,47,2,13,47,51,1,6,51,55,2,55,13,59,1,59,10,63,1,63,10,67,2,10,67,71,1,6,71,75,1,10,75,79,1,79,9,83,2,83,6,87,2,87,9,91,1,5,91,95,1,6,95,99,1,99,9,103,2,10,103,107,1,107,6,111,2,9,111,115,1,5,115,119,1,10,119,123,1,2,123,127,1,127,6,0,99,2,14,0,0")

(define memory (load-program program))

(vector-set! memory 1 12)
(vector-set! memory 2 2)

(run-program memory)

(printf "Day 02 part 1 ~a~n" (vector-ref memory 0))

(define reset (load-program program))

(define (get-program-output noun verb)
  (define mem (vector-copy reset))
  (vector-set! mem 1 noun)
  (vector-set! mem 2 verb)
  
  (run-program mem)

  (vector-ref mem 0))

(define part-02
  (for*/first ((noun (in-range 99))
               (verb (in-range 99))
               #:when (= (get-program-output noun verb) 19690720))
    (+ (* 100 noun) verb)))

(printf "Day 02 part 2 ~a~n" part-02)
       

            