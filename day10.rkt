#lang racket

(require "advent-utils.rkt")

(define lines
  (string-split
   "value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2" "\n"))

(define test-commands (map string->row lines))

(define value-commands
  (filter (λ (line) (eq? (first line) 'value))
          test-commands))

(define give-commands
  (filter (λ (line) (eq? (first line) 'bot))
          test-commands))

(define bots
  (let ((bot-setters
         (for/list ((command value-commands))
           (displayln command)
           (cons (sixth command) (second command)))))
    (make-hash
     (for/list ((bot-set bot-setters))
       (define bot-number (car bot-set))
       (define bot-chips
         (map cdr
              (filter (λ (b) (= (car b) bot-number)) bot-setters)))
       (cons (cons 'bot bot-number) (sort bot-chips <))))))

bots
(define (bot-ref bots type number)
  (hash-ref! bots (cons type number) '()))

(define (bot-set! bots type number chip) 
  (define chips (bot-ref bots type number))
  chips)

(for ((command give-commands))
  (displayln command)
  (define-values (bot-from low-type low-num high-type high-num)
    (values (second command)
            (sixth command)
            (seventh command)
            (list-ref command 10)
            (list-ref command 11)))
  (printf "~a ~a ~a ~a ~a~n" bot-from low-type low-num high-type high-num)
  (define chips (bot-ref bots 'bot bot-from))
  (displayln chips))