#lang racket

(require racket/set)
(require "advent-utils.rkt")

(define (bot-ref bots type number)
  (hash-ref! bots (cons type number) '()))

(define (bot-set! bots type number chip)
  ; Add the chip to bot's existing chips
  (define chips (sort (cons chip (bot-ref bots type number)) <))
  ;;(printf "CHIP ~a ~a ~a~n" type number chips)
  (hash-set! bots (cons type number) chips))

(define (make-bots commands)
  (define value-commands
    (filter (λ (line) (eq? (first line) 'value)) commands))
  
  (let ((bot-setters
         (for/list ((command value-commands))
           ;(displayln command)
           (cons (sixth command) (second command)))))
    (make-hash
     (for/list ((bot-set bot-setters))
       (define bot-number (car bot-set))
       (define bot-chips
         (map cdr
              (filter (λ (b) (= (car b) bot-number)) bot-setters)))
       (cons (cons 'bot bot-number) (sort bot-chips <))))))

(struct instr (id from chip) #:transparent)
(define (make-instructions command)
  (define-values (bot-from low-type low-num high-type high-num)
    (values (second command)
            (sixth command)
            (seventh command)
            (list-ref command 10)
            (list-ref command 11)))
  (list (instr (cons low-type low-num) bot-from first)
        (instr (cons high-type high-num) bot-from second)))

(define (run-commands commands)
  (define bots (make-bots commands))
  
  (define give-commands
    (filter (λ (line) (eq? (first line) 'bot)) commands))
 
  (define instructions
    (flatten
     (for/list ((command give-commands))
       ;(displayln command)
       (make-instructions command))))
  (define instruction-done (mutable-set))
  ;(displayln instructions)
  (newline)
  (newline)
  (newline)

  (define (chipset-append chips new-chips)
      (sort (remove-duplicates
             (flatten (list chips new-chips))) <))
  
  (define (instruction-ref num)
    (define bot-id (cons 'bot num))
    ;(printf "INSTR-REF ~a ~a~n" bot-id (hash-ref bots bot-id 'empty))
    ; Get existing chips
    (define chips (hash-ref bots bot-id '()))
    ;(printf "CHIPS ~a~n" chips)
    (define sources
      (filter (λ (instr) (equal? (instr-id instr) bot-id)) instructions))
    (define source-chips
      (for/list ((source sources)
                 #:when (not (set-member? instruction-done source))
                 )
        ;(printf "SOURCE ~a~n"source)
        (define bot-from (instr-from source))
        ; Get chips from source
        (define chips (instruction-ref bot-from))
        (set-add! instruction-done source)
        ; Pick the required chip
        ((instr-chip source) chips)    
        ))
    ;(printf "SOURCE CHIPSET ~a~n" source-chips)
    
    (hash-set! bots bot-id (chipset-append chips source-chips))
    (hash-ref bots bot-id))
  
  (for ((instr instructions))
    ;(printf "INSTR ~a~n" instr)
    (define bot-from (instr-from instr))
    (define new-chip ((instr-chip instr) (instruction-ref bot-from)))
    (define bot-id (instr-id instr))
    (define initial-chipset (hash-ref bots bot-id '()))
    ;(printf "INIT CHIPSET ~a ~a~n" bot-id initial-chipset)
    (define new-chipset
      (chipset-append initial-chipset (list new-chip)))
    
    (hash-set! bots bot-id new-chipset))
    
    ;(printf "SET CHIPSET ~a~a~n" bot-id new-chipset))

  bots)


;    (define chips (bot-ref bots 'bot bot-from))
;    ;(printf "CHIPSET ~a~n" chips)
;    (bot-set! bots low-type low-num (first chips))
;    (bot-set! bots high-type high-num (second chips)))
;  (for ((command give-commands))
;    (displayln command)
;    (define-values (bot-from low-type low-num high-type high-num)
;      (values (second command)
;              (sixth command)
;              (seventh command)
;              (list-ref command 10)
;              (list-ref command 11)))
;    (printf "~a ~a ~a ~a ~a~n" bot-from low-type low-num high-type high-num)
;    (define chips (bot-ref bots 'bot bot-from))
;    ;(printf "CHIPSET ~a~n" chips)
;    (bot-set! bots low-type low-num (first chips))
;    (bot-set! bots high-type high-num (second chips)))


(define lines
  (string-split
   "value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2" "\n"))

(define test-commands (map string->row lines))

(define test-bot-expected #hash(((output . 1) . (2))
                                ((bot . 2) . (2 5))
                                ((bot . 1) . (2 3))
                                ((bot . 0) . (3 5))
                                ((output . 0) . (5))
                                ((output . 2) . (3))))

(define test-bot-output (run-commands test-commands))
test-bot-output

(define puzzle-input (read-table "input10.txt"))
;(car puzzle-input)

(define puzzle-bots (run-commands puzzle-input))

(define puzzle-bot-list (hash->list puzzle-bots))
; Part 1
(filter (λ (bot) (equal? (cdr bot) '(17 61))) puzzle-bot-list)
; Part 2
(for/product ((i (in-range 3)))
  (car (hash-ref puzzle-bots (cons 'output i))))

