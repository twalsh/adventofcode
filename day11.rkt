#lang racket

(require racket/random)
(require racket/set)

(define chip-types '(hydrogen lithium))

(struct board (elevator
               chips
               generators) #:transparent)

; Return true if a chip or generator - defined as a (type . value)
; pair - is on the same floor as the elevator
(define ((in-elevator? b) item)
  (= (cdr item) (board-elevator b)))

(define (board-elevator-contents b)
  (define chips (filter (in-elevator? b) (hash->list (board-chips b))))
  (define generators (filter (in-elevator? b) (hash->list (board-generators b))))
  (cons chips generators))

(define (board-elevator-empty? b)
  (define contents (board-elevator-contents b))
  (and (empty? (first contents)) (empty? (second contents)))) 

(define (board-elevator-load b)
  (define contents (board-elevator-contents b))
  (for/sum ((items contents))
    (length items)))

(define (chip-generator chip b)
  (hash-ref (board-generators b) (first chip)))

(define (item-floor i)
  (cdr i))

(define (item-type i)
  (first i))

(define (valid-board? b)
  (cond
    ; Empty elevator
    ((board-elevator-empty? b) #f)
    ; Elevator overloaded
    ((> (board-elevator-load b) 2) #f)
    (else
     ; Check that each chip is safe
     (for/and ((chip (hash->list (board-chips b))))
       ; Get list of generators on the same floor as the chip
       (for/or ((g (hash->list (board-generators b)))
                #:when (= (item-floor g) (item-floor chip))
                )
         ; If the generator type matches the chip type then the chip is safe.
         (eq? (item-type g) (item-type chip)))))))

(define start-board
  (board 1 ; E
         #hash((hydrogen . 1)
               (lithium . 1))
         #hash((hydrogen . 2)
               (lithium . 3))))

(define (board-generator b type)
  (hash-ref (board-generators b) type))

(define (board-chip b type)
  (hash-ref (board-chips b) type))

(define (print-board b)
  (for ((floor (in-range 4 0 -1)))
    (printf "F~a " floor)
    (printf "~a " (if (= (board-elevator b) floor) "EEE" "..."))
    (for ((type chip-types))
      (define generator (board-generator b type))
      (define chip (board-chip b type))
      (display 
       (if (= generator floor)
           (format "~aG " (substring (symbol->string type) 0 2))
           "... "))
      (display 
       (if (= chip floor)
           (format "~aM " (substring (symbol->string type) 0 2))
           "... ")))
    (newline)))

start-board

(print-board start-board)

(valid-board? start-board)

;(define (start-floors)
;  (for/vector ((items
;                '((HM LM)
;                  (HG)
;                  (LG)
;                  ())))
;    (list->mutable-set items)))
;
;(define complete-set '(HG HM LG LM))
;
;(define (floor-complete? floor)
;  (for/and ((item complete-set))
;    (set-member? floor item)))
;
;(define directions '(-1 1))
;
;(define (floor-ok? floor)
;  ; Check floor has corresponding generator if microchip is present
;  (not (or (and (set-member? floor 'HM)
;                (not (set-member? floor 'HG)))
;           (and (set-member? floor 'LM)
;                (not (set-member? floor 'LG))))))
;
;(define (all-floors-ok? floors)
;  (for/and ((i (in-range 1 4)))
;    (floor-ok? (vector-ref floors i))))
;
;(define (print-floors floors)
;  
;  (for ((i (in-range 4)))
;    (displayln (set->list (vector-ref floors i)))))
;
;(define test         
;  (let loop ([floors (start-floors)]
;             [floor-number 0]
;             [steps 0])
;    (cond
;      ((floor-complete? (vector-ref floors 3)) (cons floors steps))
;      ((not (all-floors-ok? floors))
;       (displayln floors)
;       'stop)
;      ((> steps 10) floors)
;      (else
;       
;       (define floor (vector-ref floors floor-number))
;       (define picks (rest (combinations (set->list floor))))
;       (define sub-trees
;         (for/list ((direction directions)
;                    ; Cannot move down from floor 0 or up from floor 3
;                    #:when (not (or (and (= floor-number 0) (= direction -1))
;                                    (and (= floor-number 3) (= direction 1)))))
;           (for/list ((pick picks))
;             ; Move picks between floors
;             (for ((p pick))
;                (define new-floors
;                  (for/vector ((i (in-range 4)))
;                    (set-copy (vector-ref floors i))))
;               (define this-floor (vector-ref new-floors floor-number))
;               (define next-floor (vector-ref new-floors (+ floor-number direction)))
;               (set-add! next-floor p)
;               (set-remove! this-floor p)
;               (loop new-floors (+ floor-number direction) (add1 steps))))))
;       (cons (cons floors steps) sub-trees)))))
;             
;              
