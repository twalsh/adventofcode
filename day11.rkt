#lang racket

(require racket/random)
(require racket/set)

(require "advent-utils.rkt")

(define chip-types '(hydrogen lithium))

(struct board (elevator
               chips
               generators) #:transparent)

; Return true if a chip or generator - defined as a (type . value)
; pair - is on the same floor as the elevator
(define ((in-elevator? b) item)
  (eq? (cdr item) (board-elevator b)))

(define (board-elevator-contents b)
  (define chips (filter (in-elevator? b) (hash->list (board-chips b))))
  (define generators (filter (in-elevator? b) (hash->list (board-generators b))))
  (list chips generators))

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
  (car i))

(define (valid-board? b)
  (cond
    ; Empty elevator
    ((board-elevator-empty? b)
     (displayln 'EMPTY)
     #f)
    (else
     ; Check that each chip is safe
     (for/and ((chip (hash->list (board-chips b))))
       ;(printf "CHIP ~a~n" chip)
       ; Get list of generators on the same floor as the chip
       (define generators
         (filter (λ (g) (eq? (item-floor g) (item-floor chip))) (hash->list (board-generators b))))
       ;(printf "GENERATORS ~a~n" generators)
       (cond ((empty? generators) #t) ; No generators on floor so chip is safe
             (else
              (for/or ((g (hash->list (board-generators b)))
                       #:when (eq? (item-floor g) (item-floor chip)))
                ;(printf "GENERATOR ~a~n" g)
                ; If the generator type matches the chip type then the chip is safe.
                (eq? (item-type g) (item-type chip)))))))))

(define start-board
  (board 'F1 ; E
         #hash((hydrogen . F1)
               (lithium . F1))
         #hash((hydrogen . F2)
               (lithium . F3))))

(define next-board
  (board 'F2
         #hash((hydrogen . F2)
               (lithium . F1))
         #hash((hydrogen . F2)
               (lithium . F3))))

(define (board-generator b type)
  (hash-ref (board-generators b) type))

(define (board-chip b type)
  (hash-ref (board-chips b) type))

(define (print-board b)
  (for ((floor '(F4 F3 F2 F1)))
    (display floor)
    (printf " ~a " (if (eq? (board-elevator b) floor) "EEE" "..."))
    (for ((type chip-types))
      (define generator (board-generator b type))
      (define chip (board-chip b type))
      (display 
       (if (eq? generator floor)
           (format "~aG " (substring (symbol->string type) 0 2))
           "... "))
      (display 
       (if (eq? chip floor)
           (format "~aM " (substring (symbol->string type) 0 2))
           "... ")))
    (newline)))

start-board

(print-board start-board)

(valid-board? start-board)
(newline)(newline)(newline)
;(valid-board? next-board)

(define start  '((F4 #\.  #\.  #\.  #\.  #\.)  
                 (F3 #\.  #\.  #\.  LG #\.)  
                 (F2 #\.  HG #\.  #\.  #\.)  
                 (F1 E  #\.  HM #\.  LM)))


(define test-data (read-table "test11.dat"))

(define (make-board lines)
  (define chips (make-hash))
  (define generators (make-hash))
  (define elevator #f)
  (for ((line lines))
    (define floor (first line))
    (when (member 'E line)
      (set! elevator floor))
    (when (member 'HM line)
      (hash-set! chips 'hydrogen floor))
    (when (member 'HG line)
      (hash-set! generators 'hydrogen floor))
    (when (member 'LM line)
      (hash-set! chips 'lithium floor))
    (when (member 'LG line)
      (hash-set! generators 'lithium floor)))
  (board elevator chips generators))

(define (read-boards data)
    (if (empty? data)
        '()
        (cons 
         (make-board (take data 4)) (read-boards (drop data 5)))))

(define test-boards (read-boards test-data))

(for ((board test-boards))
  (cond ((not (valid-board? board))
         (displayln board)
         (print-board board))))

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
