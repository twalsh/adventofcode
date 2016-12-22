#lang racket

(require racket/set)
(require rackunit)
(require data/heap)

(require "advent-utils.rkt")

(struct board (elevator
               chips
               generators
               [depth #:mutable]
               [distance #:mutable]
               printer
               )
  #:transparent)

(struct item (type element floor score) #:transparent)

; Return true if a chip or generator - defined as a (type . value)
; pair - is on the same floor as the elevator
(define ((on-current-floor? b) item)
  (eq? (item-floor item) (board-elevator b)))

(define (current-floor-contents b)
  (define chips (filter (on-current-floor? b) (hash-values (board-chips b))))
  (define generators (filter (on-current-floor? b) (hash-values (board-generators b))))
  (list chips generators))

(define (board-elevator-empty? b)
  (define contents (current-floor-contents b))
 
  (and (empty? (first contents)) (empty? (second contents))))

(define (board-print b) ((board-printer b) b))

(define (chip-generator chip b)
  (hash-ref (board-generators b) (first chip)))

(define floors '(F1 F2 F3 F4))


(define (next-floor floor direction)
  (if (= direction -1)
      (case floor
        ((F1) '())
        ((F2) 'F1)
        ((F3) 'F2)
        ((F4) 'F3))
      (case floor
        ((F1) 'F2)
        ((F2) 'F3)
        ((F3) 'F4)
        ((F4) '()))))

(define (floor->score floor)
  (case floor
    ((F1) 3)
    ((F2) 2)
    ((F3) 1)
    ((F4) 0)))

(define (valid-board? b)
  (cond
    ; Empty elevator
    ((board-elevator-empty? b)
     ;(displayln 'EMPTY)
     #f)
    (else
     ; Check that each chip is safe
     (for/and ((chip (hash-values (board-chips b))))
       ;(printf "CHIP ~a~n" chip)
       ; Get list of generators on the same floor as the chip
       (define all-generators (hash-values (board-generators b)))
       (define floor-generators
         (filter (λ (g) (eq? (item-floor g) (item-floor chip))) all-generators))
       ;(printf "GENERATORS ~a~n" generators)
       (cond ((empty? floor-generators) #t) ; No generators on floor so chip is safe
             (else
              (for/or ((g floor-generators)
                       #:when (eq? (item-floor g) (item-floor chip)))
                ;      (printf "GENERATOR ~a~n" g)
                ; If the generator element matches the chip element then the chip is safe.
                (eq? (item-element g) (item-element chip)))))))))

(define (board-generator b type)
  (hash-ref (board-generators b) type))

(define (board-chip b type)
  (hash-ref (board-chips b) type))

(define ((make-board-print chip-types) b)
  (displayln 'BOARD-PRINT)
  (displayln b)
  (for ((floor '(F4 F3 F2 F1)))
    (display floor)
    (printf " ~a " (if (eq? (board-elevator b) floor) "EEE" "..."))
    (for ((type chip-types))
      (define generator (board-generator b type))
      (define chip (board-chip b type))
      (display 
       (if (eq? (item-floor generator) floor)
           (format "~aG " (substring (symbol->string type) 0 2))
           "... "))
      (display 
       (if (eq? (item-floor chip) floor)
           (format "~aM " (substring (symbol->string type) 0 2))
           "... ")))
    (newline))
  (printf "~a ~a ~a~n" (board-distance b) (board-depth b) (board-score b)))

(define (element-codes elements [len 2])
  (for/hash ((element elements))
    (define code (substring (symbol->string element) 0 len))
    (values code element)))

(define (score-board b)
  (define g-score
    (for/sum ((g (hash-values (board-generators b))))
      (item-score g)))
  (define c-score
    (for/sum ((c (hash-values (board-chips b))))
      (item-score c)))
  (+ g-score c-score))

(define (make-board lines element-codes)
  (define chips (make-hash))
  (define generators (make-hash))
  (define elevator #f)

  (define chip-codes
    (for/hash ((code (hash-keys element-codes)))
      (define chip-code (string->symbol (string-append code "M")))
      (values chip-code (hash-ref element-codes code))))
  (define generator-codes
    (for/hash ((code (hash-keys element-codes)))
      (define item-code (string->symbol (string-append code "G")))
      (values item-code (hash-ref element-codes code))))

  (for ((line lines))
    (define floor (first line))
    (define score (floor->score floor))
    (when (or (member 'E line) (member 'EEE line))
      (set! elevator floor))
    (for ((code (hash-keys chip-codes)))
      (when (member code line)
        (define element (hash-ref chip-codes code))
        (hash-set! chips element (item 'chip element floor score))))
    (for ((code (hash-keys generator-codes)))
      (when (member code line)
        (define element (hash-ref generator-codes code))
        (define new-generator (item 'generator element floor (floor->score floor)))
        (hash-set! generators element new-generator))))

  (define printer (make-board-print (hash-values element-codes)))
  (define new-board 
    (board elevator chips generators 0 0 printer))
  (set-board-distance! new-board (score-board new-board))
  
  new-board)

(define (read-boards initial-data elements)
  (define codes (element-codes elements 1))
  (let loop ([data initial-data])
    (if (empty? data)
        '()
        (cons 
         (make-board (take data 4) codes) (loop (drop data 5))))))

(define (board-valid-moves b)
  ; Get list of valid moves from current board
  
  ; Flatten contents list because it's 2 separate lists of chips and generators
  (define contents (flatten (current-floor-contents b))) ;

  ; List of possible combinations of items to move, with no more than 2 allowed in the
  ; elevator at once.
  (define picks
    (for/list ((pick (rest (combinations contents)))
               #:when (<= (length pick) 2))
      pick))
  ;(displayln picks)
  (define floor (board-elevator b))

  (define possible-moves
    (flatten
     (for/list ((direction '(-1 1))
                ; Cannot move down from F1 or up from F4
                #:when (not (or (and (eq? floor 'F1) (= direction -1))
                                (and (eq? floor 'F4) (= direction 1)))))
       (define new-floor (next-floor floor direction))
       (for/list ((pick picks))
         ;        (displayln 'PICK)
         ;        (displayln pick)
         ; Copy existing board
         ; Generate new chip hash from union of existing board's and picks
         (define new-chips (hash-copy (board-chips b)))
         (define new-generators (hash-copy (board-generators b)))
         (for ((i pick))
           (define new-item (struct-copy item i [floor new-floor]))
           ;          (displayln 'NEW-ITEM)
           ;          (displayln new-item)
           (cond ((eq? (item-type i) 'chip)
                  (hash-set! new-chips (item-element i) new-item))
                 (else
                  (hash-set! new-generators (item-element i) new-item))))
      
         (define new-board (struct-copy board b
                                        [elevator new-floor]
                                        [chips new-chips]
                                        [generators new-generators]))
         ;        (displayln 'NEW-BOARD)
         ;        (print-board b)
         ;        (print-board new-board)
         new-board))))
  (filter valid-board? possible-moves))

(define (board-score b)
  (+ (board-distance b) (board-depth b)))

(define (board-score-<=? a b)
  (<= (board-score a) (board-score b)))

(define (board-equal? a b)
  (and (eq? (board-elevator a) (board-elevator b))
       (equal? (board-generators a) (board-generators b))
       (equal? (board-chips a) (board-chips b))))

(define (find-prior closed next)
  (findf
   (lambda (prior)
     (board-equal? prior next))
   (set->list closed)))
    
(define (search initial goal)
  ; Initial depth = 0
  (set-board-depth! initial 0)
  (define init-open (make-heap board-score-<=?))
  (heap-add! init-open initial)

  (define closed (mutable-set))

  (let loop ((open init-open))
    (displayln 'LOOP)
    (printf "HEAP-COUNT ~a~n" (heap-count open))
    (cond
      ((> (heap-count open) 4)
       'abort)
      ((> (heap-count open) 0)
       (displayln 'HEAP)
       (for ((b (heap->vector open)))
         (displayln b)
         (board-print b))
       (define n (heap-min open))
       (newline)
       (displayln 'HEAP-MIN)
       (board-print n)
       (set-add! closed n)
       (displayln 'CLOSED)
       (for ((b (set->list closed)))
         (board-print b))
       (newline)
       (if (board-equal? n goal)
           'solution
           (for ((next (board-valid-moves n)))
             (displayln 'NEXT)
             (set-board-depth! next (add1 (board-depth n)))
             (board-print next)
             (displayln next)
             (define prior (find-prior closed next))
             (cond (prior
                    (displayln 'FOUND-PRIOR)
                    (cond (< (board-score next) (board-score prior))
                          (set-remove! closed prior)
                          (heap-add! open next)))
                   (else
                    (heap-add! open next)))
             (loop open))))
      (else 'no-solution))))

; Test
(define test-chip-types '(Hydrogen Lithium))
(define test-data (read-table "test11.dat"))
(define test-boards (read-boards test-data test-chip-types))
(define start (first test-boards))
(displayln 'START)
(displayln start)
(displayln 'END)
(define end (last test-boards))
(displayln end)
(newline)
(define result (search start end))
result


(define puzzle-input (read-input "input11.txt"))

(define-values (puzzle-start puzzle-elements)
  (let ((puzzle-chips (make-hash))
        (puzzle-generators (make-hash))
        (elements (mutable-set)))
  
    (for ((i (in-range 4)))
      (define line (list-ref puzzle-input i))
      (define floor (list-ref floors i))
      (define score (floor->score floor))
    
      (define element-chips (map
                             string->symbol
                             (regexp-match* #px"(\\w+)-compatible microchip" line #:match-select cadr)))
      
      (for ((element element-chips))
        (set-add! elements element)
        (hash-set! puzzle-chips element (item 'chip element floor score)))
      (define element-generators (map
                                  string->symbol
                                  (regexp-match* #px"(\\w+) generator" line #:match-select cadr)))
      (for ((element element-generators))
        (hash-set! puzzle-generators element (item 'generator element floor score))))
    (define printer (make-board-print (set->list elements)))
    (values (board 'F1 puzzle-chips puzzle-generators 0 0 printer) elements)))

;(displayln 'PUZZLE-START)
;(print-board puzzle-start puzzle-elements)

(define puzzle-end-data 
  '("F4 EEE ruG ruM cuG cuM coG coM prG prM plG plM" 
    "F3 ... ... ... ... ... ... ... ... ... ... ..." 
    "F2 ... ... ... ... ... ... ... ... ... ... ..." 
    "F1 ... ... ... ... ... ... ... ... ... ... ..."))

(define puzzle-end-table
  (map string->row puzzle-end-data))

(define puzzle-element-codes (element-codes puzzle-elements))
(define puzzle-end (make-board puzzle-end-table puzzle-element-codes))
;(displayln 'PUZZLE-END)
;(print-board puzzle-end puzzle-elements)

;(define puzzle-moves (moves puzzle-elements puzzle-start))



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
