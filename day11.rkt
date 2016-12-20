#lang racket

(require racket/hash)
(require racket/set)
(require rackunit)

(require graph)
(require srfi/1)

(require "advent-utils.rkt")

(define chip-types '(hydrogen lithium))

(struct board (elevator
               chips
               generators) #:transparent)

(struct item (type element floor) #:transparent)

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

(define (chip-generator chip b)
  (hash-ref (board-generators b) (first chip)))

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

(define (print-board b)
 
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
    (newline)))


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
      (hash-set! chips 'hydrogen (item 'chip 'hydrogen floor)))
    (when (member 'HG line)
      (hash-set! generators 'hydrogen (item 'generator 'hydrogen floor)))
    (when (member 'LM line)
      (hash-set! chips 'lithium (item 'chip 'lithium floor)))
    (when (member 'LG line)
      (hash-set! generators 'lithium (item 'generator 'lithium floor))))
  (board elevator chips generators))

(define (read-boards data)
  (if (empty? data)
      '()
      (cons 
       (make-board (take data 4)) (read-boards (drop data 5)))))



(define floors '(F1 F2 F3 F4))

(struct posn (item type floor) #:transparent)

(define (make-posns prefix type)
  (for/list ((floor floors))
    (posn prefix type floor)))

(define elevator-posns (make-posns 'E 'E))

(define chip-posns
  (for/list ((type chip-types))
    (make-posns 'chip type)))

(define generator-posns
  (for/list ((type chip-types))
    (make-posns 'generator type)))

;elevator-posns
;chip-posns
;generator-posns

(newline)(newline)(newline)

; All combinations of elevator and chip positions
(define elevator-chip-posns
  (map flatten
       (for/fold ((p elevator-posns))
                 ((chip chip-posns))
         (cartesian-product p chip))))

; All possible combinations of elevator, chip and generator positions.
(define possible-board-data
  (map flatten
       (for/fold ((p elevator-chip-posns))
                 ((generator generator-posns))
         (cartesian-product p generator))))

; Construct all possible boards
(define possible-boards
  (for/list ((data possible-board-data))
    (define elevator (posn-floor (first data)))
    (define chips (make-hash))
    (define generators (make-hash))
    (for ((d (rest data)))
      (if (eq? (posn-item d) 'chip)
          (hash-set! chips (posn-type d) (item 'chip (posn-type d) (posn-floor d)))
          (hash-set! generators (posn-type d) (item 'generator (posn-type d) (posn-floor d)))))
    (board elevator chips generators)))


; Get set of valid boards
(define valid-boards (filter valid-board? possible-boards))
(define valid-board-set (list->set valid-boards))
(set-count valid-board-set)

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

;(define (board-valid-move? b direction items)
  

(define (board-moves b)
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

(define all-moves
  (append*
   (for/list ((board valid-boards))
     (for/list ((move (board-moves board)))
       (list board move)))))

(define move-graph (unweighted-graph/undirected all-moves))

(define test-boards (read-boards test-data))

(for ((board test-boards))
  (cond ((not (valid-board? board))
         (displayln 'FAIL)
         (displayln board)
         (print-board board))))

(define start (first test-boards))

(define-values (distances _) (bfs move-graph start))

(check-equal? (hash-ref distances (last test-boards)) 11 "Test OK")

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
