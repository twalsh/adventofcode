#lang racket

(require racket/hash)
(require racket/set)
(require rackunit)

(require graph)
(require srfi/1)

(require "advent-utils.rkt")

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

(define floors '(F1 F2 F3 F4))

(struct posn (item type floor) #:transparent)

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

(define (print-board b chip-types)
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

(define (element-codes elements [len 2])
  (for/hash ((element elements))
    (define code (substring (symbol->string element) 0 len))
    (values code element)))

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

  (displayln element-codes)
  
  (for ((line lines))
    (define floor (first line))
    (when (or (member 'E line) (member 'EEE line))
      (set! elevator floor))
    (for ((code (hash-keys chip-codes)))
      (when (member code line)
        (define element (hash-ref chip-codes code))
        (hash-set! chips element (item 'chip element floor))))
    (for ((code (hash-keys generator-codes)))
      (when (member code line)
        (define element (hash-ref generator-codes code))
        (hash-set! generators element (item 'generator element floor)))))
    
  (board elevator chips generators))

(define (read-boards initial-data elements)
  (define codes (element-codes elements 1))
  (let loop ([data initial-data])
    (if (empty? data)
        '()
        (cons 
         (make-board (take data 4) codes) (loop (drop data 5))))))

(define (make-posns prefix type)
  (for/list ((floor floors))
    (posn prefix type floor)))

(define elevator-posns (make-posns 'E 'E))

(define (valid-boards chip-types)
  (define chip-posns
    (for/list ((type chip-types))
      (make-posns 'chip type)))

  (define generator-posns
    (for/list ((type chip-types))
      (make-posns 'generator type)))

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

  (filter valid-board? possible-boards))

(define (all-moves chip-types)
  (append*
   (for/list ((board (valid-boards chip-types)))
     (for/list ((move (board-moves board)))
       (list board move)))))

(define (move-graph chip-types) (unweighted-graph/undirected (all-moves chip-types)))

(define test-chip-types '(Hydrogen Lithium))
(define test-data (read-table "test11.dat"))

(newline)(newline)(newline)

(define test-boards (read-boards test-data test-chip-types))

;(for ((board test-boards))
;  (cond ((not (valid-board? board))
;         (displayln 'FAIL)
;         (displayln board)
;         (print-board board))))

(define start (first test-boards))

(define (moves chip-types start)
  (define-values (distances _) (bfs (move-graph chip-types) start))
  distances)

(define test-moves (moves test-chip-types start))
(define test-distance (hash-ref test-moves (last test-boards)))
(check-equal? test-distance 11 "Test OK")

(define puzzle-input (read-input "input11.txt"))

(define-values (puzzle-start puzzle-elements)
  (let ((puzzle-chips (make-hash))
        (puzzle-generators (make-hash))
        (elements (mutable-set)))
    (for ((i (in-range 4)))
      (printf "~a ~a~n" i (list-ref floors i))
      (define line (list-ref puzzle-input i))
      (define floor (list-ref floors i))
      (displayln line)
      (define element-chips (map
                        string->symbol
                        (regexp-match* #px"(\\w+)-compatible microchip" line #:match-select cadr)))
      
      (for ((element element-chips))
        (set-add! elements element)
        (hash-set! puzzle-chips element (item 'chip element floor)))
      (define element-generators (map
                                  string->symbol
                                  (regexp-match* #px"(\\w+) generator" line #:match-select cadr)))
      (for ((element element-generators))
        (hash-set! puzzle-generators element (item 'generator element floor))))
    (values (board 'F1 puzzle-chips puzzle-generators) elements)))

(displayln 'PUZZLE-START)
(print-board puzzle-start puzzle-elements)

(define puzzle-end-data 
  '("F4 EEE ruG ruM cuG cuM coG coM prG prM plG plM" 
    "F3 ... ... ... ... ... ... ... ... ... ... ..." 
    "F2 ... ... ... ... ... ... ... ... ... ... ..." 
    "F1 ... ... ... ... ... ... ... ... ... ... ..."))

(define puzzle-end-table
  (map string->row puzzle-end-data))

(define puzzle-element-codes (element-codes puzzle-elements))
(define puzzle-end (make-board puzzle-end-table puzzle-element-codes))
(displayln 'PUZZLE-END)
(print-board puzzle-end puzzle-elements)

(define puzzle-moves (moves puzzle-elements puzzle-start))



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
