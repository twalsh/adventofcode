#lang racket

(require racket/random)
(require racket/set)

(define floors
  (for/vector ((items
                '((HM LM)
                  (HG)
                  (LG)
                  ())))
    (list->mutable-set items)))

(displayln floors)

(define complete-set '(HG HM LG LM))

(define (floor-complete? floor)
  (for/and ((item complete-set))
    (set-member? floor item)))

(define directions '(-1 1))

(define (floor-ok? floor)
  ; Check floor has corresponding generator if microchip is present
  (not (or (and (set-member? floor 'HM)
                (not (set-member? floor 'HG)))
           (and (set-member? floor 'LM)
                (not (set-member? floor 'LG))))))
                        
(let loop ([floors floors]
           [floor-number 0]
           [current-path '()]
           [paths '()]
           [steps 0])
  (displayln floors)
  (cond
    ((> steps 1) 'stop)
    ((floor-complete? (vector-ref floors 3)) floors)
    (else
     (define floor (vector-ref floors floor-number))
     ; Discard empty pick list
     (define picks (rest (combinations (set->list floor))))
     (displayln picks)
    
     (for/list ((direction directions))
       (for/list ((pick picks))
         (define new-floors (set-copy floors))
         (define this-floor (vector-ref new-floors floor-number))
         (define next-floor (vector-ref new-floors (+ floor-number direction)))
         (set-add next-floor pick)
         (set-remove! this-floor pick)
         new-floors)))))
         
