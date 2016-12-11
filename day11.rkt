#lang racket

(require racket/random)
(require racket/set)

(define (start-floors)
  (for/vector ((items
                '((HM LM)
                  (HG)
                  (LG)
                  ())))
    (list->mutable-set items)))

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

(define (all-floors-ok? floors)
  (for/and ((i (in-range 1 4)))
    (floor-ok? (vector-ref floors i))))

(define (print-floors floors)
  
  (for ((i (in-range 4)))
    (displayln (set->list (vector-ref floors i)))))

(define test         
  (let loop ([floors (start-floors)]
             [floor-number 0]
             [steps 0])
    (cond
      ((floor-complete? (vector-ref floors 3)) (cons floors steps))
      ((not (all-floors-ok? floors))
       (displayln floors)
       'stop)
      ((> steps 10) floors)
      (else
       
       (define floor (vector-ref floors floor-number))
       (define picks (rest (combinations (set->list floor))))
       (define sub-trees
         (for/list ((direction directions)
                    ; Cannot move down from floor 0 or up from floor 3
                    #:when (not (or (and (= floor-number 0) (= direction -1))
                                    (and (= floor-number 3) (= direction 1)))))
           (for/list ((pick picks))
             ; Move picks between floors
             (for ((p pick))
                (define new-floors
                  (for/vector ((i (in-range 4)))
                    (set-copy (vector-ref floors i))))
               (define this-floor (vector-ref new-floors floor-number))
               (define next-floor (vector-ref new-floors (+ floor-number direction)))
               (set-add! next-floor p)
               (set-remove! this-floor p)
               (loop new-floors (+ floor-number direction) (add1 steps))))))
       (cons (cons floors steps) sub-trees)))))
             
              
