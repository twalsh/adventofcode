#lang racket

(require racket/random)

(define floor-map
  '(   F4 _  _  _  _  _  
          F3 _  _  _  LG _  
          F2 _  HG _  _  _  
          F1 E  _  HM _  LM))

(define floor-grid
  (list->vector
   (reverse
    (for/list ((i (in-range 2 24 6)))
      (list->vector (take (drop floor-map i) 4))))))

(displayln floor-grid)

(let loop ((floor 0) (steps 0))
  (displayln (vector-ref floor-grid floor))
  (when (< steps 10)
    (define floor-contents (vector-filter-not (λ (i) (eq? i '_)) (vector-ref floor-grid floor)))
   
    (define items
       (let ((number-of-items (vector-length floor-contents)))
         (cond ((zero? number-of-items) '())
               ((= number-of-items 1) floor-contents)
               (else
                
                (define item-numbers (random-sample (range number-of-items) (random 1 number-of-items)))
                (for/list ((i item-numbers)) (vector-ref floor-contents i))))))
    (define direction
      (case floor
        ((0) 1)
        ((3) -1)
        (else (if (zero? (random 2)) -1 1))))
    (displayln items)
   
    (loop (+ floor direction) (add1 steps))))
