#lang racket/base
(require racket/list)
(require racket/port)

(define input
  (string->list
   (port->string (open-input-file "input1.txt"))))


(define-values (part-one part-two _)
    (for/fold
      ([floor 0]
       [first-entry 0]
       [pos 0])
      ([order input])
      (values
        (cond ((eq? order #\() (+ floor 1))
              ((eq? order #\)) (- floor 1))
              (else floor))
        (if (and (= floor -1) (= first-entry 0))
          pos
          first-entry)
        (+ pos 1))))
                  
(printf "Part One: ~a~n" part-one)
(printf "Part Two: ~a~n" part-two)

