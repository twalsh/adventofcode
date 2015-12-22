#lang racket

(define input
  (string->list
   (port->string (open-input-file "input1.txt"))))

(define output (let loop
           (
            (orders input)
            (floor 0)
            (pos 0)
            (first-entry 0)
           )
  (if (empty? orders)
      (cons floor first-entry)
      
      (let ((order (car orders)))
          (loop 
                 (cdr orders)
                 (cond ((eq? order #\() (+ floor 1))
                       ((eq? order #\)) (- floor 1))
		       (else floor)
		       )
                 (+ pos 1)
                 (if (and (= floor -1) (= first-entry 0))
                     pos
                     first-entry))))))
                  
(display output) 
(newline)
