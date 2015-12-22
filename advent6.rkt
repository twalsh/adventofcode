#lang racket

(require "advent-utils.rkt")

(define instr
  (hash "on" (lambda (lights xy)
              (hash-update lights xy (lambda (x) #t) #t))
        "off" (lambda (lights xy)
                (hash-update lights xy (lambda (x) #f) #f))
        "toggle" (lambda (lights xy)
                   (hash-update lights xy not 0))))

(define lights (make-immutable-hash))

(define lines (call-with-input-file "input6.txt" read-lines))

(define (process-line line)
  (let ((fields (string-split line)))
    (let ((instr (if (string=? (car fields) "toggle") (cons "toggle" 
                     (cadr fields))))
      instr)))

(map process-line lines)