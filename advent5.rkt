#lang racket
(require rackunit)
(require "advent-utils.rkt")

(define rulesets
  (list
   (hash
    'nice (list "ugknbfddgicrmopn" "aaa")
    'naughty (vector "jchzalrnumimnmhp" "haegwjzuvuyypxyu" "dvszwmarrgswjxmb")
    'rules 
    (list
     (cons "At least 3 vowels"
           (lambda (s) (>= (length (regexp-match* #px"[aeiou]" s)) 3)))
     (cons "At least 1 doubled letter" 
           (lambda (s) (not (empty? (regexp-match* #px"([a-z])\\1" s)))))
     (cons "No ab,cd,pq or xy" 
           (lambda ( s) (not (regexp-match #px"(ab|cd|pq|xy)" s))))
     )
    'expected-rules-output 
    (vector (vector #t #t #f) (vector #f #t #t) (vector #t #f #t))
    )
   (hash
    'nice (list "qjhvhtzxzqqjkmpb" "xxyxx")
    'naughty (vector "uurcxstgmygtbstg" "ieodomkazucvgmuy")
    'rules 
    (list
     (cons "Pair at least twice"
           (lambda (s)
             (not (empty? (regexp-match* #px"([a-z]{2}).*\\1" s)))))
     (cons "X*X"
           (lambda (s)
             (>= (length (regexp-match* #px"([a-z])\\w\\1" s)) 1)))
     )
    'expected-rules-output 
    (vector (vector #t #f) (vector #f #t))
    )
  ))

(define (rule-filter ruleset)
  (let ((rules (hash-ref ruleset 'rules)))
    (lambda (s)
      (andmap (lambda (rule) ((cdr rule) s)) rules))))

(define (check-ruleset ruleset)
  (let ((rules (hash-ref ruleset 'rules))
        (naughty-s (hash-ref ruleset 'naughty))
        (nice-s (hash-ref ruleset 'nice))
        (expected-rules-output (hash-ref ruleset 'expected-rules-output)))
    (let ((rule (lambda (i) (cdr (list-ref rules (- i 1))))))
      (for ((i (range (length rules))))
        (let ((rule (list-ref rules i)))
          (let ((expected (vector-ref expected-rules-output i)))
            (for ((j (range (vector-length naughty-s))))
              (let ((s (vector-ref naughty-s j)))
                (check-equal? 
                 ((cdr rule) s) 
                 (vector-ref expected j) 
                 (format "~s ~s" s (car rule))
                 )
                )))))
      
      (for ((s nice-s))
        (check-equal? ((rule-filter ruleset) s) #t s))
      )))

(define lines (call-with-input-file "input5.txt" read-lines))
(check-ruleset (first rulesets))
(count (rule-filter (first rulesets)) lines) 

(check-ruleset (second rulesets))
(count (rule-filter (second rulesets)) lines) 
