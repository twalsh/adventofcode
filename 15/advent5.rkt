#lang racket
(require rackunit)
(require "advent-utils.rkt")

(struct rule (desc fn))
(struct ruleset (nice naughty rules expected-rules-output))

(define rulesets
  (list
   (ruleset
    '("ugknbfddgicrmopn" "aaa")
    #("jchzalrnumimnmhp" "haegwjzuvuyypxyu" "dvszwmarrgswjxmb")
    (list
     (rule "At least 3 vowels"
           (lambda (s) (>= (length (regexp-match* #px"[aeiou]" s)) 3)))
     (rule "At least 1 doubled letter" 
           (lambda (s) (not (empty? (regexp-match* #px"([a-z])\\1" s)))))
     (rule "No ab,cd,pq or xy" 
           (lambda ( s) (not (regexp-match #px"(ab|cd|pq|xy)" s))))
     ) 
    #(#(#t #t #f) #(#f #t #t) #(#t #f #t))
    )
   (ruleset
    '("qjhvhtzxzqqjkmpb" "xxyxx")
    #("uurcxstgmygtbstg" "ieodomkazucvgmuy")
    (list
     (rule "Pair at least twice"
           (lambda (s)
             (not (empty? (regexp-match* #px"([a-z]{2}).*\\1" s)))))
     (rule "X*X"
           (lambda (s)
             (>= (length (regexp-match* #px"([a-z])\\w\\1" s)) 1)))
     )
    #(#(#t #f) #(#f #t))
    )
   ))

(define (rule-filter rs)
  (lambda (s)
    (andmap (lambda (r) ((rule-fn r) s)) (ruleset-rules rs))))

(define (check-ruleset rs)
  (let ((rules     (ruleset-rules rs))
        (naughty-s (ruleset-naughty rs))
        (nice-s    (ruleset-nice rs))
        (expected-rules-output (ruleset-expected-rules-output rs)))
    (for ((i (range (length rules))))
      (let ((r (list-ref rules i))
            (expected (vector-ref expected-rules-output i)))
        (for ((j (range (vector-length naughty-s))))
          (let ((s (vector-ref naughty-s j)))
            (check-equal? 
             ((rule-fn r) s) 
             (vector-ref expected j) 
             (format "~s ~s" s (rule-desc r))
             )
            ))))
    
    (for ((s nice-s))
      (check-equal? ((rule-filter rs) s) #t s))
    ))

(define lines (call-with-input-file "input5.txt" read-lines))
(check-ruleset (first rulesets))
(define part-one (count (rule-filter (first rulesets)) lines)) 

(check-ruleset (second rulesets))
(define part-two (count (rule-filter (second rulesets)) lines))

(check-equal? part-one 238)
(check-equal? part-two 69)

(printf "part-one: ~s~n" part-one)
(printf "part-two: ~s~n" part-two)
