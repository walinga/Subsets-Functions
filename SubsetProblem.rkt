#lang racket

;;;;;;;;;;;;;;;;;;;;;;;

;; "The Subset Problem"

;;;;;;;;;;;;;;;;;;;;;;;


;; Returns the subsets of lon using abstract list functions
(define (subsets lon)
  (foldr (lambda (x y) (append (map (lambda (z) (cons x z)) y) y)) '(()) lon))


;; Returns the sum of the elements of lon
(define (sum lon)
  (foldr + 0 lon))


;; Returns true if any list in lol sums to zero
(define (sub-sum-acc lol)
  (cond
    [(empty? lol) false]
    [(and (cons? (first lol)) (zero? (sum (first lol)))) true]
    [else (sub-sum-acc (rest lol))]))


;; Returns true if any subset of lon sums to 0
(define (sub-sum-zero? lon)
  (sub-sum-acc (subsets lon)))


(sub-sum-zero? '(1 -1))
(sub-sum-zero? '(25))
(sub-sum-zero? '(0))
(sub-sum-zero? '(2 3 4 5))
(sub-sum-zero? '())
