;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname PvsNP) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;

;;  P vs NP
;; "The Subset Problem"

;;;;;;;;;;;;;;;;;;;;;;;


(define (subsets2 lon)
  (foldr (lambda (x y) (append (map (lambda (z) (cons x z)) y) y)) '(()) lon))

(define (sub-sum-zero? lon)
  (local
    [(define subsets (subsets2 lon))
     (define (sum lon)
       (cond
         [(empty? lon) 0]
         [else (+ (first lon) (sum (rest lon)))]))
     (define (sub-sum-acc lol)
       (cond
         [(empty? lol) false]
         [(zero? (sum (first lol))) true]
         [else (sub-sum-acc (rest lol))]))]
    (sub-sum-acc subsets)))