;; The first three lines of this file were inserted by DrRacket. They record metadata
;; ****************************
;; Matthew Walinga 20627018
;; Assignment 10 - Question 3
;; Allowed Recursion
;; ****************************

(define (subsets1 lon)
  (subsets2 lon))


;; (subsets2 lon) produces a list of all subsets of lon
;; The procedure is as follows: (using foldr)
;; 1. Set the base case to (list empty)
;; 2. Cons the last element of lon onto (list empty)
;; 3. Append this new list to (list empty)
;; 4. Repeat steps 2 and 3 using the second-last element of lon and the
;;    previously created list instead of (list empty)
;; 5. Repeat step 4 for each element in lon

(define (subsets2 lon)
  (foldr (lambda (x y) (append (map (lambda (z) (cons x z)) y) y)) '(()) lon))


;; (subsets3 lon) behaves very similarly to subsets2, the key difference
;;  being that subsets3 uses lambda recursion to replicate the behaviour of
;;  foldr, append, and map
;; Sidenote: some research done on lambda recursion

(define (subsets3 lon)
  (((lambda (x) (x x))
    (lambda (my-foldr)
      (lambda (c b l)
        (cond [(empty? l) b]
              [else (c (first l)
                       ((my-foldr my-foldr) c b (rest l)))]))))
   (lambda (x y)
     (((lambda (x) (x x))
       (lambda (my-append)
         (lambda (l1 l2)
           (cond [(empty? l1) l2]
                 [else (cons (first l1)
                             ((my-append my-append) (rest l1) l2))]))))
      (((lambda (x) (x x))
        (lambda (my-map)
          (lambda (f l)
            (cond [(empty? l) empty]
                  [else (cons (f (first l)) ((my-map my-map) f (rest l)))]))))
       (lambda (z) (cons x z)) y) y)) (list empty) lon))


;(subsets3 '(1 2 3 4 5))
;(subsets2 '(1 2 3 4))
;(time (subsets1 '(1 2 3 4 5 6 7 8 9 10)))
;(time (subsets2 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)))
;(time (subsets3 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)))
