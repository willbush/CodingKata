#lang racket/base

(require rackunit
         "BasicFunctions.rkt")

(check-equal? (my-reverse '(1 2 3 4)) '(4 3 2 1))
(check-equal? (my-reverse '(2 3 6 7)) '(7 6 3 2))
(check-equal? (my-reverse '(a b c d)) '(d c b a))

(check-equal? (my-map sqrt '(9 25 81 49)) '(3 5 9 7))
(check-equal? (my-map (lambda (x) (+ x 2)) '(1 2 3 4 5)) '(3 4 5 6 7))
(check-equal? (my-map (lambda (x) (* x 2)) '(1 2 3)) '(2 4 6))

(check-equal? (function-3 sqrt) 1.7320508075688772)
(check-equal? (function-3 (lambda (x) (* x 2))) 6)

(check-equal? (zipper '(1 2 3 4) '(a b c d)) '((1 a) (2 b) (3 c) (4 d)))
(check-equal? (zipper '(1 2 3) '(4 9 5 7)) '((1 4) (2 9) (3 5)))
(check-equal? (zipper '(3 5 6) '("one" 6.18 #t "two")) '((3 "one") (5 6.18) (6 #t)))
(check-equal? (zipper '(3) '()) '())

(check-equal? (segregate '(7 2 3 5 8)) '((2 8) (7 3 5)))
(check-equal? (segregate '(3 -5 8 16 99)) '((8 16) (3 -5 99)))
(check-equal? (segregate '()) '(() ()))

(check-equal? (is-member? 6 '(4 8 6 2 1)) #t)
(check-equal? (is-member? 6 '(1 2 3 4 5)) #f)

(check-equal? (my-sorted? '(1 2 3 4 5)) #t)
(check-equal? (my-sorted? '(7 25 4 15 11 34)) #f)
(check-equal? (my-sorted? '("alpha" "beta" "charlie")) #t)
(check-equal? (my-sorted? '("john" "zack" "bob")) #f)

(check-equal? (my-flatten '(1)) '(1))
(check-equal? (my-flatten '((1 2) 3)) '(1 2 3))
(check-equal? (my-flatten '(((4 3) 6) ((7 2 9 (1 0) 5 (3 1))))) '(4 3 6 7 2 9 1 0 5 3 1))

(check-equal? (filter-by-threshold '(1 2 3 4 5 6) 4) '(1 2 3))
(check-equal? (filter-by-threshold '(4 8 5 6 7) 6.1) '(4 5 6))

(check-equal? (get-elem-at-index '(4 7 9) 0) 4)
(check-equal? (get-elem-at-index '(4 7 9) 1) 7)
(check-equal? (get-elem-at-index '(4 7 9) 2) 9)
(check-equal? (get-elem-at-index '("bob" "cat" 4) 0) "bob")
(check-equal? (get-elem-at-index '("bob" "cat" 4) 1) "cat")
(check-equal? (get-elem-at-index '("bob" "cat" 4) 2) 4)

(check-equal? (deep-reverse '()) '())
(check-equal? (deep-reverse '(((4 3) 6) ((7 2 9) (5 1)))) '(((1 5) (9 2 7)) (6 (3 4))))
(check-equal? (deep-reverse '((1 2) 3)) '(3 (2 1)))
(check-equal? (deep-reverse '(3 6 9 12)) '(12 9 6 3))
(check-equal? (deep-reverse '(1 (2 (3 4 (5 6) (7 8) 9)))) '(((9 (8 7) (6 5) 4 3) 2) 1))
