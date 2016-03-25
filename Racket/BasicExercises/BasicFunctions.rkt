#lang racket

(provide my-reverse)
(provide my-map)
(provide function-3)
(provide zipper)
(provide segregate)
(provide is-member?)
(provide my-sorted?)
(provide my-flatten)
(provide filter-by-threshold)
(provide get-elem-at-index)
(provide deep-reverse)

(define (my-reverse xs)
  (define (go xs result)
    (if (empty? xs)
        result
        (go (rest xs) (cons (first xs) result))))
  (go xs '()))

(define (my-map func xs)
  (define (go xs result)
    (if (empty? xs)
        (my-reverse result)
        (go (rest xs) (cons (func (first xs)) result))))
  (go xs '()))

;; simply takes a function and applies 3 to it.
(define (function-3 func) (func 3))

;; zips two lists together
(define (zipper xs ys)
  (define (go xs ys result)
    (if (or (empty? xs) (empty? ys))
        (my-reverse result)
        (go (rest xs) (rest ys) (cons (take-heads xs ys) result))))
  (go xs ys '()))

;; takes the heads from two lists and returns them in a list
(define (take-heads xs ys)
  (cons (first xs) (cons (first ys) '())))

;; Segregates the evens into one list and the odds into the other
(define (segregate xs)
  (define (go xs evens odds)
    (cond
      [(empty? xs)
       (cons (my-reverse evens) (cons (my-reverse odds) '()))]
      [(is-even? (first xs))
       (go (rest xs) (cons (first xs) evens) odds)]
      [else (go (rest xs) evens (cons (first xs) odds))]))
  (go xs '() '()))

(define (is-even? n)
  (equal? 0 (modulo n 2) ))

(define (is-member? x xs)
  (cond
    [(empty? xs) #f]
    [(equal? x (first xs)) #t]
    [else (is-member? x (rest xs))]))

(define (my-sorted? xs)
  (cond
    [(<= (length xs) 1) #t]
    [(and (first2-is-number? xs) (< (first xs) (second xs)))
     (my-sorted? (rest xs))]
    [(and (first2-is-string? xs) (string<? (first xs) (second xs)))
     (my-sorted? (rest xs))]
    [else #f]))

(define (first2-is-number? xs)
  (and (number? (first xs)) (number? (second xs))))

(define (first2-is-string? xs)
  (and (string? (first xs)) (string? (second xs))))

;; flattens list of lists into one list
(define (my-flatten xs)
  (define (go xs result)
    (cond
      [(empty? xs) result]
      [(list? (first xs))
       (go (rest xs) (go (first xs) result))]
      [else
       (go (rest xs) (cons (first xs) result))]))
  (my-reverse (go xs '())))

;; Takes a list and a threshold and returns a list of
;; all elements below that threshold.
(define (filter-by-threshold xs t)
  (define (go xs result)
    (cond
      [(empty? xs) (my-reverse result)]
      [(< (first xs) t)
       (go (rest xs) (cons (first xs) result))]
      [else (go (rest xs) result)]))
  (go xs '()))

(define (get-elem-at-index xs index)
  (cond
    [(or (< index 0) (empty? xs)) "index out of bounds"]
    [(equal? index 0) (first xs)]
    [else (get-elem-at-index (rest xs) (- index 1))]))

;; reverses a list of lists
(define (deep-reverse xs)
  (define (go xs result)
    (cond
      [(empty? xs) result]
      [(list? (first xs))
       (go (rest xs) (cons (go (first xs) '()) result))]
      [else (go (rest xs) (cons (first xs) result))]))
  (go xs '()))

