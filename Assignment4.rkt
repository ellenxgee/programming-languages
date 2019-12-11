;;; Ellen Gee
;;; Assignment 4

#lang racket

(require test-engine/racket-tests)
(require 2htdp/planetcute)

;;; downseries step high low
;;; Consumes three integer numbers that represent a step, high, and low
;;; Produces a list of integers in the high-low range, starting at high and decrementing by step
(define (downseries step high low)
  (cond [(<= step 0) error "Invalid step"]
        [(> low high) '()]
        [else (cons high (downseries step (- high step) low))]))

;;; meow-string-map sList
;;; Consumes a list of strings 
;;; Produces a list of the initial strings with "meow" appended to each
(define (meow-string-map lst)
  (cond [(empty? lst) '()]
        [else (map (lambda (i) (string-append i "meow")) lst)]))

;;; list-ref-div lst n
;;; Consumes a list and a number
;;; Produces the ith element of the list where we count from zero
;;;    and i is the quotient produced when dividing n by the list's length
(define (list-ref-div lst n)
  (cond [(< n 0) error "list-ref-div: negative number"]
        [(empty? lst) error "list-ref-div: empty list"]
        [else (list-ref lst (quotient n (length lst)))]))

;;; nats
;;; Consumes nothing
;;; Produces an endless list of the Natural Numbers
(define nats (letrec
                 ([func (lambda (x)
                          (cons x (lambda () (func (+ x 1)))))])
               (lambda () (func 1))))

;;; next-k-items s k
;;; Consumes a stream s and a non-negative number k
;;; Produces a list consisting of the next k elements extracted from the stream
(define (next-k-items s k)
  (cond [(= k 0) '()]
        [else (cons (car (s)) (next-k-items (cdr (s)) (- k 1)))]))

;;; kth-item s k
;;; Consume a stream s a non-negative number k
;;; Produces the result of extracting k elements from the stream,
;;;    returning only the last element
(define (kth-item s k)
  (cond [(= k 1) (car (s))]
        [else (kth-item (cdr (s)) (- k 1))]))

;;; negate-2-and-5
;;; Consumes nothing
;;; Produces a stream of natural numbers except that
;;;    numbers divisible by 2 or 5 are negated
(define negate-2-and-5 (letrec
                           ([func (lambda (x)
                                    (cond [(= (modulo x 2) 0) (cons (* x -1) (lambda () (func (+ x 1))))]
                                          [(= (modulo x 5) 0) (cons (* x -1) (lambda () (func (+ x 1))))]
                                          [else (cons x (lambda () (func (+ x 1))))]))])
                         (lambda () (func 1))))

;;; key-heart-star
;;; Consumes nothing
;;; Produces a stream where elements of the stream alternate between key, heart, and yellow-star
(define key-heart-star (letrec
                           ([func (lambda (x)
                                    (cond [(= (modulo x 3) 1) (cons key (lambda () (func (+ x 1))))]
                                          [(= (modulo x 3) 2) (cons heart (lambda () (func (+ x 1))))]
                                          [else (cons yellow-star (lambda () (func (+ x 1))))]))])
                         (lambda () (func 1))))

;;; two-pairs-stream s
;;; Consumes a stream s
;;; Produces a new stream such that each element in the stream is the pair '(2 . k),
;;;    where k is the kth element returned from s
(define (two-pairs-stream s) (letrec
                                 ([func (lambda (x)
                                          (cons (cons 2 (car (s))) (two-pairs-stream (cdr (s)))))])
                               (lambda () (func 1))))

;;; spin-stream xs ys
;;; Consumes two lists
;;; Produces a stream where the stream returns pairs of elements from each list
;;;    and rotates forever through the lists
(define (spin-stream xs ys) (letrec
                                ([func (lambda (x)
                                         (cons (cons (list-ref xs (modulo x (length xs)))
                                                     (list-ref ys (modulo x (length ys))))
                                               (lambda () (func (+ x 1)))))])
                              (lambda () (func 0))))

;;; kvpv-lookup v vec
;;; Consumes a value v and a vector vec
;;; Produces #f if there isn't vector element with a car field equal to v,
;;;    otherwise return the (first) pair found    
(define (kvpv-lookup v vec) (letrec                                
                                ([len (vector-length vec)]
                                 [func (lambda (x)
                                         (if (< x len)
                                             (cond [(and (pair? (vector-ref vec x)) (eq? v (car (vector-ref vec x)))) (vector-ref vec x)]
                                                   [else (func (+ x 1))])
                                             #f))])
                              (func 0)))
                                         
;;; cached-lookup lst n
;;; Consumes a list of key-value pairs and a number that represents the cache size
;;; Produces a function that takes the same arguments as assoc, in other words a value and a list of key-value pairs
;;;    call this function with those arguments to find them by first checking the cache and then using assoc on the list
;;;    if it's in the cache (found with kvpv-lookup) return a pair (#t, p) where p is the result of kvpv-lookup
;;;    if it's found by assoc return a pair (#f, p) where p is the result of the assoc
;;;    otherwise return #f
;;; Discussed this problem with Isabelle Ro
(define (cached-lookup lst n)
  (let ([cache (make-vector n #f)]
        [cache-pos 0])
    (lambda (v lst)
      (let ([cache-find (kvpv-lookup v cache)]
            [list-find (assoc v lst)])
        (cond [(pair? cache-find) (cons #t cache-find)]
              [(pair? list-find) (begin 
                                   (vector-set! cache (modulo cache-pos (vector-length cache)) list-find) 
                                   (set! cache-pos (+ cache-pos 1))
                                   (cons #f list-find))]
              [else #f])))))


(test)
