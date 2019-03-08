
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;;1
(define (sequence low hight stride)
  (define (aux l acc)
    (let ([next (+ l stride)])
     (if (> next hight)
         (cons l acc)
         (aux next (cons l acc)))))
  (if (> low hight) (list) (reverse (aux low (list)))))
  
;;2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;;3
(define (list-nth-mod xs n)
  (define (get-n-element list n) (car (list-tail list n)))
  (cond
    [(< n 0) (error "list-nth-mod: negative number")]
    [(= (length xs) 0)(error "list-nth-mod: empty list") ]
    [#t (get-n-element xs (remainder n (length xs)))]))

;;4
(define (stream-for-n-steps s n)
  (define (aux acc stream)
    (if (= (length acc) n)
        acc
        (aux (cons (car (stream)) acc) (cdr (stream)))))
  (reverse (aux (list) s)))

;;5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (let ([next (+ (abs x) 1)]) (if (= (remainder next 5) 0) (- 0 next) next))))))])
    (lambda () (f 1))))
  