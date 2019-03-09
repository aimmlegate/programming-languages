
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define ones (lambda () (cons 1 ones)))

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

;;6
(define dan-then-dog
  (letrec ([dan "dan.jpg"]
           [dog "dog.jpg"]
           [f (lambda (str) (cons str (lambda () (f (if (string=? str dog) dan dog)))))])
  (lambda () (f dan))))
 
;;7
(define (stream-add-zero stream)
  (letrec ([f (lambda (s)
                (let ([stream-val (car (s))]
                      [stream-fn (cdr (s))])
                  (cons (cons 0 stream-val) (lambda () (f stream-fn)))))])
  (lambda () (f stream))))

;;8
(define (make-stream list)
  (letrec ([f (lambda (l)
                (cons
                 (if (null? l) (car list) (car l))
                 (lambda () (f (if (null? l) (cdr list) (cdr l))))))])
    (lambda() (f list))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (stream-xs stream-ys)
               (cons (cons (car (stream-xs)) (car (stream-xs)))
                     (lambda () (f stream-xs stream-ys))))])
    (lambda () (f (make-stream xs) (make-stream ys)))))