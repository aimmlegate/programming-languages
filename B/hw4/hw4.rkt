
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
  (map (λ (x) (string-append x suffix)) xs))

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
  (letrec ([f (λ (x)
                (cons x
                      (λ () (f (let ([next (+ (abs x) 1)])
                                 (if (= (remainder next 5) 0)
                                     (- 0 next)
                                     next))))))])
    (lambda () (f 1))))

;;6
(define dan-then-dog
  (letrec ([dan "dan.jpg"]
           [dog "dog.jpg"]
           [f (λ (str) (cons str (λ () (f (if (string=? str dog) dan dog)))))])
  (λ () (f dan))))
 
;;7
(define (stream-add-zero stream)
  (letrec ([f (λ (s)
                (let ([stream-val (car (s))]
                      [stream-fn (cdr (s))])
                  (cons (cons 0 stream-val) (λ () (f stream-fn)))))])
  (lambda () (f stream))))

;;8
(define (make-stream list)
  (letrec ([f (λ (l)
                (cons
                 (if (null? l)
                     (car list)
                     (car l))
                 (λ () (f (if (null? l)
                              (cdr list)
                              (cdr l))))))])
    (λ () (f list))))

(define (cycle-lists xs ys)
  (letrec ([f (λ (stream-xs stream-ys)
               (cons (cons (car (stream-xs)) (car (stream-ys)))
                     (λ () (f (cdr (stream-xs)) (cdr (stream-ys))))))])
    (λ () (f (make-stream xs) (make-stream ys)))))

;;9
(define (vector-assoc v vec)
  (letrec ([filtred-vec (vector-filter pair? vec)]
           [vec-with-v (vector-filter (λ (pair) (equal? v (car pair))) filtred-vec)]
           [result (if (equal? (vector-length vec-with-v) 0) #f (vector-ref vec-with-v 0))])
    result))

