;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")                           
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)                       
(struct add  (e1 e2)  #:transparent)  ;; add two expressions                                     
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4                  
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function          
(struct call (funexp actual)       #:transparent) ;; function call                 
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)                
(struct apair (e1 e2)     #:transparent) ;; make a new pair                                      
(struct fst  (e)    #:transparent) ;; get first part of a pair                                   
(struct snd  (e)    #:transparent) ;; get second part of a pair                                  
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list                       
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0                          

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist list)
 (if (null? list)
     (aunit)
     (apair (car list) (racketlist->mupllist (cdr list)))))

(define (mupllist->racketlist muplist)
  (if (aunit? muplist)
      null
      (cons (apair-e1 muplist) (mupllist->racketlist (apair-e2 muplist)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) int e]
        [(aunit? e) aunit e]
        [(isaunit? e) (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
        [(closure? e) closure e]
        [(fun? e) (closure env e)]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(ifgreater? e)
         (let ([e1 (eval-under-env (ifgreater-e1 e) env)]
               [e2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? e1) (int? e2))
               (if (> (int-num e1) (int-num e2)) (eval-under-env (ifgreater-e3 e) env) (eval-under-env (ifgreater-e4 e) env))
           (error "MUPL ifgreater applied to non-number")))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-apair")))]
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        [(call? e)
         (let ([closure-val (eval-under-env (call-funexp e) env)]
               [arguments-val (eval-under-env (call-actual e) env)])
           (if (closure? closure-val)
               (letrec ([closure-fun-body (fun-body (closure-fun closure-val))]
                        [closure-fun-name (fun-nameopt (closure-fun closure-val))]
                        [closure-fun-args-name (fun-formal (closure-fun closure-val))]
                        [closure-fun-env (closure-env closure-val)]
                        [extended-env (cons (cons closure-fun-args-name arguments-val) closure-fun-env)])
                    (if closure-fun-name
                        (eval-under-env closure-fun-body (cons (cons closure-fun-name closure-val) extended-env))
                        (eval-under-env closure-fun-body extended-env)))
                  (error "MUPL call applied to non-apair")))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([var (car (car lstlst))]
            [e (cdr (car lstlst))])
        (mlet var e (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1 (mlet "_y" e2
                      (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3)))))

;; Problem 4

(define mupl-map
   (fun "map" "f"
       (fun #f "xs"
            (ifaunit (var "xs")
                     (aunit)
                     (apair (call (var "f") (fst (var "xs")))
                            (call (call (var "map") (var "f"))
                                  (snd (var "xs"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
