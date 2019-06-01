#lang racket

(require racket/contract/parametric)

;;Zad.1
(define/contract (suffixes xs)
  (parametric->/c [a] (-> (listof a) (listof (listof a))))
  (define (helper xs acc)
    (if (null? xs)
        (cons acc xs)
        (cons acc
              (helper (cdr xs) (append (list (car xs)) acc)))))  
  (helper (reverse xs) null))

(suffixes '(1 2 3 4))

;;Zad.2
(define (sublists xs)
  (parametric->/c [a] (-> (listof a) (listof (listof a))))
  (if (null? xs)
      (list null)
       (append-map
        (lambda (ys) (cons (cons (car xs) ys) (list ys)))
        (sublists (cdr xs)))))

;(sublists '(1 2 3))

;;Zad.3
;1)
(define/contract (test1 x y)
  (parametric->/c [a b] (-> a b a))
  (if (not (eq? x y))
      x
      (- x 1)))

;3) coś źle
;(define/contract (test3 f1 f2)
;  (parametric->/c [a b c] (-> (-> b c) (-> a b) (-> a c)))
;  (if (and (f1 5)
;           (eq? (f2 (cons 5 2)) 1))
;      #t
;      #f))
  
;(test3 (lambda (x) (number? x))
;       (lambda (y) (car y)))
  
;;Zad.5
(define/contract (foldl-map f a xs)
  (parametric->/c [a b]
                  (-> (-> a b (cons/c a b))
                      b (listof a) (cons/c (listof a) b)))
  (define (it a xs ys)
    (if (null? xs)
        (cons (reverse ys) a)
        (let [(p (f (car xs) a))]
          (it (cdr p)
              (cdr xs)
              (cons (car p) ys)))))
  (it a xs null))

;(foldl-map (lambda (x a) (cons a (+ a x))) 0 '(1 2 3 4))

;;Zad.6
(define (expr/c e)
  (flat-rec-contract expr
                     (struct/c const number?)
                     (struct/c op symbol? expr expr)
                     (struct/c let-expr symbol? expr expr)
                     (struct/c variable symbol?)))

(struct const    (val)      #:transparent)
(struct op       (symb l r) #:transparent)
(struct let-expr (x e1 e2)  #:transparent)
(struct variable (x)        #:transparent)

(define/contract (subst x v e)
  (-> symbol? expr/c expr/c expr/c)
  (match e
    [(op s l r)   (op s (subst x v l)
                        (subst x v r))]
    [(const n)    (const n)]
    [(variable y) (if (eq? x y)
                      (const v)
                      (variable y))]
    [(let-expr y e1 e2)
     (if (eq? x y)
         (let-expr y
                   (subst x v e1)
                   e2)
         (let-expr y
                   (subst x v e1)
                   (subst x v e2)))]))

(define/contract (eval e)
  (-> expr/c (or/c number? string?))
  (match e
    [(const n)    n]
    [(op '+ l r)  (+ (eval l) (eval r))]
    [(op '* l r)  (* (eval l) (eval r))]
    [(let-expr x e1 e2)
     (eval (subst x (eval e1) e2))]
    [(variable n) (error n "cannot reference an identifier before its definition ;)")]))

;(eval (let-expr 'x (const 2) (op '+ (variable 'x) (const 1))))