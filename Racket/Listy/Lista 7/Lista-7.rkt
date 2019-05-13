#lang racket

;; Drzewa BST

(struct leaf () #:transparent)
(struct node (v l r) #:transparent)

(define (tree? t)
  (match t
    [(leaf) #t]
    [(node _ l r) (and (tree? l) (tree? r))]
    [_ #f]))

(define (insert-bst v t)
  (match t
    [(leaf) (node v (leaf) (leaf))]
    [(node val l r)
     (if (< v val)
         (node val (insert-bst v l) r)
         (node val l (insert-bst v r)))]))

(define tree (node 2 (node 1 (leaf) (leaf)) (node 3 (leaf) (leaf))))
(define tree2 (node 3 (node 2 (leaf) (node 6 (node 7 (leaf) (leaf)) (leaf)))
             (node 7 (leaf) (leaf))))

;; Wyrażenia arytmetyczne

(struct const (c) #:transparent)
(struct op (symb a b) #:transparent)
(struct variable () #:transparent)
(struct var (x) #:transparent)

;; Let-expr
(struct let-expr (x e1 e2) #:transparent)

;(expr? (let-expr 'x (const 2) (const 3)))

(define e1 (op '+ (const 1) (const 2)))

(define (deriv e)
  (match e
    [(const c) (const 0)]
    [(variable) (const 1)]
    [(op '+ a b)
     (op '+ (deriv a) (deriv b))]
    [(op '- a b)
     (op '- (deriv a) (deriv b))]
    [(op '/ a b)
     (op '/ (op '- (op '* (deriv a) b)
                   (op '* a (deriv b)))
         (op '* b b))]
    [(op '* a b)
     (op '+ (op '* (deriv a) b)
            (op '* a (deriv b)))]))

;;Zad.2
(struct my-if (cond e1 e2) #:transparent)
(struct my-cond (xs) #:transparent)
(struct my-lambda (args e) #:transparent)

;; Podstawienie wartości jako stałej w wyrażeniu

(define (subst x v e)
  (match e
    [(op symb a b)
     (op symb (subst x v a)
              (subst x v b))]
    [(const n) (const n)]
    [(var y)
     (if (eq? x y)
         (const v)
         (var y))]
    [(let-expr y e1 e2)
     (if (eq? x y)
         (let-expr y (subst x v e1) e2)
         (let-expr y (subst x v e1) (subst x v e2)))]
    ))

;; Gorliwa ewaluacja wyrażeń w modelu podstawieniowym
         
(define (eval e)
  (match e
    [(const c) c]
    [(op '+ a b) (+ (eval a) (eval b))]
    [(op '- a b) (- (eval a) (eval b))]
    [(op '* a b) (* (eval a) (eval b))]
    [(op '/ a b) (/ (eval a) (eval b))]
    [(let-expr x e1 e2)
     (eval (subst x (eval e1) e2))]
    [(my-if cond e1 e2)
     (if (eq? cond #t)
         (eval e1)
         (eval e2))]
    [(my-cond xs)
     (if (eq? (caar xs) #t)
         (car (cdar xs))
         (eval (my-cond (cdr xs))))]
    ;[(my-lambda args e)
     ;(eval (subst args
      ;            e
       ;           e))]
    [(var n) error "error"]
    ))

;(lambda (x y z) (+ x y z))
;(eval (my-cond (list (list (= 2 3) 7) (list (= 4 4) 9))))

;;Zad.1
(struct pow (a b) #:transparent)
(struct sum (e1 e2 f) #:transparent)
(struct integral (e1 e2 f) #:transparent)
(struct min (xs f) #:transparent)

(define (expr? e)
  (match e
    ;[(var x) (symbol? x)]
    [(const c) (number? c)]
    [(op symb a b) (and (member symb '(+ - * /))
                        (expr? a)
                        (expr? b))]
    [(let-expr x e1 e2) (and (symbol? x)
                             (expr? e1)
                             (expr? e2))]
    [(pow a b)
     (and (expr? a)
          (expr? b))]
    [(sum e1 e2 f)
     (and (expr? e1)
          (expr? e2)
          (procedure? f))]
    [(integral e1 e2 f)
     (and (expr? e1)
          (expr? e2)
          (procedure? f)
          (< e1 e2))]
    [(min xs f)
     (and (procedure? f)
          (andmap expr? xs))]
    [_ #f]))

;;Zad.3 (do poprawy)
(define (from-quote e)
  (cond
    [(number? e) (const e)]
    [(eq? (car e) '+) (op '+ (from-quote (cadr e)) (from-quote (caddr e)))]
    [(eq? (car e) '-) (op '- (from-quote (cadr e)) (from-quote (caddr e)))]
    [(eq? (car e) '*) (op '* (from-quote (cadr e)) (from-quote (caddr e)))]
    [(eq? (car e) '/) (op '/ (from-quote (cadr e)) (from-quote (caddr e)))]
    ))

;;Zad.5 Raczej źle (do poprawy)
(define (find-used e)
  (match e
    [(var x) (var x)]
    [(const c) (const c)]
    [(op symb a b)
     (remove-duplicates (append (find-used a) (find-used b)))]
    [(let-expr x e1 e2)
     (remove-duplicates (append (find-used e1) (find-used e2)))]))

(define (remove-unused-expr e xs)
  (match e
    [(var x) (var x)]
    [(const c) (const c)]
    [(op symb a b)
     (op symb (remove-unused-expr a xs) (remove-unused-expr b xs))]
    [(let-expr x e1 e2)
     (if (not (member x xs))
              (remove-unused-expr e2 xs)
              (let-expr x (remove-unused-expr e1 xs) (remove-unused-expr e2 xs)))]))

(define (optimize e)
  (remove-unused-expr e (find-used e)))

(optimize (let-expr 'x (op '+ (const 2) (const 2))
                    (let-expr 'y (op '* (const 3) (var 'x))
                      (op '+ (const 7) (var 'x)))))
     
    






