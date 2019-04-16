#lang racket
;Bartłomiej Hildebrandt
;IIUWr 2018/2019

(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))))

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
  
(define (expr? e)
  (match e
    [(var x) (symbol? x)]
    [(const c) (number? c)]    
    [(op symb a b) (and
                    (member symb '(+ - * /))
                    (expr? a)
                    (expr? b))]
    [(let-expr x e1 e2) (and
                         (symbol? x)
                         (expr? e1)
                         (expr? e2))]
    [_ #f]))

;(expr? (let-expr 'x (const 2) (const 3)))

(define (to-normal e)
  (match e
    [(const c) c]
    [(op symb a b)
     (list (to-normal a) symb (to-normal b))]))

(define (to-rpn e)
  (match e
    [(const n) (list n)]
    [(op s l r) (append (to-rpn l)
                        (to-rpn r)
                        (list s))]))

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
         (let-expr y (subst x v e1) (subst x v e2)))]))

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
    [(var n) error "dupa"]
    ))

;; Przykładowe programy

(define p1
  (let-expr 'x (op '+ (const 2) (const 2))
     (op '+ (const 1000) (let-expr 'y (op '+ (const 5) (const 5))
                            (op '* (var 'x) (var 'y))))))

;; Zad.1
(define 1a '(+ 4 3 (* 5 8)))
(define 1b (list '+ 4 3 (list '* 5 8)))

(define 2a '(+ 4 3 '(* 5 8)))
(define 2b (list '+ 4 3 ''(* 5 8)))

(define 3a '(+ 4 3 (5 . 8)))
(define 3b (list '+ 4 3 (cons 5 8)))

(define 4a '(+ 4 3 ,(* 5 8)))
(define 4b (list '+ 4 3 ', (* 5 8)))
  
;; Zad.2
(define (paths t)
  (define (helper t xs)
    (match t
      [(leaf) (list (append xs (list '*)))]
      [(node v l r)
       (append (helper l (append xs (list v)))
               (helper r (append xs (list v))))
       ]))
  (helper t null))

;(define (paths-iter t)
;  (define (helper t acc)
;    (match t
;      [(leaf) (list (append acc (list '*)))]
;      [(node v l r)
;       (helper (append (helper l (append acc (list v))) (helper r (append acc (list v))))
;               acc)
;       ]))
;  (helper t null))

;(paths tree)
;(paths tree2)
;(paths-iter tree2)

;; Zad.3
(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (how-many e)
  (match e
    [(const n) 0]
    [(op '+ a b)
     (+ 1 (+ (how-many a) (how-many b)))]
    [(op '* a b)
     (+ (- 0 1) (how-many a) (how-many b))]))

;(how-many (op '* (const 5) (op '* (op '+ (const 2) (const 3)) (const 1))))

;; Zad.4
(struct text (title author chapters) #:transparent)
(struct chapter (title elems) #:transparent)
(struct subchapter (title elems) #:transparent)
(struct elem (elem) #:transparent)

(define (book-text? t)
  (and (text? t)
       (match t
         [(text title author chapters) (and (string? title)
                                            (string? author)
                                            (andmap
                                             book-chapter? chapters))])))

(define (book-chapter? c)
  (and (chapter? c)
       (match c
         [(chapter title elems)
          (and (string? title)
               (element? elems))])))

(define (element? e) ;lista elementów
  (or (null? e)
      (and (or (elem? (car e))
               (book-chapter? (car e)))
           (element? (cdr e)))))

(book-text? (text "Title" "Author" (list
                                    (chapter "Chapter 1" (list (elem "Paragraph")))
                                    (chapter "Chapter 2" (list (elem "Paragraph 2"))))))







      