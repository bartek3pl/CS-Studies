#lang racket

;definicja wyrażeń

(struct variable (x) #:transparent)
(struct const (n) #:transparent)
(struct op (symb a b) #:transparent)
(struct let-expr (x e1 e2) #:transparent)
(struct if-expr (cond e1 e2) #:transparent)
(struct cons-expr (a b) #:transparent)
(struct car-expr (a) #:transparent)
(struct cdr-expr (b) #:transparent)
(struct pair?-expr (p) #:transparent)
(struct null-expr () #:transparent)
(struct null?-expr (e) #:transparent)
(struct symbol-expr (s) #:transparent)
(struct symbol?-expr (e) #:transparent)
(struct lambda-expr (x e) #:transparent)
(struct app-expr (f e) #:transparent)
(struct letrec-expr (x e1 e2) #:transparent)
(struct set!-expr (x v) #:transparent)

(define (expr? e)
  (match e
    [(variable x) (symbol? x)]
    [(const n) (number? n)]
    [(op sym a b)
     (and (member op '(- + * /))
          (expr? a)
          (expr? b))]
    [(let-expr x e1 e2)
     (and (symbol? x)
          (expr? e1)
          (expr? e2))]
    [(letrec-expr x e1 e2)
     (and (symbol? x)
          (expr? e1)
          (expr? e2))]
    [(if-expr cond e1 e2)
     (andmap (expr? (list cond e1 e2)))]
    [(cons-expr a b)
     (andmap (expr? (list a b)))]
    [(car-expr a)
     (expr? a)]
    [(cdr-expr b)
     (expr? b)]
    [(pair?-expr p)
     (expr? p)]
    [(null-expr)
     #t]
    [(null?-expr e)
     (expr? e)]
    [(symbol-expr s)
     (symbol? s)]
    [(symbol?-expr s)
     (expr? s)]
    [(lambda-expr x e)
     (and (symbol? x)
          (expr? e))]
    [(app-expr f e)
     (andmap (expr? (list f e)))]
    [(set!-expr x v)
     (and (symbol? x)
          (expr? v))]
    [_ #f]))

;wartości zwracane przez interpreter
(struct val-symbol (s) #:transparent)
(struct closure (x b e))
;używanie w definicji zmiennej właśnie tej zdefiniowanej zmiennej
(struct blackhole ())

(define (my-value? v)
  (or (number? v)
      (boolean? v)
      (and (pair? v)
           (my-value? (car v))
           (my-value? (cdr v)))
      (and (symbol? v) (eq? v 'null))
      (and ((val-symbol? v) (symbol? (val-symbol-s v))))
      (and (closure? v)
           (symbol? (closure-x v))
           (expr? (closure-b v))
           (env? (closure-e v)))))
           
;wyszukiwanie wartości dla klucza na liście asocjacyjnej
;dwuelementowych list

(define (lookup x xs)
  (cond
    [(null? xs)
     (error x "Unknown identifier")]
    [(eq? x (caar xs))
     (cadar xs)]
    [else (lookup x (cdr xs))]))

(define (mlookup x xs)
  (cond
    [(null? xs)
     (error x "Unknown m identifier")]
    [(eq? x (mcar (mcar xs)))
     (match (mcar (mcdr (mcar xs)))
       [(blackhole) (error "Stuck in a black hole")]
       [x x])]
    [else (mlookup x (mcdr xs))]))

;ustawia wartość zmiennej na liście modyfikowalnej

(define (mupdate! x v xs)
  (define (update! ys)
    (cond
      [(null? ys) (error x "Unknown m identifier")]
      [(eq? x (mcar (mcar ys)))
       (set-mcar! (mcdr (mcar ys)) v)]
      [else (update! (mcdr ys))]))
  (begin (update! xs) xs))

;operatory do wykorzystania w interpreterze

(define (op-to-proc x)
  (lookup x `(
              (+, +)
              (*, *)
              (-, -)
              (/, /)
              (>, >)
              (>=, >=)
              (<, <)
              (<=, <=)
              (%, modulo)
              (=, =)
              (% ,modulo)
              (!= ,(lambda (x y) (not (= x y)))) 
              (&& ,(lambda (x y) (and x y)))
              (|| ,(lambda (x y) (or x y)))
              (eq? ,(lambda (x y) (eq? (val-symbol-s x)
                                       (val-symbol-s y))))
              )))

;interfejs do obsługi środowisk

(define (env-empty) null)
(define env-lookup lookup)
(define (env-add x v env) (cons (list x v) env))

(define (env? e)
  (and (list? e)
       (andmap (lambda (xs) (and (list? e)
                                 (= (length e) 2)
                                 (symbol? (car e)))))))

;interpretacja wyrażeń

(define (eval e env)
  (match e
    [(const n) n]
    [(op symb a b)
     ((op-to-proc symb) (eval a env) (eval b env))]
    [(let-expr x e1 e2)
     (eval e2 (env-add x (eval e1 env) env))]
    [(if-expr cond e1 e2)
     (if (eval cond env)
         (eval e1 env)
         (eval e2 env))]
    [(variable x) (env-lookup x env)]
    [(cons-expr a b)
     (cons (eval a env) (eval b env))]
    [(car-expr a)
     (car (eval a env))]
    [(cdr-expr b)
     (cdr (eval b env))]
    [(pair?-expr p)
     (pair? (eval p env))]
    [(null-expr)
     'null]
    [(null?-expr e)
     (eq? 'null (eval e env))]
    [(symbol-expr s)
     (val-symbol s)]
    [(lambda-expr x e)
     (closure x e env)]
    [(app-expr f e)
     (match (eval f env)
       [(closure x b c-env)
        ;ewaluujemy funkcje lambdy b, dodajemy do środowiska zmienną x
        ;z wartością wyrażenia z app-expr, w środowisku lambdy c-env
        (eval b (env-add x (eval e env) c-env))]
       [_ (error "application: not a function")])]
    ))

(define (run e)
  (eval e (env-empty)))

;przykład
(define fact-in-expr
  (letrec-expr 'fact (lambda-expr 'n
                                  (if-expr (op '= (variable 'n) (const 0))
                                           (const 1)             ;wywołujemy rekurencyjnie 'fact z argumenten (- n 1)
                                           (op '* (variable 'n) (app-expr (variable 'fact) 
                                                                          (op '- (variable 'n) (const 1))))))
               (app-expr (variable 'fact) ;wywołujemy 'fact z argumentem (const 5)
                         (const 5))))

;;Zad.1
(define expr1
  (let-expr 'x (const 5) (lambda-expr 'z (let-expr 'y (const 5) (op '+ (op '+ (variable 'x) (variable 'y)) (variable 'z))))))
(define expr2
  (let-expr 'x (const 5) (lambda-expr (variable 'x) (let-expr 'y (const 5) (op '+ (variable 'x) (variable 'y))))))

(define (expr-to-closure e)
  (define (helper e env)
    (match e
      [(let-expr x e1 e2)
       (helper e2 (env-add x e1 (env-empty)))]
      [(lambda-expr x e)
       (list 'closure x e env)]))
  (helper e (env-empty)))

;;Zad.2
;Wynikiem jest '(1 2 3 . null), ponieważ właśnie tak zdefiniowaliśmy null'a w naszym języku, jako symbol 'null.

