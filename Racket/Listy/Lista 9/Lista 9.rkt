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
(define env-lookup mlookup)
(define (env-add x v env)
  (mcons (mcons x (mcons v '())) env))
(define env-update! mupdate!)

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
    
    ;dodajemy do środowiska nową zmienną z wartością '()
    ;przypisujemy do tej zmiennej zewaluowane e1
    ;aktualizujemy środowisko, czyli zapisujemy wartość e1 do x
    ;ewaluujemy e2 w środowisku, w którym x ma odpowiednią wartość
    [(letrec-expr x e1 e2)
     (let ((new-env (env-add x (blackhole) env)))
       (eval e2 (env-update! x
                             (eval e1 new-env)
                             new-env)))]
    
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
    [(set!-expr x v)
     (env-update! x (eval v env) env)]
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
;domknięcie to kod wraz ze środowiskiem
;pierwszy argument domknięcia to lista argumentów lambdy
;drugi to funkcja zawarta w lambdzie
;trzeci argument to środowisko przekazywane do tej lambdy
;a środowisko to lista zmiennych

;1) (let (x 5) (lambda (z) (let (y 5) (+ x y z))))
;   (closure '(z) (let-expr 'y (const 5) (op '+ (variable 'x) (op '+ (variable 'z) (variable 'y)))) '((x (const 5))))
;2) (let (x 5) (lambda (x) (let (y 5) (+ x y))))
;   (closure '(x) (let-expr 'y (const 5) (op '+ (variable 'x) (variable 'y))) '((x (const 5))))
;3) ((lambda (x) (lambda (y) (+ x y))) 10)
;   (closure '(x) (op '+ (variable 'x) (variable 'y)) '(x (y (const 10))))

;;Zad.2
(define (reverse xs)
  (define (helper xs acc)
    (if (null? xs)
        acc
        (helper (cdr xs) (cons (car xs) acc))))
  (helper xs null))

(define rev-in-expr
  (letrec-expr 'rev
               (lambda-expr 'xs
                            (lambda-expr 'acc
                                         (if-expr (null?-expr (variable 'xs))
                                                  (variable 'acc)
                                                  (app-expr (app-expr (variable 'rev)
                                                                      (cdr-expr (variable 'xs)))
                                                            (cons-expr (car-expr (variable 'xs)) (variable 'acc))))))
               (app-expr (app-expr (variable 'rev)
                                   (cons-expr (const 1) (cons-expr (const 2) (cons-expr (const 3) (null-expr)))))
                         (null-expr))))

(define (map f xs)
  (if (null? xs)
      null
      (cons (f (car xs)) (map f (cdr xs)))))

(define map-in-expr
  (letrec-expr 'map
               (lambda-expr 'f
                            (lambda-expr 'xs
                                         (if-expr (null?-expr (variable 'xs))
                                                  (null-expr)
                                                  (cons-expr (app-expr (variable 'f)
                                                                       (car-expr (variable 'xs)))
                                                             (app-expr (app-expr (variable 'map)
                                                                                 (variable 'f))
                                                                       (cdr-expr (variable 'xs)))))))
               (app-expr (app-expr (variable 'map)
                                   (lambda-expr 'x (op '+ (variable 'x) (const 10))))
                         (cons-expr (const 1) (cons-expr (const 2) (cons-expr (const 3) (null-expr)))))))

(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))))

(define append-in-expr
  (letrec-expr 'append
               (lambda-expr 'xs
                            (lambda-expr 'ys
                                         (if-expr (null?-expr (variable 'xs))
                                                  (variable 'ys)
                                                  (cons-expr (car-expr (variable 'xs))
                                                             (app-expr (app-expr (variable 'append)
                                                                                 (cdr-expr (variable 'xs)))
                                                                       (variable 'ys))))))
               (app-expr (app-expr (variable 'append)
                                   (cons-expr (const 1) (cons-expr (const 2) (null-expr))))
                         (cons-expr (const 3) (cons-expr (const 4) (null-expr))))))
                                                                       

