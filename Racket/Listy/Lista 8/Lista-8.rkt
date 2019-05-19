#lang racket

(struct variable (x) #:transparent)
(struct const (n) #:transparent)
(struct op (symb a b) #:transparent)
(struct let-expr (x e1 e2) #:transparent)
(struct if-expr (cond e1 e2) #:transparent)
(struct ++ (x) #:transparent)

(define (expr? e)
  (match e
    [(variable x) (symbol? x)]
    [(const n) (number? n)]
    [(++ x) (symbol? x)]
    [(op symb a b)
     (and (member symb '(+ - * /))
          (number? a)
          (number? b))]
    [(let-expr x e1 e2)
     (and (symbol? x)
          (expr? e1)
          (expr? e2))]
    [(if-expr cond e1 e2)
     (andmap (expr? (list cond e1 e2)))]
    [_ #f]
    ))

;wyszukiwanie wartości dla klucza na liście asocjacyjnej
;dwuelementowych list

(define (lookup x xs)
  (cond
    [(null? xs)
     (error x "unknown identifier")]
    [(eq? (caar xs) x)
     (cadar xs)]
    [else
     (lookup x (cdr xs))]
    ))

;operatory do wykorzystania w interpreterze

(define (op-to-proc x)
  (lookup x `(
              (+,+)
              (-,-)
              (*,*)
              (/,/)
              (>,>)
              (>=,>=)
              (<,<)
              (<=,<=)
              (%, modulo)
              (==, (lambda (x y) (= x y)))
              (!=, (lambda (x y) (not (= x y))))
              (&&, (lambda (x y) (and x y)))
              (||, (lambda (x y) (or x y)))
              )))

;interfejs do obsługi środowisk

(define (env-empty) null)
(define env-lookup lookup)

(define (env-add x v env)
  (cons (list x v) env))

;interpretacja wyrażeń

(define (eval e env)
  (match e
    [(const n) n]
    [(op s l r) ((op-to-proc s) (eval l env)
                                (eval r env))]
    [(++ x) (+ (eval x env) 1)]
    [(let-expr x e1 e2)
     (let ((v1 (eval e1 env)))
       (eval e2 (env-add x v1 env)))]
    [(variable x) (env-lookup x env)]
    [(if-expr b t e) (if (eval b env)
                         (eval t env)
                         (eval e env))]))

(define (run e)
  (eval e (env-empty)))

(define test
  (let-expr 'x (op '* (const 4) (const 3))
            (op '- (variable 'x) (const 2))))

;;WHILE

;definicja instrukcji w języku WHILE

(struct skip () #:transparent) ; skip
(struct comp (s1 s2) #:transparent) ; s1; s2
(struct assign (x e) #:transparent) ; x := e
(struct while (b s) #:transparent) ; while (b) s
(struct if-stm (b e1 e2) #:transparent) ; if (b) e1 else e2
(struct var-block (x e s) #:transparent) ; var x := e in s
(struct for (e1 e2 s1 s2) #:transparent) ; for (x := e1, e2, s1) s2

(define (stm? e)
  (match e
    [(skip) #t]
    [(comp s1 s2)
     (and (stm? s1)
          (stm? s2))]
    [(assign x e)
     (and (symbol? x)
          (expr? e))]
    [(while b s)
     (and (expr? b)
          (stm? s))]
    [(if-stm b e1 e2)
     (and (expr? b)
          (stm? e1)
          (stm? e2))]
    [_ #f]
    ))

;aktualizacja środowiska dla danej zmiennej (koniecznie już
;istniejącej w środowisku)

(define (update x v xs)
  (cond
    [(null? xs)
     (error x "unknown identifier")]
    [(eq? (caar xs) x)
     (cons (list x v) (cdr xs))]
    [else
     (cons (car xs) (update x v (cdr xs)))]
    ))

(define env-update update)
(define env-discard cdr)
(define (env-from-assoc-list xs) xs)

;interpretacja programów w języku WHILE, gdzie środowisko m to stan
;pamięci. Interpreter to procedura, która dostaje program i początkowy
;stan pamięci, a której wynikiem jest końcowy stan pamięci. Pamięć to
;aktualne środowisko zawierające wartości zmiennych.

(define (interp p m) ; p-procedura, m-stan pamięci
  (match p
    [(skip) m]
    [(comp s1 s2)
     (interp s2 (interp s1 m))]
    [(assign x e)
     (env-update x (eval e m) m)]
    [(while b s)
     (if (eval b m)
         (interp p (interp s m))
         m)]
    [(var-block x e s)
     (env-discard
      (interp s (env-add x (eval e m) m)))]
    [(if-stm b e1 e2)
     (if (eval b m)
         (interp e1 m)
         (interp e2 m))]
    [(++ x)
     (env-update x (+ (eval(variable x) m) 1) m)]
    [(for e1 e2 s1 s2)
     (define (iter e2 s1 s2 m)
       (if (eval e2 m)
           (iter e2 s1 s2 (interp s1 (interp s2 m)))
           m))
     (iter e2 s1 s2 (interp e1 m))]
    ))


;;Zad.1
(define (list-to-cycle xs)
  (define (helper xs fst)
    (if (null? xs)
        fst
        (mcons (car xs) (helper (cdr xs) fst))))
  (let ((start (mcons (car xs) null))) 
        (begin
          (set-mcdr! start (helper (cdr xs) start))
          start)))

(define testlist '(1 2 3 4))

;;Zad.2
(define (set-nth! n xs v)
  (if (null? xs)
      '()
      (if (> n 0)
          (mcons (mcar xs)
                 (set-nth! (- n 1) (mcdr xs) v))
          (begin (set-mcar! xs v)
                 xs))))
      
(define testmlist (mcons 1 (mcons 2 (mcons 3 (mcons 4 '())))))

;;Zad.3
;fibonacci
(define fib-in-WHILE
  (var-block 'a (const 0)
             (var-block 'b (const 1)
                        (comp (while (op '> (variable 'n) (const 0))
                                     (comp (assign 'b (op '+ (variable 'b) (variable 'a)))
                                           (comp (assign 'a (op '- (variable 'b) (variable 'a)))
                                                 (assign 'n (op '- (variable 'n) (const 1))))))
                              (assign 'n (variable 'b))))))

(define (fib n)
  (env-lookup 'n (interp fib-in-WHILE `( (n, (- n 1)) ))))

;;Zad.4
;Koniunkcja w naszym interpreterze jest gorliwa, czyli ewaluuje oba argumenty.
;Zwykły and jest leniwy np. (and #f (/ 1 0)) zwraca #f.
;Natomiast nasza koniunkcja - ((lambda (x y) (and x y)) #f (/ 1 0)) zwróci błąd
;dzielenia przez zero.

;;Zad.5
;Zmiany są w ewaluatorze oraz interpreterze języka WHILE.
;(eval (++ (variable 'x)) '( (x 5) ))

;;Zad.6
;factorial
(define fact-in-WHILE
  (var-block 'x (const 0)
             (comp (assign 'x (const 1))
                   (comp (while (op '> (variable 'i) (const 0))
                                (comp (assign 'x (op '* (variable 'x) (variable 'i)))
                                      (assign 'i (op '- (variable 'i) (const 1)))))
                         (assign 'i (variable 'x))))))

(define (fact n)
  (env-lookup 'i (interp fact-in-WHILE `( (i, n) ))))

;factorial with for loop and ++ expr
(define fact-in-WHILE-for
  (var-block 'x (const 0)
             (for (assign 'x (const 1)) (op '<= (variable 'x) (variable 'n)) (++ 'x)
               (assign 'sum (op '* (variable 'x) (variable 'sum))))))

(define (fact-for n)
  (env-lookup 'sum (interp fact-in-WHILE-for `( (n, n) (sum 1) ))))

