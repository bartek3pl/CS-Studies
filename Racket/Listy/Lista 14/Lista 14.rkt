#lang racket

(require racklog)

(define %rodzic ; (%rodzic x y) oznacza, że iks jest rodzicem igreka
  (%rel ()
        [('elżbieta2 'karol)]
        [('elżbieta2 'anna)]
        [('elżbieta2 'andrzej)]
        [('elżbieta2 'edward)]
        [('karol     'william)]
        [('karol     'harry)]
        [('anna      'piotr)]
        [('anna      'zara)]
        [('andrzej   'beatrycze)]
        [('andrzej   'eugenia)]
        [('edward    'james)]
        [('edward    'louise)]
        [('william   'george)]
        [('william   'charlotte)]
        [('william   'louis)]
        [('harry     'archie)]
        [('piotr     'savannah)]
        [('piotr     'isla)]
        [('zara      'mia)]
        [('zara      'lena)]))

(define %rok-urodzenia
  (%rel ()
        [('elżbieta2 1926)]
        [('karol     1948)]
        [('anna      1950)]
        [('andrzej   1960)]
        [('edward    1964)]
        [('william   1982)]
        [('harry     1984)]
        [('piotr     1977)]
        [('zara      1981)]
        [('beatrycze 1988)]
        [('euagenia  1990)]
        [('james     2007)]
        [('louise    2003)]
        [('george    2013)]
        [('charlotte 2015)]
        [('louis     2018)]
        [('archie    2019)]
        [('savannah  2010)]
        [('isla      2012)]
        [('mia       2014)]
        [('lena      2018)]))

(define %plec
  (%rel ()
        [('elżbieta2 'k)]
        [('karol     'm)]
        [('anna      'k)]
        [('andrzej   'm)]
        [('edward    'm)]
        [('william   'm)]
        [('harry     'm)]
        [('piotr     'm)]
        [('zara      'k)]
        [('beatrycze 'k)]
        [('euagenia  'k)]
        [('james     'm)]
        [('louise    'k)]
        [('george    'm)]
        [('charlotte 'k)]
        [('louis     'm)]
        [('archie    'm)]
        [('savannah  'k)]
        [('isla      'k)]
        [('mia       'k)]
        [('lena      'k)]))

(define %spadl-z-konia
  (%rel ()
        [('anna)]))

;;Zad.1
;czy Elżbieta 2 jest prababcią? TAK
(%find-all (x y z) (%and (%rodzic 'elżbieta2 x)
                         (%rodzic x y)
                         (%rodzic y z)))

;czy Elżbieta 2 jest praprababcią? NIE
(%find-all (x y z q) (%and (%rodzic 'elżbieta2 x)
                           (%rodzic x y)
                           (%rodzic y z)
                           (%rodzic z q)))

;kiedy urodziły się prawnuki królowej?
(map (lambda (k) (last k))
     (%find-all (x y z q) (%and (%rodzic 'elżbieta2 x)
                                (%rodzic x y)
                                (%rodzic y z)
                                (%rok-urodzenia z q))))

;ile mają lat
(map (lambda (k) (- 2019 (cdr (last k))))
     (%find-all (x y z q) (%and (%rodzic 'elżbieta2 x)
                                (%rodzic x y)
                                (%rodzic y z)
                                (%rok-urodzenia z q))))

;jak nazywają się kuzyni księcia Archiego?
(map (lambda (k) (last k)) (%find-all (x y z q) (%and (%rodzic x 'archie)
                                                      (%rodzic y x)
                                                      (%rodzic y z)
                                                      (%rodzic z q))))

;;Zad.2 (nie wiem jak porównać wiek)
;Act of Settlement
(define %starsze-rodzenstwo
  (%rel (x y z k1 k2)
        [(x y) ;x jest starszym rodzenstwem y
         (%rodzic z x)
         (%rodzic z y)
         (%plec x 'm)
         ]
        [(x null)
         (%plec x 'k)]))

;(%find-all (x y) (%starsze-rodzenstwo x y))

;;Zad.4
(define (reverse xs)
  (if (null? xs)
      null
      (append (reverse (cdr xs)) (list (car xs)))))

(define %rev
  (%rel (x xs y zs) 
        [(null null)] 
        [((cons x xs) y) 
         (%rev xs zs) 
         (%append zs (list x) y)]))

;(%which (x) (%rev '(1 2 3 4) x))
;(%which (x) (%rev x '(1 2 3 4)))
