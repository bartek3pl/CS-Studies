# Baza zapisów
## Tytułem wskazówki
#### w tabeli użytkownik semestr ma znaczenie tylko dla studentów;
#### w tabeli przedmiot są przedmioty, a w tabeli przedmiot_semestr są edycje przedmiotu w poszczególnych semestrach;
#### w tabeli przedmiot rodzaj określa, czy jest to przedmiot obowiązkowy, zaawansowany, nieinformatyczny itp.
#### w tabeli grupa są grupy założone do przedmiotu w semestrze: kod_uz w grupie to kod prowadzącego, a rodzaj_zajec to ćwiczenia, wykład itp. przy grupie podany jest także termin (jest tam "zakodowany" dzień tygodnia i godzina) i max_osoby - dopuszczalna liczba studentów w grupie.
#### w tabeli wybor znajduje się informacja, że student zapisał się do grupy; podany jest tam czas zapisu.

### Poniżej znajdują się wyjaśnienia wartości słownikowych dla poszczególnych relacji i ich atrybutów.

## Relacja przedmiot:
### rodzaj:
#### o -- obowiązkowy
#### p -- podstawowy
#### z -- zaawansowany
#### k -- kurs
#### s -- seminarium
#### n -- nieinformatyczny

## Relacja grupa:
### rodzaj_zajec:
#### w -- wykład
#### e -- wykład-repetytorium
#### s -- seminarium
#### c -- ćwiczenia
#### C -- ćwiczenia zaawansowane
#### p -- pracownia
#### P -- pracownia zaawansowana
#### r -- ćwiczenia+pracownia
#### R -- ćwiczenia+pracownia (zaawansowane)
#### g -- grupa oczekujących na zapis
#### l -- lektorat

## Relacja plan:
#### atrybut termin ma format dt:g1:m1:g2:m2, gdzie:
#### dt -- dzień tygodnia 1=poniedziałek, 2=wtorek...
#### g1:m1 -- od
#### g2:m2 -- do

## Relacja uzytkownik:
#### atrybut semestr mówi trochę o użytkowniku:
#### 0 - pracownik
#### 1..10 - student