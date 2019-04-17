# Lista SQL2

## Dla każdego z poniższych zapytań napisz JEDNO zapytanie SQL, które zwraca informację będącą odpowiedzią na pytanie.

### 1. Podaj kody, imiona i nazwiska wszystkich osób, które chodziły na dowolne zajęcia z Algorytmów i struktur danych, a w jakimś semestrze późniejszym (o większym numerze) chodziły na zajęcia z Matematyki dyskretnej. Za AiSD oraz MD uznaj wszystkie przedmioty, których nazwa zaczyna się od podanych nazw. Zapisz to zapytanie używając operatora IN z podzapytaniem.
### 2. Zapisz zapytanie pierwsze używając operatora EXISTS z podzapytaniem.
### 3. Podaj kody, imiona i nazwiska osób, które prowadziły jakiś wykład, ale nigdy nie prowadziły żadnego seminarium (nie patrzymy, czy zajęcia były w tym samym semestrze). Pisząc zapytanie użyj operatora NOT EXISTS.
### 4. Zapisz zapytanie trzecie, używając różnicy zbiorów.
### 5. Dla każdego przedmiotu typu kurs z bazy danych podaj jego nazwę oraz liczbę osób, które na niego uczęszczały. Uwzględnij w odpowiedzi kursy, na które nikt nie uczęszczał – w tym celu użyj złączenia zewnętrznego (LEFT JOIN lub RIGHT JOIN).
### 6. Podaj kody użytkowników, którzy uczęszczali w semestrze letnim 2016/2017 na wykład z 'Baz danych' i nie uczęszczali na wykład z 'Sieci komputerowych', i odwrotnie. Sformułuj to zapytanie używając instrukcji WITH, by wstępnie zdefiniować zbiory osób uczęszczających na każdy z wykładów.
### 7. Podaj kody, imiona i nazwiska wszystkich prowadzących, którzy w jakiejś prowadzonej przez siebie grupie mieli więcej zapisanych osób, niż wynosił limit max_osoby dla tej grupy. Do zapisania zapytania użyj GROUP BY i HAVING.
### 8. Podaj nazwę przedmiotu podstawowego, na wykład do którego chodziło najwięcej różnych osób. Użyj w tym celu zapytania z GROUP BY i HAVING (z warunkiem używającym ponownie GROUP BY).
### 9. Dla każdego semestru letniego podaj jego numer oraz nazwisko osoby, która jako pierwsza zapisała się na zajęcia w tym semestrze. Jeśli w semestrze było kilka osób, które zapisały się jednocześnie: a) podaj wszystkie; b) podaj tę o najwcześniejszym leksykograficznie nazwisku.
### 10. Jaka jest średnia liczba osób zapisujących się na wykład w semestrze letnim 2016/2017? Zapisz to zapytanie definiując najpierw pomocniczą relację (np. na liście from z aliasem), w której dla każdego interesującego cię wykładu znajdziesz liczbę zapisanych na niego osób).
### 11. Kto prowadzi w jednym semestrze wykład do przedmiotu i co najmniej dwie grupy innych zajęć do tego przedmiotu (nie muszą być tego samego typu)?