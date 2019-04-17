select distinct grupa.kod_uz, uzytkownik.imie, uzytkownik.nazwisko
from grupa join uzytkownik using (kod_uz)
		   join wybor using (kod_grupy)
group by grupa.kod_grupy, uzytkownik.imie, uzytkownik.nazwisko, grupa.max_osoby
having count(*) > grupa.max_osoby


