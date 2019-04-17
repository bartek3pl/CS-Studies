(select distinct grupa.kod_uz, imie, nazwisko
from grupa join uzytkownik using (kod_uz)
where grupa.rodzaj_zajec='w')
except
(select grupa.kod_uz, imie, nazwisko
from grupa join uzytkownik using (kod_uz)
where grupa.rodzaj_zajec='s')

