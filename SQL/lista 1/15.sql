select count(distinct kod_grupy)
from grupa join uzytkownik using (kod_uz)
where grupa.rodzaj_zajec in ('r', 'R')
and uzytkownik.nazwisko like '%Kanarek';