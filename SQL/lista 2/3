select distinct grupa.kod_uz, imie, nazwisko
from grupa join uzytkownik u using (kod_uz)
where grupa.rodzaj_zajec='w'
and not exists (
	select *
	from grupa join uzytkownik using (kod_uz)
	where grupa.rodzaj_zajec='s'
	and uzytkownik.kod_uz = u.kod_uz
)

