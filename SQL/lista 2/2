select distinct u.kod_uz, imie, nazwisko
from uzytkownik u join wybor using (kod_uz)
		   join grupa using (kod_grupy)
		   join przedmiot_semestr ps using (kod_przed_sem)
		   join przedmiot using (kod_przed)
where przedmiot.nazwa like '%Algorytmy i struktury danych%'
and exists (
	select *
	from uzytkownik join wybor using (kod_uz)
		   			join grupa using (kod_grupy)
		   			join przedmiot_semestr using (kod_przed_sem)
		   			join przedmiot using (kod_przed)
	where przedmiot.nazwa like '%Matematyka dyskretna%'
	and przedmiot_semestr.semestr_id > ps.semestr_id 
	and uzytkownik.kod_uz = u.kod_uz
)
