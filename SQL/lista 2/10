select semestr.semestr_id, uzytkownik.nazwisko, wybor.data
from wybor join uzytkownik using (kod_uz)
		   join grupa using (kod_grupy)
		   join przedmiot_semestr using (kod_przed_sem)
		   join semestr using (semestr_id)
where semestr.nazwa like 'Semestr letni%'
group by semestr.semestr_id, uzytkownik.nazwisko, wybor.data
having (wybor.data) <= all (
	select wybor.data
	from wybor join uzytkownik using (kod_uz)
			   join grupa using (kod_grupy)
			   join przedmiot_semestr using (kod_przed_sem)
		   	   join semestr using (semestr_id)
	where semestr.nazwa like 'Semestr letni%'	
)
order by uzytkownik.nazwisko asc



