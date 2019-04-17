select distinct uzytkownik.kod_uz, imie, nazwisko
from uzytkownik join wybor using (kod_uz)
		   join grupa using (kod_grupy)
		   join przedmiot_semestr ps using (kod_przed_sem)
		   join przedmiot using (kod_przed)
where przedmiot.nazwa like '%Algorytmy i struktury danych%'
and uzytkownik.kod_uz in 
(select wybor.kod_uz
 from wybor join grupa using (kod_grupy)
		  	join przedmiot_semestr using (kod_przed_sem)
		    join przedmiot using (kod_przed)
 where przedmiot.nazwa like '%Matematyka dyskretna%'
 and ps.semestr_id < semestr_id)
