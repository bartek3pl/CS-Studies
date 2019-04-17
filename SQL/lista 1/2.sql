select imie, nazwisko, data
from uzytkownik join wybor using (kod_uz)
				join grupa using (kod_grupy)
				join przedmiot_semestr using (kod_przed_sem)
				join przedmiot using (kod_przed)
				join semestr using (semestr_id)
			
where przedmiot.nazwa='Matematyka dyskretna (M)'
and semestr.nazwa='Semestr zimowy 2017/2018'
and grupa.rodzaj_zajec='w'
order by data desc limit 1
				

					
								 