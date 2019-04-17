select count(distinct kod_uz)
from uzytkownik join grupa using (kod_uz)
				join przedmiot_semestr using (kod_przed_sem)
                join przedmiot using (kod_przed)
                join semestr using (semestr_id)               
where przedmiot.rodzaj='o'
and grupa.rodzaj_zajec in ('c', 'C')
and semestr.nazwa like '%Semestr zimowy%'


	
