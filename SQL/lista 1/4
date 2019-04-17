select count(distinct przedmiot.kod_przed)
from przedmiot join przedmiot_semestr using (kod_przed)
				join grupa using (kod_przed_sem)                   
where przedmiot.rodzaj='o'
and grupa.rodzaj_zajec='e'

	
