select nazwisko
from uzytkownik join grupa using (kod_uz) 
				join przedmiot_semestr using (kod_przed_sem)
				join przedmiot using (kod_przed)
				join semestr using (semestr_id)
where przedmiot.nazwa='Matematyka dyskretna (M)' 
and semestr.nazwa='Semestr zimowy 2017/2018'
and grupa.rodzaj_zajec='c'
order by nazwisko asc
					
								 