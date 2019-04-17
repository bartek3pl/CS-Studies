select count(kod_grupy)
from grupa join przedmiot_semestr using (kod_przed_sem)
           join przedmiot using (kod_przed)
           join semestr using (semestr_id)
where grupa.rodzaj_zajec in ('c','C')
and przedmiot.nazwa='Logika dla informatyków'
and semestr.nazwa='Semestr zimowy 2017/2018'






