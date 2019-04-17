select count(distinct kod_grupy)
from grupa join przedmiot_semestr using (kod_przed_sem)
           join przedmiot using (kod_przed)
           join uzytkownik using (kod_uz)
where przedmiot.nazwa like 'Logika dla informatyków'
and uzytkownik.nazwisko='Charatonik';