
((select wybor.kod_uz
from wybor join grupa using (kod_grupy)
		   join przedmiot_semestr using (kod_przed_sem)
		   join przedmiot using (kod_przed)
		   join semestr using (semestr_id)
where semestr.nazwa='Semestr letni 2016/2017'
and grupa.rodzaj_zajec='w'
and przedmiot.nazwa='Bazy danych')
except
(select wybor.kod_uz
from wybor join grupa using (kod_grupy)
		   join przedmiot_semestr using (kod_przed_sem)
		   join przedmiot using (kod_przed)
		   join semestr using (semestr_id)
where semestr.nazwa='Semestr letni 2016/2017'
and grupa.rodzaj_zajec='w'
and przedmiot.nazwa='Sieci komputerowe'))

union

((select wybor.kod_uz
from wybor join grupa using (kod_grupy)
		   join przedmiot_semestr using (kod_przed_sem)
		   join przedmiot using (kod_przed)
		   join semestr using (semestr_id)
where semestr.nazwa='Semestr letni 2016/2017'
and grupa.rodzaj_zajec='w'
and przedmiot.nazwa='Sieci komputerowe')
except
(select wybor.kod_uz
from wybor join grupa using (kod_grupy)
		   join przedmiot_semestr using (kod_przed_sem)
		   join przedmiot using (kod_przed)
		   join semestr using (semestr_id)
where semestr.nazwa='Semestr letni 2016/2017'
and grupa.rodzaj_zajec='w'
and przedmiot.nazwa='Bazy danych'))

