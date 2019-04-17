select przedmiot.nazwa, count(distinct wybor.kod_uz)
from przedmiot left join przedmiot_semestr using (kod_przed)
			   left join grupa using (kod_przed_sem)
			   left join wybor using (kod_grupy)
where przedmiot.nazwa like '%baz%'
and przedmiot.rodzaj='k'
group by przedmiot.kod_przed, przedmiot.nazwa

