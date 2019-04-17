select przedmiot.nazwa, count(distinct wybor.kod_uz)
from przedmiot join przedmiot_semestr using (kod_przed)
		   	   join grupa using (kod_przed_sem)
			   join wybor using (kod_grupy)
where przedmiot.rodzaj='p'
and grupa.rodzaj_zajec='w'
group by przedmiot.nazwa
having count(distinct wybor.kod_uz) >= all (
	select count(distinct wybor.kod_uz)
	from przedmiot join przedmiot_semestr using (kod_przed)
			   	   join grupa using (kod_przed_sem)
				   join wybor using (kod_grupy)
	where przedmiot.rodzaj='p'
	and grupa.rodzaj_zajec='w'
	group by przedmiot.nazwa	
)



