with POM as (
select wybor.kod_grupy, count(distinct wybor.kod_uz) as "liczba"
from wybor join grupa using (kod_grupy)
		   join przedmiot_semestr using (kod_przed_sem)
		   join semestr using (semestr_id)
where semestr.nazwa='Semestr letni 2016/2017'
and grupa.rodzaj_zajec='w'
group by wybor.kod_grupy
)

select avg(liczba) from POM





