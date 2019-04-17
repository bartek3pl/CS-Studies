select kod_grupy
from grupa join przedmiot_semestr using (kod_przed_sem)
            join przedmiot using (kod_przed)
            join semestr using (semestr_id)            
where grupa.rodzaj_zajec='w'
and przedmiot.nazwa='Matematyka dyskretna (M)'
and semestr.nazwa='Semestr zimowy 2017/2018';

select w1.data-w2.data
from wybor w1 join grupa using (kod_grupy)
			join wybor w2 using (kod_grupy)
where w1.data > w2.data
and grupa.kod_grupy='5649';
	
