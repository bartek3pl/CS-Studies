select count(semestr_id)
from semestr join przedmiot_semestr using (semestr_id)
             join przedmiot using (kod_przed)          
where przedmiot.rodzaj='o'
and semestr_id='32';









