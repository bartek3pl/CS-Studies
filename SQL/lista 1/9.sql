select *
from semestr join przedmiot_semestr using (semestr_id)
             join przedmiot using (kod_przed)
where przedmiot.rodzaj='o'