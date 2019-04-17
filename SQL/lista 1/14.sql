select kod_przed, nazwa
from przedmiot        
where przedmiot.rodzaj='k'
and kod_przed not in (select kod_przed
                      from przedmiot_semestr)











