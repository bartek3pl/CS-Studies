select count(w1.kod_uz)
from grupa g1, grupa g2,
     wybor w1, wybor w2,
     przedmiot_semestr ps1, przedmiot_semestr ps2
where     ps1.kod_przed=12
      and ps2.kod_przed=12
      and g1.rodzaj_zajec='w'
      and g2.rodzaj_zajec='w'
      and ps1.kod_przed_sem=g1.kod_przed_sem
      and ps2.kod_przed_sem=g2.kod_przed_sem
      and g1.kod_grupy=w1.kod_grupy
      and g2.kod_grupy=w2.kod_grupy
      and w1.kod_uz=w2.kod_uz
      and g1.kod_przed_sem>g2.kod_przed_sem