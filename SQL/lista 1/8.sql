select *
from uzytkownik u join wybor w1 using (kod_uz)
				  join grupa g1 using (kod_grupy)
		   		  join przedmiot_semestr ps1 using (kod_przed_sem)
           		  join przedmiot p1 using (kod_przed)
                  
                  join wybor w2 on (u.kod_uz=w2.kod_uz)
				  join grupa g2 using (g2.kod_uz=w2.kod_grupy)
		   		  join przedmiot_semestr ps2 using (kod_przed_sem=w2.)
           		  join przedmiot p2 using (kod_przed)
                               
where przedmiot.nazwa='Algorytmy i struktury danych (M)'
and 




	
