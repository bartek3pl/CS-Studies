#include <iostream>
#include "Wyrazenie.h"
#include "Stala.h"
#include "Liczba.h"
#include "Zmienna.h"
#include "Operatory.h"
using namespace std;

int main()
{
    vector<pair<string,double>> zbior;

    zbior.push_back(make_pair("x", 2));
    zbior.push_back(make_pair("y", 5));

    Wyrazenie *w = new Zmienna(zbior, 1);
    cout << w->opis() << " = " << w->oblicz() << endl;
    w = new Zmienna(zbior, 0);
    cout << w->opis() << " = " << w->oblicz() << endl << endl;

    /////////////////////////////////////////////////////

    w = new Podziel(
                    new Pomnoz(
                               new Odejmij(
                                           new Zmienna(zbior,0), new Liczba(1)),
                               new Zmienna(zbior, 0)),
                     new Liczba(2));

    cout << w->opis() << " = " << w->oblicz() << endl;


    w = new Podziel(new Dodaj(new Liczba(3), new Liczba(5)),
                    new Dodaj(new Liczba(2), new Pomnoz(new Zmienna(zbior, 0), new Liczba(7))));

    cout << w->opis() << " = " << w->oblicz() << endl;


    w = new Odejmij(new Dodaj(new Liczba(2), new Pomnoz(new Zmienna(zbior, 0), new Liczba(7))),
                    new Dodaj(new Pomnoz(new Zmienna(zbior, 1), new Liczba(3)), new Liczba(5)));

    cout << w->opis() << " = " << w->oblicz() << endl;


    w = new Podziel(
                    new Cosinus(
                                new Pomnoz(
                                           new Dodaj(new Zmienna(zbior, 0), new Liczba(1)), new Zmienna(zbior, 0))),
                     new Potega(new e(), new Potega(new Zmienna(zbior, 0), new Liczba(2))));

    cout << w->opis() << " = " << w->oblicz() << endl << endl;

    //////////////////////////////

    double j = 0.01;
    for (int i = 2; i < 100; ++i)
    {
        zbior.push_back(make_pair("z", j));

        w = new Podziel(new Dodaj(new Liczba(3), new Liczba(5)),
                        new Dodaj(new Liczba(2), new Pomnoz(new Zmienna(zbior, i), new Liczba(7))));

        cout << w->oblicz() << endl;
        j+= 0.01;
    }


    return 0;
}
