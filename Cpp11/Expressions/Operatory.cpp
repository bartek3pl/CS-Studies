#include <iostream>
#include <cmath>
#include "Operatory.h"
using namespace std;

Dodaj::Dodaj(Wyrazenie *d1, Wyrazenie *d2)
{
    w1 = d1;
    w2 = d2;
}

Odejmij::Odejmij(Wyrazenie *d1, Wyrazenie *d2)
{
    w1 = d1;
    w2 = d2;
}

Pomnoz::Pomnoz(Wyrazenie *d1, Wyrazenie *d2)
{
    w1 = d1;
    w2 = d2;
}

Podziel::Podziel(Wyrazenie *d1, Wyrazenie *d2)
{
    w1 = d1;
    w2 = d2;
}

Modulo::Modulo(Wyrazenie *d1, Wyrazenie *d2)
{
    w1 = d1;
    w2 = d2;
}

Potega::Potega(Wyrazenie *d1, Wyrazenie *d2)
{
    w1 = d1;
    w2 = d2;
}

Sinus::Sinus(Wyrazenie *d1)
{
    w1 = d1;
}

Cosinus::Cosinus(Wyrazenie *d1)
{
    w1 = d1;
}

Reverse::Reverse(Wyrazenie *d1)
{
    w1 = d1;
}


///////////////////////////////////

double Dodaj::oblicz()
{
    return w1->oblicz() + w2->oblicz();
}

string Dodaj::opis()
{
    return "(" + w1->opis() + " + " + w2->opis() + ")";
}

double Odejmij::oblicz()
{
    return w1->oblicz() - w2->oblicz();
}

string Odejmij::opis()
{
    return "(" + w1->opis() + " - " + w2->opis() + ")";
}

double Pomnoz::oblicz()
{
    return w1->oblicz() * w2->oblicz();
}

string Pomnoz::opis()
{
    return "(" + w1->opis() + " * " + w2->opis() + ")";
}

double Podziel::oblicz()
{
    return w1->oblicz() / w2->oblicz();
}

string Podziel::opis()
{
    return "(" + w1->opis() + " / " + w2->opis() + ")";
}

double Modulo::oblicz()
{
    return double(int(w1->oblicz()) % int(w2->oblicz()));
}

string Modulo::opis()
{
    return "(" + w1->opis() + " % " + w2->opis() + ")";
}

double Potega::oblicz()
{
    return pow(w1->oblicz(), w2->oblicz());
}

string Potega::opis()
{
    return "(" + w1->opis() + " ** " + w2->opis() + ")";
}

//1 argumentowe

double Sinus::oblicz()
{
    return sin(w1->oblicz());
}

string Sinus::opis()
{
    return "sin(" + w1->opis() + ")";
}

double Cosinus::oblicz()
{
    return cos(w1->oblicz());
}

string Cosinus::opis()
{
    return "cos(" + w1->opis() + ")";
}

double Reverse::oblicz()
{
    return (w1->oblicz() * (-1));
}

string Reverse::opis()
{
    return "-(" + w1->opis() + ")";
}
