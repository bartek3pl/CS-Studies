#include <iostream>
#include "Wyrazenie.h"
using namespace std;

class Dodaj: public Wyrazenie
{
    Wyrazenie *w1, *w2;

public:
    Dodaj(Wyrazenie*, Wyrazenie*);

    virtual double oblicz();
    virtual string opis();
};

class Odejmij: public Wyrazenie
{
    Wyrazenie *w1, *w2;

public:
    Odejmij(Wyrazenie*, Wyrazenie*);

    virtual double oblicz();
    virtual string opis();
};

class Pomnoz: public Wyrazenie
{
    Wyrazenie *w1, *w2;

public:
    Pomnoz(Wyrazenie*, Wyrazenie*);

    virtual double oblicz();
    virtual string opis();
};

class Podziel: public Wyrazenie
{
    Wyrazenie *w1, *w2;

public:
    Podziel(Wyrazenie*, Wyrazenie*);

    virtual double oblicz();
    virtual string opis();
};

class Modulo: public Wyrazenie
{
    Wyrazenie *w1, *w2;

public:
    Modulo(Wyrazenie*, Wyrazenie*);

    virtual double oblicz();
    virtual string opis();
};

class Potega: public Wyrazenie
{
    Wyrazenie *w1, *w2;

public:
    Potega(Wyrazenie*, Wyrazenie*);

    virtual double oblicz();
    virtual string opis();
};

//1 argumentowe

class Sinus: public Wyrazenie
{
    Wyrazenie *w1;

public:
    Sinus(Wyrazenie*);

    virtual double oblicz();
    virtual string opis();
};

class Cosinus: public Wyrazenie
{
    Wyrazenie *w1;

public:
    Cosinus(Wyrazenie*);

    virtual double oblicz();
    virtual string opis();
};

class Reverse: public Wyrazenie
{
    Wyrazenie *w1;

public:
    Reverse(Wyrazenie*);

    virtual double oblicz();
    virtual string opis();
};
