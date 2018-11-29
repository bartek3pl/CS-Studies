#ifndef LICZ_h
#define LICZ_h

#include <iostream>
#include "Wyrazenie.h"
using namespace std;

class Liczba: public Wyrazenie
{
public:
    double l;
    Liczba(double);

    virtual double oblicz();
    virtual string opis();
};

#endif
