#include <iostream>
#include <cmath>
#include "Liczba.h"
using namespace std;

//konstruktor
Liczba::Liczba(double val)
{
    l = val;
}

double Liczba::oblicz()
{
     return l;
}

string Liczba::opis()
{
    return to_string(l).substr(0,4);
}





