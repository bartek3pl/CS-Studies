#include <iostream>
#include "Stala.h"
using namespace std;

//konstruktory
pi::pi()
{
    s = 3.1415926535;
}

e::e()
{
    s = 2.7182818284;
}

fi::fi()
{
    s = 1.6180339887;
}
////////////////////

double pi::oblicz()
{
     return s;
}

string pi::opis()
{
    return "pi";
}
//////////////////
double e::oblicz()
{
     return s;
}

string e::opis()
{
    return "e";
}
//////////////////
double fi::oblicz()
{
     return s;
}

string fi::opis()
{
    return "fi";
}
