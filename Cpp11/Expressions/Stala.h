#ifndef STA_h
#define STA_h

#include <iostream>
#include "Wyrazenie.h"
using namespace std;

class pi: public Wyrazenie
{
public:
    double s;
    pi();

    virtual double oblicz();
    virtual string opis();
};

class e: public Wyrazenie
{
public:
    double s;
    e();

    virtual double oblicz();
    virtual string opis();
};

class fi: public Wyrazenie
{
public:
    double s;
    fi();

    virtual double oblicz();
    virtual string opis();
};
#endif // STA_h
