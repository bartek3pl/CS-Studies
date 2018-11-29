#ifndef ZM_h
#define ZM_h

#include <iostream>
#include <vector>
#include "Wyrazenie.h"
using namespace std;

class Zmienna: public Wyrazenie
{
public:
    int i;

private:
    vector<pair<string,double>> zbior;

public:
    vector<pair<string,double>> get_zbior() const
    {
        return zbior;
    }

    Zmienna(vector<pair<string,double>>, int);

    virtual double oblicz();
    virtual string opis();
};
#endif // ZM_h
