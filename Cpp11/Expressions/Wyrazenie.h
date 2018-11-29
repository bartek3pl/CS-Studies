#ifndef WYR_h
#define WYR_h

#include <iostream>
using namespace std;

class Wyrazenie
{
public:
    virtual double oblicz()=0;
    virtual string opis()=0;
};

#endif // WYR_h
