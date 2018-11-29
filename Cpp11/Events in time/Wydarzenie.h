#ifndef WYD_h
#define WYD_h

#include <iostream>
#include <ctime>
#include "Header.h"
#include "DataGodz.h"
using namespace std;


class Wyd
{
    public:
        string event;
        DataGodz dg;
        Wyd(DataGodz g, string);

        void wyswietl(Wyd w);

        bool operator <=(Wyd w);
};

#endif

