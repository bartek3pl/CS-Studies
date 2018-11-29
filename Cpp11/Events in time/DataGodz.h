#ifndef DATAGODZ_h
#define DATAGODZ_h

#include <iostream>
#include "Header.h"
using namespace std;

class DataGodz : public Data
{
    protected:
        int hour;
        int minute;
        int second;

     public:
        int get_hour() //gettery
        {
            return hour;
        }
        int get_minute()
        {
            return minute;
        }
        int get_second()
        {
            return second;
        }

        DataGodz(int, int, int);
        DataGodz();

        int operator -(DataGodz g);

        DataGodz operator ++();
        DataGodz operator --();
        DataGodz operator +=(int);

        bool operator <=(DataGodz g);
        bool operator ==(DataGodz g);
};

#endif

