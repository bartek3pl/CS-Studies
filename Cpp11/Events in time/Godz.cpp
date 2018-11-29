#include <iostream>
#include <ctime>
#include <cmath>
#include <windows.h>
#include "DataGodz.h"
#include "Header.h"
using namespace std;

DataGodz::DataGodz(int h, int m, int s) : hour(h), minute(m), second(s)
{
    Data d;

    day = 30;
    month = 12;
    year = 1998;

    h = 20;
    m = 56;
    s = 30;
}

DataGodz::DataGodz()
{
    SYSTEMTIME st;
    GetLocalTime(&st);

    hour = st.wHour;
    minute = st.wMinute;
    second = st.wSecond;
}

int DataGodz::operator -(DataGodz g1)
{
    DataGodz g2;
    int ile_g1, ile_g2;

    ile_g1 = g1.hour * 3600 + g1.minute * 60 + g1.second;
    ile_g2 = g2.hour * 3600 + g2.minute * 60 + g2.second;

    return abs(ile_g1-ile_g2);
}

DataGodz DataGodz::operator ++()
{
     DataGodz g = *this;

    if(g.second+1 >= 0 && g.second+1 < 59)
    {
        *this = DataGodz(g.hour,g.minute,g.second+1);
        return *this;
    }
    else if (g.second+1 >= 59 && g.minute < 59)
    {
        *this = DataGodz(g.hour,g.minute+1,00);
        return *this;
    }
    else if (g.second+1 >= 59 && g.minute >= 59)
    {
        *this = DataGodz(g.hour+1,00,00);
        return *this;
    }
}

DataGodz DataGodz::operator --()
{
     DataGodz g = *this;

    if(g.second-1 > 0 && g.minute > 0)
    {
        *this = DataGodz(g.hour,g.minute,g.second-1);
        return *this;
    }
    else if (g.second-1 == 0 && g.minute > 0)
    {
        *this = DataGodz(g.hour,g.minute-1,59);
        return *this;
    }
    else if (g.minute == 0 && g.hour > 0)
    {
        *this = DataGodz(g.hour-1,59,59);
        return *this;
    }
}

DataGodz DataGodz::operator +=(int x)
{
    DataGodz d = *this;

    if(d.second+x > 0 && d.second+x < 60)
    {
        *this = DataGodz(d.hour,d.minute,d.second+x);
        return *this;
    }
    else
    {
        int j = 0;
        int p = 0;

        for (int i = 0; i < x; i += 60)
        {
              if (x + d.second >= 3600)
            {
                x -= 3600;
                ++p;
            }
            else if (x + d.second >= 60)
            {
                x -= 60;
                ++j;
            }
            if (d.hour + p == 23 && x + d.minute >= 60)
            {
                d.hour = 0;
                x -= 3600;
                p = 1;
            }
            if (d.minute + j == 59 && x + d.second >= 60)
            {
                d.minute = 0;
                x -= 60;
                j = 1;
            }
        }

        *this = DataGodz(d.hour+p,d.minute+j,d.second+x);
        return *this;
    }
}

bool DataGodz::operator <=(DataGodz g1)
{
    DataGodz g2 = *this;

    if(g2.hour < g1.hour)
        return true;
    else if(g2.hour == g1.hour && g2.minute < g1.minute)
        return true;
    else if (g2.minute == g1.minute && g2.second < g1.second)
        return true;

    return false;
}

bool DataGodz::operator ==(DataGodz g1)
{
    DataGodz g2 = *this;

    return(g2.hour == g1.hour && g2.minute == g1.minute && g2.second == g1.second);
}



