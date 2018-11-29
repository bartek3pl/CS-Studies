#include <iostream>
#include <ctime>
#include <cmath>
#include <windows.h>
#include "Header.h"

using namespace std;


//pobiera systemowa date
Data::Data()
{
    SYSTEMTIME st;
    GetLocalTime(&st);

    day = st.wDay;
    month = st.wMonth;
    year = st.wYear;
}

//wywolanie konkretnej daty
Data::Data(int d, int m, int y) : day(d), month(m), year(y)
{

}

// czy rok przestepny
bool Data::if_leap_year()
{
    Data d;
    return (d.get_year() % 400 == 0 || (d.get_year() % 4 == 0 && d.get_year() % 100 != 0));
}

//czy prawidlowa data - korzysta z "if_leap_year"
bool Data::if_data_correct(int d, int m, int y)
{
    if(d < 1 || d > 31 || m < 1 || m > 12 || y < 0)
        return false;

    if (!if_leap_year() && m == 2 && d > 29)
        return false;
    else if (m == 2 && d > 28)
        return false;

    return true;
}

int Data::dniodpoczroku[2][13] =
{
    {0,31,59,90,120,151,181,212,243,273,304,334,365}, // lata zwykłe
    {0,31,60,91,121,152,182,213,244,274,305,335,366} // lata przestępne
};

//ile dni minelo od pewnej daty (od 1.1.0) do daty w argumentach
int Data::days_counter(int d, int m, int y)
{
    if(!if_data_correct(d, m, y))
    {
        cerr << "Invalid data" << endl;
        return 0;
    }
    int ile = (y * dniodpoczroku[if_leap_year()][12]) + dniodpoczroku[if_leap_year()][m] + d;

    return ile;
}

///////////////////////////operatory

//inkrementuje dzien
Data Data::operator ++()
{
    Data d = *this;

    if(d.if_data_correct(d.day+1,d.month,d.year))
    {
        *this = Data(d.day+1,d.month,d.year);
        return *this;
    }

    if(d.if_data_correct(1,d.month+1,d.year))
    {
        *this = Data(1,d.month+1,d.year);
        return *this;
    }

    if(d.if_data_correct(1,1,d.year+1))
     {
        *this = Data(1,1,d.year+1);
        return *this;
    }
}

//dekrementuje dzien
Data Data::operator --()
{
    Data d = *this;

     if(d.if_data_correct(d.day-1,d.month,d.year))
     {
         *this = Data(d.day-1,d.month,d.year);
         return *this;
     }

    if(d.if_data_correct(31,d.month-1,d.year))
    {
        *this = Data(31,d.month+1,d.year);
        return *this;
    }

    if(d.if_data_correct(30,d.month-1,d.year))
    {
        *this = Data(30,d.month+1,d.year);
        return *this;
    }

    if(d.if_data_correct(29,d.month-1,d.year))
    {
        *this = Data(29,d.month+1,d.year);
        return *this;
    }

    if(d.if_data_correct(28,d.month-1,d.year))
    {
        *this = Data(28,d.month+1,d.year);
        return *this;
    }

    if(d.if_data_correct(1,1,d.year-1))
    {
        *this = Data(31,12,d.year+1);
        return *this;
    }
}

int Data::dniwmiesiacach[2][13] =
{
    {0,31,28,31,30,31,30,31,31,30,31,30,31}, // lata zwykłe
    {0,31,29,31,30,31,30,31,31,30,31,30,31} // lata przestępne
};

Data Data::operator +=(int x)
{
    Data d = *this;

    if(d.if_data_correct(d.day+x,d.month,d.year))
    {
        *this = Data(d.day+x,d.month,d.year);
        return *this;
    }
    else
    {
        int j = 0;
        int p = 0;

        for (int i = 0; i <= x; i += dniwmiesiacach[d.if_leap_year()][d.month])
        {
            if(x + d.month > dniodpoczroku[d.if_leap_year()][12])
           {
                x -= dniodpoczroku[d.if_leap_year()][12];
                ++p;
           }
           else if(x + d.day > dniwmiesiacach[d.if_leap_year()][d.month])
           {
                x -= dniwmiesiacach[d.if_leap_year()][d.month];
                ++j;
           }
        }
        *this = Data(x+d.day,d.month+j,d.year+p);
        return *this;
    }
}

Data Data::operator -=(int x)
{
   Data d = *this;

    if(d.if_data_correct(d.day-x,d.month,d.year))
    {
        *this = Data(d.day-x,d.month,d.year);
        return *this;
    }
    else
    {
        int j = 0;
        int p = 0;
        x = d.day - x;
        for (int i = 0; i > x; i -= dniwmiesiacach[d.if_leap_year()][d.month])
        {
            if(x < dniodpoczroku[d.if_leap_year()][12])
           {
                x += dniodpoczroku[d.if_leap_year()][12];
                ++p;
           }
           else if(x < dniwmiesiacach[d.if_leap_year()][d.month])
           {
                x += dniwmiesiacach[d.if_leap_year()][d.month];
                ++j;
           }
        }
        *this = Data(x,d.month-j,d.year-p);
        return *this;
    }
}

//zwraca roznice w sekundach
int Data::operator -(Data d1)
{
    Data d2;
    int ile_d1, ile_d2;

    ile_d1 = (d1.year * dniodpoczroku[d1.if_leap_year()][12]) + dniodpoczroku[d1.if_leap_year()][d1.month] + d1.day;
    ile_d2 = (d2.year * dniodpoczroku[d2.if_leap_year()][12]) + dniodpoczroku[d2.if_leap_year()][d2.month] + d2.day;

    return abs(ile_d1-ile_d2);
}








