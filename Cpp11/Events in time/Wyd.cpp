#include <iostream>
#include <ctime>
#include "Wydarzenie.h"
#include "Header.h"
#include "DataGodz.h"
using namespace std;

Wyd::Wyd(DataGodz d, string e): dg(d), event(e)
{

}

bool Wyd::operator <=(Wyd w1)
{
    Wyd w2 = *this;

    if(w2.dg.get_year() < w1.dg.get_year())
        return true;
    else if(w2.dg.get_year() == w1.dg.get_year() && w2.dg.get_year() < w1.dg.get_year())
        return true;
    else if (w2.dg.get_year() == w1.dg.get_year() && w2.dg.get_year() < w1.dg.get_year())
        return true;

    return false;
}

void Wyd::wyswietl(Wyd d)
{
    cout << d.dg.get_day() << "." << d.dg.get_month() << "." << d.dg.get_year() << " "
    << d.dg.get_hour() << ":" << d.dg.get_minute() << ":" << d.dg.get_second() << " " << event << endl;
}




