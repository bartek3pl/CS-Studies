#include <iostream>
#include "Header.h"
#include "DataGodz.h"
#include "Wydarzenie.h"
using namespace std;

int main()
{
    ///////////////////////// DATA

    Data d; //daty systemowe
    Data d2;

    cout << d.get_day() << "." << d.get_month() << "." << d.get_year() << endl;

    ++d;
    cout << d.get_day() << "." << d.get_month() << "." << d.get_year() << endl;

    --d;
    cout << d.get_day() << "." << d.get_month() << "." << d.get_year() << endl;

    d-=368;
    cout << d.get_day() << "." << d.get_month() << "." << d.get_year() << endl;

    d+=367;
    cout << d.get_day() << "." << d.get_month() << "." << d.get_year() << endl << endl;

    cout << "Data d1: " << d.get_day() << "." << d.get_month() << "." << d.get_year() << endl;
    cout << "Data d2: " << d2.get_day() << "." << d2.get_month() << "." << d2.get_year() << endl;
    cout << "Roznica: " << d2-d << endl << endl;

    ///////////////////////// GODZINA

    DataGodz g;
    DataGodz g2(16,59,59);

    cout << "Godzina g1: " << g.get_hour() << ":" << g.get_minute() << ":" << g.get_second() << endl;
    cout << "Godzina g2: " << g2.get_hour() << ":" << g2.get_minute() << ":" << g2.get_second() << endl;
    cout << "Roznica: " << g-g2 << endl << endl;

    ++g2;
    cout << "Godzina g2: " << g2.get_hour() << ":" << g2.get_minute() << ":" << g2.get_second() << endl;

    --g2;
    cout << "Godzina g2: " << g2.get_hour() << ":" << g2.get_minute() << ":" << g2.get_second() << endl;

    g2+=36020; //10h 00min 20s
    cout << "Godzina g2: " << g2.get_hour() << ":" << g2.get_minute() << ":" << g2.get_second() << endl << endl;

    cout << "Godzina g1: " << g.get_hour() << ":" << g.get_minute() << ":" << g.get_second() << endl;
    cout << "Godzina g2: " << g2.get_hour() << ":" << g2.get_minute() << ":" << g2.get_second() << endl << endl;

    cout << "Czy godzina g1 jest rowna godzinie g2: ";
    if(g2==g)
        cout << "Tak" << endl;
    else
        cout << "Nie" << endl;

    cout << "Czy godzina g1 jest pozniejsza niz godzina g2: ";
    if(g2<=g)
        cout << "Tak" << endl << endl;
    else
        cout << "Nie" << endl << endl;

    ///////////////////////// WYDARZENIE

    Wyd w(g,"Event"); //przykladowe wydarzenie
    w.wyswietl(w);

    return 0;
}
