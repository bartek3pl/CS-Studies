#ifndef HEADER_h
#define HEADER_h

#include <iostream>
#include <ctime>
using namespace std;


class Data
{
    protected:
        int day;
        int month;
        int year;

    public:
        int get_day() //gettery
        {
            return day;
        }
        int get_month()
        {
            return month;
        }
        int get_year()
        {
            return year;
        }

    protected:
        static bool if_leap_year(); //czy rok przestêpny
        int days_counter(int, int, int);

    public:
        static bool if_data_correct(int, int, int); //wykorzysta metode "if_leap_year"
        static int dniodpoczroku[2][13];
        static int dniwmiesiacach[2][13];

        Data(); //konstruktor bezargumentowy - pobierze date systemowa
        Data(int, int, int); //do ustalenia konkretnego dnia

        Data operator ++();
        Data operator --();
        Data operator +=(int);
        Data operator -=(int);

        int operator -(Data d);
};

#endif


