#include <iostream>
#include <vector>
#include "Zmienna.h"
using namespace std;

Zmienna::Zmienna(vector<pair<string,double>> zb, int ii)
{
    zbior = zb;
    i = ii;
}

double Zmienna::oblicz()
{
    return zbior[i].second;
}

string Zmienna::opis()
{
    return zbior[i].first;
}

