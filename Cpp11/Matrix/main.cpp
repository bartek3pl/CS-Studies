#include <iostream>
#include "Matrix.h"
using namespace std;
using namespace calculations;

int main()
{
    int x = 5;
    int y = 3;

    cout << "Matrix M1: " << endl;
    Matrix *m1 = new Matrix(x);
    cout << *m1 << endl;

    cout << "Matrix M2: " << endl;
    Matrix *m2 = new Matrix(x);
    cout << *m2 << endl;

    cout << "Matrix M3: " << endl;
    Matrix *m3 = new Matrix(x,y);
    cout << *m3 << endl;

    cout << "Matrix M4: " << endl;
    Matrix *m4 = new Matrix(x,y);
    cout << *m4 << endl;

    //ARITHMETIC OPERATIONS

    cout << "Matrix M1 * 2: " << endl;
    cout << *m1 * 2 << endl;

    cout << "Matrix M2 * 0: " << endl;
    cout << *m2 * 0 << endl;

    cout << "Matrix M3 * 2: " << endl;
    cout << *m3 * 2 << endl;

    cout << "Matrix M3+=M4 (RECTANGULAR): " << endl;
    *m3 += *m4;
    cout << *m3 << endl;

    cout << "Matrix M1+M2 (SQUARE): " << endl;
    cout << *m1 + *m2 << endl;

    cout << "Matrix M3+M4 (RECTANGULAR): " << endl;
    cout << *m3 + *m4 << endl;

    cout << "Matrix M3-M4 (RECTANGULAR): " << endl;
    cout << *m3 - *m4 << endl;

    cout << "Matrix M1-M2 (SQUARE): " << endl;
    cout << *m1 - *m2 << endl;

    cout << "Matrix M1*M2 (SQUARE): " << endl;
    cout << *m1 * *m2 << endl;

    //ELEMENTARY OPERATIONS

    cout << "Switching 0, 1 rows: " << endl;
    m1->switching_two_rows(0, 1);
    cout << *m1 << endl;

    cout << "Multiplying 3th row *2: " << endl;
    m1->multiplying_row(2, 2);
    cout << *m1 << endl;

    cout << "Add 3* 1st row to 2nd row: " << endl;
    m1->add_multiplied_row(1, 0, 3);
    cout << *m1 << endl;

    cout << "Add 2* 5th row to 3th row: " << endl;
    m1->add_multiplied_row(4, 2, 2);
    cout << *m1 << endl;

    cout << "Switching 1, 3 columns: " << endl;
    m1->switching_two_columns(0, 2);
    cout << *m1 << endl;

    cout << "Multiplying 1st column *2: " << endl;
    m1->multiplying_column(0, 2);
    cout << *m1 << endl;

    cout << "Add 2* 1st column to 6th column: " << endl;
    m1->add_multiplied_column(5, 0, 2);
    cout << *m1 << endl;

    cout << "Matrix M1 transposition: " << endl;
    m1->transposition();
    cout << *m1 << endl;

    cout << "Add 2nd row to 1st row: " << endl;
    m1->add_multiplied_row(0, 1, 1);
    cout << *m1 << endl;

    cout << "Delete 2nd row and column: " << endl;
    cout << del(1, 1, *m1) <<endl;

    cout << "ACTUAL M1 MATRIX:" << endl;
    cout << *m1 << endl;

    cout << "DETERMINANT M1 MATRIX:" << endl;
    cout << determinant(*m1) << endl << endl;

    cout << "INVERSE M1 MATRIX:" << endl;
    cout << inverse(*m1) << endl << endl;

    return 0;
}
