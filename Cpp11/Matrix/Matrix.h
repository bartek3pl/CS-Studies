#ifndef MAC_h
#define MAC_h

#include <iostream>
using namespace std;

namespace calculations
{
    class Matrix
    {
    public:

        int x, y;
        int **tab;

        //constructors and destructor

        Matrix(int); //unit square matrix
        Matrix(int, int); //rectangular matrix filled with "0"

        Matrix(Matrix&&);   //move constructor
        Matrix & operator = (Matrix&&);  //move assignment

        Matrix(const Matrix&);  //copy constructor
        Matrix & operator = (const Matrix&);   //copy assignment

        ~Matrix(); //destructor

        //operators

        friend Matrix operator*(const Matrix&, int); //multiplying by scalar

        Matrix & operator +=(const Matrix&);
        friend Matrix operator +(const Matrix&, const Matrix&);

        Matrix & operator -=(const Matrix&);
        friend Matrix operator -(const Matrix&, const Matrix&);

        friend Matrix operator *(const Matrix&, const Matrix&);

        //elementary transformations

        Matrix & transposition();

        void switching_two_rows(int, int);
        void multiplying_row(int, int);
        void add_multiplied_row(int, int, int);

        void switching_two_columns(int, int);
        void multiplying_column(int, int);
        void add_multiplied_column(int, int, int);

        //determinant, removal, inverse matrix

        friend Matrix del(int, int, const Matrix&);
        friend double determinant(const Matrix&);
        friend Matrix inverse(const Matrix&);

        //input, output

        friend istream& operator>>(istream &we, Matrix &x);
        friend ostream& operator<<(ostream& wy, const Matrix& x);
    };

}
#endif // MAC_h

