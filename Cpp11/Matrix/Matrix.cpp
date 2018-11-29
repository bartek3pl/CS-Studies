#include <iostream>
#include <iomanip>
#include "Matrix.h"
#include "Exception.h"
using namespace std;

namespace calculations {    ////////////////////////////////////////////////// CONSTRUCTORS

    //Dynamic square array with "ones" on the diagonal
    Matrix::Matrix(int xx)
    {
        x = xx;
        y = xx;
        tab = new int *[xx];

        for (int i = 0; i < xx; ++i)
        {
            tab[i] = new int [xx];

            for (int j = 0; j < xx; ++j)
            {
                if(j == i)
                    tab[i][j] = 2;
                else
                    tab[i][j] = 0;
            }
        }

    }

    //Dynamic rectangular array filled with "0"
    Matrix::Matrix(int xx, int yy)
    {
        x = xx;
        y = yy;
        tab = new int *[xx];

        for (int i = 0; i < xx; ++i)
        {
            tab[i] = new int [yy];

            for (int j = 0; j < yy; ++j)
                tab[i][j] = 4;
        }
    }

    //Move constructor
    Matrix::Matrix(Matrix&& matrix)
    {
        x = matrix.x;
        y = matrix.y;
        tab = new int *[x];

        for(int i = 0; i < x; ++i)
        {
            tab[i] = new int [x];
            for(int j = 0; j < y; ++j)
                tab[i][j] = matrix.tab[i][j];

            delete [] matrix.tab[i];
        }
        delete [] matrix.tab;
    }

    //Move assignment
    Matrix & Matrix::operator = (Matrix&& matrix)
    {
        if(&matrix == this) // jeœli to ten sam element
            return *this;

        swap(x, matrix.x);
        swap(y, matrix.y);
        swap(tab, matrix.tab);
        return *this;
    }

    //Copy constructor
    Matrix::Matrix(const Matrix& matrix)
    {
        x = matrix.x;
        y = matrix.y;
        tab = new int *[x];

        for(int i = 0; i < x; ++i)
        {
            tab[i] = new int [x];
            for(int j = 0; j < y; ++j)
                tab[i][j] = matrix.tab[i][j];
        }
    }

    //Copy assignment
    Matrix & Matrix::operator = (const Matrix& matrix)
    {
        if(x != matrix.x || y != matrix.y) //if the dimensions do not match
        {
            for(int i = 0; i < x; ++i)
                delete [] tab[i];
            delete [] tab;

            x = matrix.y;
            y = matrix.y;
            tab = new int *[x];

            for(int i = 0; i < x; ++i)
            {
                tab[i] = new int [x];
                for(int j = 0; j < y; ++j)
                    tab[i][j] = matrix.tab[i][j];
            }
        }

        return *this;
    }

    //Destructor
    Matrix::~Matrix()
    {
        for(int i = 0; i < x; ++i)
            delete [] tab[i];
        delete [] tab;
    }

    /////////////////////////////////////////////////////////

    //OPERATORS

    Matrix operator *(const Matrix &m, int sk)
    {
        Matrix temp = Matrix(m.x, m.y);
        for (int i = 0; i < temp.x; ++i) //multiplies elements by a scalar
            for (int j = 0; j < temp.y; ++j)
                temp.tab[i][j] = m.tab[i][j] * sk;
        return temp;
    }

    Matrix & Matrix::operator +=(const Matrix &m2)
    {
        if(x != m2.x || y != m2.y)
            throw (wrong_matrix_size_plusminus());

        for (int i = 0; i < x; ++i)
            for (int j = 0; j < y; ++j)
                tab[i][j] = tab[i][j] + m2.tab[i][j];
        return *this;
    }

    Matrix operator +(const Matrix &m1, const Matrix &m2)
    {
        if(m1.x != m2.x || m1.y != m2.y)    //if rows or columns do not match
            throw (wrong_matrix_size_plusminus());

        Matrix temp = Matrix(m1.x, m1.y);

        for (int i = 0; i < temp.x; ++i)
            for (int j = 0; j < temp.y; ++j)
                temp.tab[i][j] = m1.tab[i][j] + m2.tab[i][j];
        return temp;
    }

    Matrix & Matrix::operator -=(const Matrix &m2)
    {
        if(x != m2.x || y != m2.y)
            throw (wrong_matrix_size_plusminus());

        for (int i = 0; i < x; ++i)
            for (int j = 0; j < y; ++j)
                tab[i][j] = tab[i][j] - m2.tab[i][j];
        return *this;
    }

    Matrix operator -(const Matrix &m1, const Matrix &m2)
    {
        if(m1.x != m2.x || m1.y != m2.y)
            throw (wrong_matrix_size_plusminus());

        Matrix temp = Matrix(m1.x, m1.y);

        for (int i = 0; i < temp.x; ++i)
            for (int j = 0; j < temp.y; ++j)
                temp.tab[i][j] = m1.tab[i][j] - m2.tab[i][j];
        return temp;
    }

    Matrix operator *(const Matrix &m1, const Matrix &m2)
    {
         if(m1.x != m2.y)
            throw (wrong_matrix_size_mult());

        Matrix temp = Matrix(m1.x, m1.y);

        for (int i = 0; i < temp.x; ++i)
            for (int j = 0; j < temp.x; ++j)
                temp.tab[i][j] = 0;

        for (int i = 0; i < temp.x; ++i)
            for (int j = 0; j < temp.x; ++j)
                for (int k = 0; k < temp.x; ++k)
                    temp.tab[i][j] = temp.tab[i][j] + m1.tab[i][k] * m2.tab[k][j];
        return temp;
    }

    /////////////////////////////// OPERACJE ELEMENTARNE

    //Matrix transpositioning

    Matrix & Matrix::transposition()
    {
        for (int i = 0; i < x; ++i)
            for (int j = 0; j < y; ++j)
                if(i <= j)
                    swap(tab[i][j], tab[j][i]);
        return *this;
    }

    //On rows

    void Matrix::switching_two_rows(int w1, int w2)
    {
        if(w1+1 > x || w2+1 > x || w1 < 0 || w2 < 0)
            throw(row_not_exist());
        swap(tab[w1], tab[w2]);
    }

    void Matrix::multiplying_row(int w1, int sk)
    {
        for (int j = 0; j < x; ++j)
            tab[w1][j] *= sk;
    }

    void Matrix::add_multiplied_row(int w1, int w2, int sk)
    {
        for (int j = 0; j < x; ++j)
            tab[w1][j] += tab[w2][j] * sk;
    }

    //On columns

    void Matrix::switching_two_columns(int k1, int k2)
    {
        if(k1+1 > y || k2+1 > y || k1 < 0 || k2 < 0)
            throw(column_not_exist());
        for (int j = 0; j < x; ++j)
            swap(tab[j][k1], tab[j][k2]);
    }

    void Matrix::multiplying_column(int k1, int sk)
    {
        for (int j = 0; j < x; ++j)
            tab[j][k1] *= sk;
    }

    void Matrix::add_multiplied_column(int k1, int k2, int sk)
    {
        for (int j = 0; j < x; ++j)
            tab[j][k1] += tab[j][k2] * sk;
    }

    Matrix del(int i, int j, const Matrix& m)
    {
        if(i < 0 || i+1 > m.x || j < 0 || j + 1 > m.y)
            throw(row_column_not_exist());

        Matrix temp = Matrix(m.x-1, m.y-1);

        int kontrRow = 0;
        int kontrCol = 0;

        for(int k = 0; k < m.x; ++k)
        {
            if(k == i)
            {
                kontrRow = 1;
                continue;
            }
            for(int p = 0; p < m.y; ++p)
            {
                if(p == j)
                {
                    kontrCol = 1;
                    continue;
                }
                temp.tab[k - kontrRow][p - kontrCol] = m.tab[k][p];
            }
            kontrCol = 0;
        }

        return temp;
    }

    double determinant(const Matrix& m)
    {
        if(m.x != m.y)
            throw(wrong_matrix_size());

        if(m.x == 1)
            return m.tab[0][0];
        else
        {
            double result = 0;
            for(int i = 0; i < m.x; ++i)
                result += (i%2 == 0? 1:(-1)) * m.tab[0][i] * (determinant(del(0, i, m)));

            return result;
        }
    }

    Matrix inverse(const Matrix& m)
    {
        if(determinant(m) == 0)
            throw(determinant_equal_zero());

        Matrix temp = Matrix(m.x);

        for(int i = 0; i < temp.x; ++i)
            for(int j = 0; j < temp.y; ++j)
                temp.tab[i][j] = (((i+j)%2 == 0? 1:(-1)) * determinant(del(i, j, m))) / determinant(m);
        return temp.transposition();
    }

    //Input and output

    ostream& operator << (ostream& wy, const Matrix& m)
    {
        for(int i = 0; i < m.x; ++i)
        {
            for(int j = 0; j < m.y; ++j)
                wy << setw(3) << m.tab[i][j];
            wy << "\n";
        }
        return wy;
    }

    istream& operator >> (istream &we, Matrix &m)
    {
        for(int i = 0; i < m.x; ++i)
        {
            for(int j = 0; j < m.y; ++j)
            {
                cout << "(" << i << "," << j << "): ";
                we >> m.tab[i][j];
            }
        }
        return we;
    }

}














