#include <iostream>
#ifndef EXP_H
#define EXP_H

using namespace std;

class wrong_matrix_size
{
    protected:
    virtual const char* what() const throw();
};

class wrong_matrix_size_plusminus
{
    protected:
    virtual const char* what() const throw();
};

class wrong_matrix_size_mult
{
    protected:
    virtual const char* what() const throw();
};

class determinant_equal_zero
{
    protected:
    virtual const char* what() const throw();
};

class row_not_exist
{
    protected:
    virtual const char* what() const throw();
};

class column_not_exist
{
    protected:
    virtual const char* what() const throw();
};

class row_column_not_exist
{
    protected:
    virtual const char* what() const throw();
};

#endif


