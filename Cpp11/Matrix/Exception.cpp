#include <iostream>
#include "Exception.h"

using namespace std;


const char* wrong_matrix_size::what() const throw()
{
    return "WRONG MATRIX SIZE\n";
}

const char* wrong_matrix_size_plusminus::what() const throw()
{
    return "CANT ADD NON SQUARE MATRICES\n";
}

const char* wrong_matrix_size_mult::what() const throw()
{
    return "CANT MULTIPLY MATRICES THAT HAVE SUCH DIMENSIONS\n";
}

const char* determinant_equal_zero::what() const throw()
{
    return "DETERMINANT EQUAL ZERO\n";
}

const char* row_not_exist::what() const throw()
{
    return "ROW DO NOT EXIST\n";
}

const char* column_not_exist::what() const throw()
{
    return "COLUMN DO NOT EXIST\n";
}

const char* row_column_not_exist::what() const throw()
{
    return "ROW OR COLUMN DO NOT EXIST\n";
}



