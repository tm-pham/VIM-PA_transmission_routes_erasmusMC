#include <string>
#include <sstream>
#include <iostream>
#include <vector>
#include <fstream>
using namespace std;
// Changed int to double
void printarray(vector< vector<double> >& array,string s)
{
 cout << s<<"\n";
 for (size_t i=0; i<array.size(); ++i)
     {
         for (size_t j=0; j<array.at(i).size(); ++j)
         {
             cout << array.at(i).at(j) << "|"; // (separate fields by |)
         }
         cout << "\n";
     }
}
