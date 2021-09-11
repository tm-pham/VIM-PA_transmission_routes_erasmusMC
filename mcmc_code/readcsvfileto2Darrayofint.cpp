#include <string>
#include <sstream>
#include <iostream>
#include <vector>
#include <fstream>
#include <stdlib.h>
using namespace std;
void READCSVFILETO2DARRAYOFINT(string s,vector< vector<int> >& array){

  ifstream in(s.c_str());

    string line, field;
    int tmp;
    vector<int> v;                // array of values for one line only

    while ( getline(in,line) )    // get next line in file
    {
        v.clear();
        stringstream ss(line);

        while (getline(ss,field,','))  // break line into comma delimitted fields
        {
            tmp=atoi(field.c_str());
            v.push_back(tmp);  // add each field to the 1D array
        }
        array.push_back(v);  // add the 1D array to the 2D array
    }

}

void printarray(vector< vector<int> >& array,string s){
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
// Changed int to double
void printarray(vector< vector<double> >& array,string s){
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

