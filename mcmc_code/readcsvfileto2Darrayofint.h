#ifndef _DSS_READCSVFILETO2DARRAYOFINT_H_
#define _DSS_READCSVFILETO2DARRAYOFINT_H_
#include <string>
#include <sstream>
#include <iostream>
#include <vector>
#include <fstream>
using namespace std;
void READCSVFILETO2DARRAYOFINT(string s,vector< vector<int> >& array);
void printarray(vector< vector<int> >& array,string s);
// Changed int to double/string
void printarray(vector< vector<double> >& array,string s);
#endif //_DSS_READCSVFILETO2DARRAYOFINT_H_
