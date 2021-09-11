#ifndef _DSS_RELABEL_H_
#define _DSS_RELABEL_H_

#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

void relabeldates(vector< vector<int> >& admissionarray,vector< vector<int> >& culturearray,int *maxdate);
void relabelpatientID(vector< vector<int> >& admissionarray,vector< vector<int> >& culturearray, int *numberofpatients);
void relabelwards(vector< vector<int> >& array,int *numberofwards);
#endif //_DSS_RELABEL_H_
