#ifndef _DSS_ENVIRONMENT_H_
#define _DSS_ENVIRONMENT_H_

#include <algorithm>
#include <math.h>
#include "stdhdr.h"

using namespace std;

double environment(parameters param,int I,int N,double E_t);
void updateenvironment(parameters param,vector< vector<int> >& I,vector< vector<int> >& N,vector< vector<double> >& E,int numberofwards,int maxdate);
void updateenvironment2(parameters param,vector< vector<int> >& I,vector< vector<int> >& N,vector<int>& admissionstate,vector<int>&acquisition,vector<int> &acquisitionday,vector<int> &admissionday,vector<int> &dischargeday,vector<vector<double> >& E,vector<vector<double> >& E_stay,vector<vector<double> >& E_dis,int numberofwards,int maxdate);
#endif //_DSS_ENVIRONMENT_H_
