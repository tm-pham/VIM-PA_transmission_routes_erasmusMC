#ifndef _DSS_LOGLIKELIHOOD_H_
#define _DSS_LOGLIKELIHOOD_H_

#include <iostream>
#include <vector>
#include <math.h>
#include <algorithm>
#include "stdhdr.h"
#include "pacquisition.h"
using namespace std;
double loglikelihood(vector <int> & acquisitionday,vector <int> & acquisition,vector< vector<int> >& culturearray,vector< vector<int> >& N,
                     vector< vector<int> >& I,vector< vector<int> >& numberofacquisitions,vector <int> & admissionstate, parameters param,
                     int startperiod, int endperiod,vector <int> & admissionday,int multipleparameters, vector<vector<double> >& E, int addingenvironment);
#endif //_DSS_LOGLIKELIHOOD_H_


