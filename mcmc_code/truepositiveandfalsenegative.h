#ifndef _DSS_TRUEPOSITIVEANDFALSENEGATIVE_H_
#define _DSS_TRUEPOSITIVEANDFALSENEGATIVE_H_

#include <iostream>
#include <vector>
#include "stdhdr.h"

using namespace std;
void truepositiveandfalsenegative(int *truepositive,int *falsenegative,vector< vector<int> >& culturearray,vector <int> & admissionday,
                                 vector<vector<int> > &colstatus,int startperiod, int endperiod);


void falsenegativefunction(int *falsenegative,vector< vector<int> >& culturearray,vector <int> & admissionday,
                                 vector<vector<int> > &colstatus,int startperiod, int endperiod);

void numberposatadmisison(int *numberpos,int *numberneg,vector <int> & admissionstate,vector <int> & admissionday,int startperiod, int endperiod);
#endif //_DSS_TRUEPOSITIVEANDFALSENEGATIVE_H_
