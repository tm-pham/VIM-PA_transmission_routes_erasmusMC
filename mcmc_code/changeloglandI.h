#ifndef _DSS_CHANGELOGLANDI_H_
#define _DSS_CHANGELOGLANDI_H_

#include <iostream>
#include <vector>
#include <algorithm>
#include <math.h>
#include "pacquisition.h"
#include "stdhdr.h"
using namespace std;

void changeloglandI(int patid,int oldacquisitionday,int oldadmissionstate,int oldacquisition,int newacquisitionday,int newadmissionstate,int newacquisition,
                    int startperiod, int endperiod,double *logl,parameters param,
                    vector< vector< vector <int> > >& present,
                    vector< vector<int> >& I,vector< vector<int> >& N,vector< vector<int> >& colstatus,vector< vector<int> >& culturearray,
                    vector< vector<int> >& cultureperpatient,vector< vector<int> >& whereabouts, vector< vector<int> >& numberofacquisitions,
                    vector <int> & admissionday,int multipleparameters);




#endif //_DSS_CHANGELOGLANDI_H_


