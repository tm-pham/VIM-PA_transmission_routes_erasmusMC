#ifndef _DSS_CREATEUSEFULDATA_H_
#define _DSS_CREATEUSEFULDATA_H_

#include <iostream>
#include <vector>

using namespace std;
void admissiondischargeday(vector< vector<int> >& whereabout,vector <int> & admissionday,vector <int> & dischargeday);
void cultureresultsperpatient(vector< vector<int> >& culturearray,vector< vector<int> >& cultureperpatients);
void determinepossibleacquisitiondays(vector<int>& possibleacquistiondays,vector< vector<int> >& culturearray,vector< vector<int> >& whereabouts,vector <int> & admissionday);
void numberofpatientsperward(vector< vector<int> >& n,vector< vector<int> >& admissionarray,vector< vector<int> >& whereabouts,vector <int> & admissionday,vector <int> & dischargeday);
void whopresentwhen(vector< vector< vector <int> > >& present,vector< vector<int> >& admissionarray);
void numberofinfectiouspatients2(vector< vector<int> >& I,vector< vector<int> >& whereabouts,vector< vector<int> >& colstatus,vector <int> & admissionday,vector <int> & acquisitionday,vector <int> & admissionstate,vector <int> & acquisition,
                                       vector< vector<int> >& numberofacquisitions,int maxdate,int numberofwards);
void numberofinfectiouspatientsperward(vector< vector<int> >& I,vector< vector<int> >& whereabouts,vector< vector<int> >& colstatus,vector <int> & admissionday,vector <int> & acquisitionday,vector <int> & admissionstate,vector <int> & acquisition,
                                       vector< vector<int> >& numberofacquisitions);
void totaldaysatrisk(vector <int> & daysatrisk,vector <int> & acquisitions,
                                 vector< vector<int> >& numberofacquisitions,
                                 vector< vector<int> >& N,
                                 vector< vector<int> >& I,int startday,int endday);

void defineneverposdefpostemppos(vector <int> & listneverpos,vector <int> & listdefpos,vector <int> & listtemppos,
                                 vector <int> & admissionstate,vector <int> & acquisition,
                                 vector< vector<int> >& culturearray,
                    vector< vector<int> >& cultureperpatient);

 void addelementtovector(vector <int> & v,int patientid);

void deleteindexelementvector(vector <int> & v,int index);

 void poscultureposatadmission(vector< vector<int> >& culturearray,
                    vector< vector<int> >& cultureperpatient,vector <int> & admissionstate);
#endif //_DSS_CREATEUSEFULDATA_H_

