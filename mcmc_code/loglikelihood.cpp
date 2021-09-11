#include <iostream>
#include <vector>
#include <math.h>
#include <algorithm>
#include "stdhdr.h"
#include "pacquisition.h"
using namespace std;
// ADDED: Vector for Environmental contamination E
double loglikelihood(vector <int> & acquisitionday,vector <int> & acquisition,vector< vector<int> >& culturearray,vector< vector<int> >& N,vector< vector<int> >& I,vector< vector<int> >& numberofacquisitions, vector <int> & admissionstate,parameters param,int startperiod, int endperiod,
                     vector <int> & admissionday,int multipleparameters,vector<vector<double> >& E, int addingenvironment)

{
 long double logl=0;
 double loglacq=0,loglnoacq=0,logladm=0,loglsens=0;
 int patid,day;
 unsigned int unit,patient,culture;
 double ntotal=0;
 double itotal=0;
 if(multipleparameters==0)
 {
  for(day=startperiod;day<endperiod;day++)
    {
     //cout<<"day="<<day<<endl;
     for(unit=0;unit<N.size();unit++)
        {
            itotal+=I.at(unit).at(day);
            ntotal+=N.at(unit).at(day);
            //cout<<"unit="<<unit<<endl;
            if(N.at(unit).at(day)-I.at(unit).at(day)>0)
              {
				/** day added */  
				// Improving of runtime by saving loglnoacq???
				loglnoacq = logpnoacquisition(param,I.at(unit).at(day),N.at(unit).at(day),E.at(unit).at(day),addingenvironment);
                logl+= loglnoacq*(N.at(unit).at(day)-I.at(unit).at(day)-numberofacquisitions.at(unit).at(day));
                //loglnoacq=logpnoacquisition(param,I.at(unit).at(day),N.at(unit).at(day));
                //cout << "No acquisition logl="<<loglnoacq<<", ";
                /** day added */  
                //logl+= logpacquisition(param,I.at(unit).at(day),N.at(unit).at(day),E.at(unit).at(day),addingenvironment)*numberofacquisitions.at(unit).at(day);
                loglacq=logpacquisition(param,I.at(unit).at(day),N.at(unit).at(day),E.at(unit).at(day),addingenvironment)*numberofacquisitions.at(unit).at(day);
                logl += log(1.0-exp(loglnoacq))*numberofacquisitions.at(unit).at(day);
                //loglacq=logpacquisition(param,I.at(unit).at(day),N.at(unit).at(day));
                //cout << "Acquisition logl="<<loglacq<<endl;
                if(isnan(logl)){
					cout << "Day="<<day << endl;
					cout << "I.at(unit).at(day)=" << I.at(unit).at(day)<< "; N.at(unit).at(day)=" << N.at(unit).at(day)<< "; E.at(unit).at(day)="<< E.at(unit).at(day)<< endl;
					cout << "param.a="<<param.a<<"; param.b="<<param.b<<"; param.c="<<param.c<<"; param.nu="<<param.nu<<"; param.mu="<<param.mu << endl;
					cout << "No acquisition logl = " << logpnoacquisition(param,I.at(unit).at(day),N.at(unit).at(day),E.at(unit).at(day),addingenvironment)*(N.at(unit).at(day)-I.at(unit).at(day)-numberofacquisitions.at(unit).at(day))<<endl;
					cout << "Acquisition logl = "<<logpacquisition(param,I.at(unit).at(day),N.at(unit).at(day),E.at(unit).at(day),addingenvironment) << endl;
					cout << "Number of acquisitions = " << numberofacquisitions.at(unit).at(day) << endl;
					exit(0);
				}
              }
        }
    }
 }
 else
 {
//cout << "multiple"<<endl;
 for(day=startperiod;day<endperiod;day++)
    {
     //cout<<"day="<<day<<endl;
     for(unit=0;unit<N.size();unit++)
        {
            //cout<<"unit="<<unit<<endl;
            if(N.at(unit).at(day)-I.at(unit).at(day)>0)
              {
                //cout<<"a"<<endl;
                /** day added */  
                logl+= logpnoacquisitionward(param,I.at(unit).at(day),N.at(unit).at(day),unit,E.at(unit).at(day),addingenvironment)*(N.at(unit).at(day)-I.at(unit).at(day)-numberofacquisitions.at(unit).at(day));
                //cout<<"b"<<endl;
                /** day added */  
                logl+= logpacquisitionward(param,I.at(unit).at(day),N.at(unit).at(day),unit,E.at(unit).at(day),addingenvironment)*numberofacquisitions.at(unit).at(day);
                if(isnan(logl)){
					cout << "N.at(unit).at(day)=" << N.at(unit).at(day)<< endl;
				}
              }
        }
    }
 }
/** Calclulate contribution being colonized on admission to loglikelihood */
 for(patient=0;patient<admissionstate.size();patient++)
    {
     if(admissionstate.at(patient)==1)
         {
             logl+=log(param.f);logladm+=log(param.f);
         }
     else
        {
            logl+=log(1.-param.f);logladm+=log(1.-param.f);
        }
    }
 for(culture=0;culture<culturearray.size();culture++)
    {
     if(culturearray.at(culture).at(1)>=startperiod&&culturearray.at(culture).at(1)<=endperiod)
       {
        if(culturearray.at(culture).at(2)==0)//culture negative
          {
           patid=culturearray.at(culture).at(0);
           if(admissionstate.at(patid)==1||(acquisition.at(patid)==1&&culturearray.at(culture).at(1)>admissionday.at(patid)+acquisitionday.at(patid)))
             {
               logl+=log(1.0-param.phi);//false negative culture
               loglsens=log(1.0-param.phi);
               //cout << "False negative loglsens=" << loglsens << endl;
             }
           else
             {
               //culture negative and patient negative
             }
          }
        else //culture positive
          {
           patid=culturearray.at(culture).at(0);

           if(admissionstate.at(patid)==1||(acquisition.at(patid)==1&&culturearray.at(culture).at(1)>admissionday.at(patid)+acquisitionday.at(patid)))
             {
              logl+=log(param.phi);//positive culture for colonized patient
              loglsens=log(param.phi);
              //cout << "True positive loglsens=" << loglsens << endl;
             }
           else
             {
              logl-=1000000.0; //culture positive and patient uncolonized
              loglsens-=1000000.0;
             }
          }
       }

    }
 //cout <<"mean prev="<< itotal/ntotal<<endl;
 //cout<<"logl="<<logl<<", loglacq="<<loglacq<<", loglnoacq="<<loglnoacq<<", logladm="<<logladm<<", loglsens="<<loglsens<<endl;
 return logl;
}
