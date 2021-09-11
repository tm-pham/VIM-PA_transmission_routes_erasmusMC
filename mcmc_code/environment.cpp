#include <algorithm>
#include <math.h>
#include "stdhdr.h"

using namespace std;
// ADDED environmental contamination E

void updateenvironment(parameters param,vector< vector<int> >& I,vector< vector<int> >& N,vector< vector<double> >& E,int numberofwards,int maxdate){
	int unit, day;
	if(param.c == 0){
		for(unit=0;unit<numberofwards;unit++){
			for(day=0;day<maxdate;day++){
				E.at(unit).at(day) = param.E0;
			}
		}
	}else{
		double E_t;
		for(unit=0;unit<numberofwards;unit++){
			E.at(unit).at(0)=param.E0;
			for(day=0;day<maxdate;day++){
				E_t=E.at(unit).at(day-1)*exp(-param.mu)+(1.0-exp(-param.mu))*(param.nu/param.mu)*((double)I.at(unit).at(day-1)/(double)N.at(unit).at(day-1));
				E.at(unit).at(day)=E_t;
			}
		}
	}
}

void updateenvironment2(parameters param,vector< vector<int> >& I,vector< vector<int> >& N,vector<int>& admissionstate,vector<int>& acquisition,vector<int>& acquisitionday,vector<int>& admissionday,vector<int>& dischargeday,vector<vector<double> >& E,
                        vector<vector<double> >& E_stay,vector<vector<double> >& E_dis,int numberofwards,int maxdate){
	int unit,day,patient;
	if(param.c == 0){
		for(unit=0;unit<numberofwards;unit++){
			for(day=0;day<maxdate;day++){
				E.at(unit).at(day) = param.E0;
			}
		}
	}else{
		int acqday,admday,disday;
		double E_t, Estay, Edis;
		for(unit=0;unit<numberofwards;unit++){
			E.at(unit).at(0)=param.E0;
			E_stay.at(unit).at(0) = 0.0; E_dis.at(unit).at(0) = 0.0;
			for(day=1;day<=maxdate;day++){
                if(N.at(unit).at(day-1)>0){
                    E_t=E.at(unit).at(day-1)*exp(-param.mu)+(1.0-exp(-param.mu))*(param.nu/param.mu)*((double)I.at(unit).at(day-1)/(double)N.at(unit).at(day-1));
                }else{
                    E_t=E.at(unit).at(day-1)*exp(-param.mu)+(1.0-exp(-param.mu));
                }
				E.at(unit).at(day)=E_t;
				E_stay.at(unit).at(day) = 0.0; E_dis.at(unit).at(day) = 0.0;
				if(isnan(E.at(unit).at(day))){
                    cout << "day="<<day<<endl;
                    cout << "E_t="<<E_t<<"; N="<<N.at(unit).at(day-1)<<"; I="<<I.at(unit).at(day-1)<<"; param.mu="<<param.mu<<endl;
                    cout << "E[t-1]=" << E.at(unit).at(day-1) << endl;
                    exit(0);
				}
			}
			for(patient=0; patient<dischargeday.size(); patient++){
				if(acquisition.at(patient)==1 || admissionstate.at(patient)==1){
				  if(acquisitionday.at(patient)==-1){
				    acqday = admissionday.at(patient);
				  }
				  else acqday = admissionday.at(patient)+acquisitionday.at(patient)+1;
					disday = dischargeday.at(patient);
					for(day=acqday+1;day<=disday;day++){
						Estay = (param.nu/param.mu)*(1.0-exp(-param.mu*((double)day-(double)acqday)));
						E_stay.at(unit).at(day) = E_stay.at(unit).at(day)+ Estay/(double)N.at(unit).at(day-1);
					}
					/*
					for(day=disday;day<maxdate;day++){
						E_dis.at(unit).at(day) = E_dis.at(unit).at(day) + Estay*exp(-param.mu*((double)day-(double)disday))/(double)N.at(unit).at(day-1);
					}*/

				}
			}
		}
	}
}




