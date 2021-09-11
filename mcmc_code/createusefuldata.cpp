#include <iostream>
#include <vector>


using namespace std;
void admissiondischargeday(vector< vector<int> >& whereabouts,vector <int> & admissionday,vector <int> & dischargeday)
{
 unsigned int i;
 for(i=0;i<whereabouts.size();i++)
     {
         admissionday[i]=whereabouts[i][0];
         dischargeday[i]=whereabouts[i][whereabouts[i].size()]+1;
        // cout << i << ", admission="<< admissionday[i]<<", discharge="<< dischargeday[i]<<"\n";
     }

}

void cultureresultsperpatient(vector< vector<int> >& culturearray,vector< vector<int> >& cultureperpatient)
{
 unsigned int culture;
 for(culture=0;culture<culturearray.size();culture++)
    {
     cultureperpatient.at(culturearray.at(culture).at(0)).push_back(culture);
    }
    /** each patient has a list cultureperpatient.at(patient) which numbers representing the elements of culturearray**/
}




void determinepossibleacquisitiondays(vector<int>& possibleacquistiondays,vector< vector<int> >& culturearray,vector< vector<int> >& whereabouts,vector <int> & admissionday)
{
	cout << "determinpossibleacquisitiondays function begin." << endl;
unsigned int patient, culture,day,result;
for(patient=0;patient<whereabouts.size();patient++)
   {
    possibleacquistiondays.at(patient)=whereabouts.at(patient).size();
   }
 /** set possibleacquisitiondays to length of stay of all patients, below it is changed to a lower number if dictated by culture results **/


for(culture=0;culture<culturearray.size();culture++)
   {
    patient=culturearray.at(culture).at(0);
    day=culturearray.at(culture).at(1)-admissionday.at(patient);
    result=culturearray.at(culture).at(2);
    if(result==1&&day<(unsigned int)possibleacquistiondays.at(patient))
      {
       possibleacquistiondays.at(patient)=day;
      }
   }
   cout << "determinpossibleacquisitiondays function end." << endl;
}


void numberofpatientsperward(vector< vector<int> >& n,vector< vector<int> >& admissionarray,vector< vector<int> >& whereabouts,vector <int> & admissionday,vector <int> & dischargeday)
{
 unsigned int patientday;
 for(patientday=0;patientday<admissionarray.size();patientday++)
    {
     /// increase the number of patients in ward admissionarray.at(patient).at(2) at day admissionarray.at(patient).at(1) by 1.
     n.at(admissionarray.at(patientday).at(2)).at(admissionarray.at(patientday).at(1))+=1;
     /// assume per patient, days are ordered
     whereabouts.at(admissionarray.at(patientday).at(0)).push_back(admissionarray.at(patientday).at(2));
     if(admissionarray.at(patientday).at(1)<admissionday.at(admissionarray.at(patientday).at(0)))admissionday.at(admissionarray.at(patientday).at(0))=admissionarray.at(patientday).at(1);
     if(admissionarray.at(patientday).at(1)+1>dischargeday.at(admissionarray.at(patientday).at(0)))dischargeday.at(admissionarray.at(patientday).at(0))=admissionarray.at(patientday).at(1)+1;
    }
}

void whopresentwhen(vector< vector< vector <int> > >& present,vector< vector<int> >& admissionarray)
{
 unsigned int patientday;
 for(patientday=0;patientday<admissionarray.size();patientday++)
     {
        //admissiondata is of form: patid, date, roomnumber
       present.at(admissionarray.at(patientday).at(2)).at(admissionarray.at(patientday).at(1)).push_back (admissionarray.at(patientday).at(0));
     }
 //cout <<"end whopresentwhen\n";
}


void numberofinfectiouspatients2(vector< vector<int> >& I,vector< vector<int> >& whereabouts,vector< vector<int> >& colstatus, vector <int> & admissionday,vector <int> & acquisitionday,
                                       vector <int> & admissionstate,vector <int> & acquisition,vector< vector<int> >& numberofacquisitions,int maxdate,int numberofwards)
{
 unsigned int patient,day;
 int day2,unit;
 for(unit=0;unit<numberofwards;unit++)
 {
  for(day2=0;day2<maxdate;day2++)
    {
     I.at(unit).at(day2)=0;
     numberofacquisitions.at(unit).at(day2)=0;
    }
 }
 for(patient=0;patient<whereabouts.size();patient++)
    {
      if(admissionstate.at(patient)==1)//patient is colonized upon admission
        {
         for(day=0;day<whereabouts.at(patient).size();day++)
            {
             I.at(whereabouts.at(patient).at(day)).at(admissionday.at(patient)+day)+=1;
             colstatus.at(patient).at(day)=-1;
            }


        }
      else//patient uncolonized upon admission
        {
          if(acquisition.at(patient)==1)//acquisition during stay
            {
              for(day2=0;day2<acquisitionday.at(patient);day2++)
                 {
                   colstatus.at(patient).at(day2)=0;
                 }
              colstatus.at(patient).at(acquisitionday.at(patient))=1;
              for(day=acquisitionday.at(patient)+1;day<whereabouts.at(patient).size();day++)
                 {
                   I.at(whereabouts.at(patient).at(day)).at(admissionday.at(patient)+day)+=1;
                   colstatus.at(patient).at(day)=-1;
                 }
              numberofacquisitions.at(whereabouts.at(patient).at(acquisitionday.at(patient))).at(admissionday.at(patient)+acquisitionday.at(patient))+=1;
              for(day2=0;day2<acquisitionday.at(patient);day2++)
                  {
                   colstatus.at(patient).at(day2)=0;
                  }
              colstatus.at(patient).at(acquisitionday.at(patient))=1;
              for(day=acquisitionday.at(patient)+1;day<whereabouts.at(patient).size();day++)
                  {
                   colstatus.at(patient).at(day)=-1;
                  }
            }
          else
            {
               for(day=0;day<whereabouts.at(patient).size();day++)
                  {
                   colstatus.at(patient).at(day)=0;
                  }
            }

        }
    }

}


void numberofinfectiouspatientsperward(vector< vector<int> >& I,vector< vector<int> >& whereabouts,vector< vector<int> >& colstatus, vector <int> & admissionday,vector <int> & acquisitionday,
                                       vector <int> & admissionstate,vector <int> & acquisition,vector< vector<int> >& numberofacquisitions)
{
 unsigned int patient,day;
 int day2;

 for(patient=0;patient<whereabouts.size();patient++)
    {
      if(admissionstate.at(patient)==1)//patient is colonized upon admission
        {
         for(day=0;day<whereabouts.at(patient).size();day++)
            {
             I.at(whereabouts.at(patient).at(day)).at(admissionday.at(patient)+day)+=1;
             colstatus.at(patient).push_back(-1);
             //cout << "patient="<<patient<< ", colstatus="<<colstatus.at(patient).at(day)<<endl;
             if(patient==15){
               cout << I.at(whereabouts.at(patient).at(day)).at(admissionday.at(patient)+day) << endl; 
             }
            }

        }
      else//patient uncolonized upon admission
        {
          if(acquisition.at(patient)==1)//acquisition during stay
            {
              for(day2=0;day2<acquisitionday.at(patient);day2++)
                 {
                   colstatus.at(patient).push_back(0);
                 }
              colstatus.at(patient).push_back(1);
              for(day=acquisitionday.at(patient)+1;day<whereabouts.at(patient).size();day++)
                 {
                   I.at(whereabouts.at(patient).at(day)).at(admissionday.at(patient)+day)+=1;
                   colstatus.at(patient).push_back(-1);
                 }
              numberofacquisitions.at(whereabouts.at(patient).at(acquisitionday.at(patient))).at(admissionday.at(patient)+acquisitionday.at(patient))+=1;

            }
          else
            { //cout <<"start no acquisition\n";
               for(day=0;day<whereabouts.at(patient).size();day++)
                  {
                   colstatus.at(patient).push_back(0);
                  }
            }
        }
    }

}

void totaldaysatrisk(vector <int> & daysatrisk,vector <int> & acquisitions,
                                 vector< vector<int> >& numberofacquisitions,
                                 vector< vector<int> >& N,
                                 vector< vector<int> >& I,int startday,int endday)
{
 int unit,day;
 for(unit=0;unit<N.size();unit++)
    {
     acquisitions.at(unit)=0;
     daysatrisk.at(unit)=0;
     for(day=startday;day<endday;day++)
        {
         daysatrisk.at(unit)+=N.at(unit).at(day)-I.at(unit).at(day);
         acquisitions.at(unit)+= numberofacquisitions.at(unit).at(day);
        }
    }
}
void defineneverposdefpostemppos(vector <int> & listneverpos,vector <int> & listdefpos,vector <int> & listtemppos,
                                 vector <int> & admissionstate,vector <int> & acquisition,
                                 vector< vector<int> >& culturearray,
                    vector< vector<int> >& cultureperpatient)
{
  int patient,culture;
  int temp;
  for (patient=0;patient<(int)admissionstate.size();patient++)
      {
       temp=0;
       //cout<<"admissionstate["<<patient<<"]="<<admissionstate.at(patient)<<", acquisition["<<patient<<"]="<<acquisition.at(patient)<<endl;
       if(max(admissionstate.at(patient),acquisition.at(patient))==0)
         {
            listneverpos.push_back(patient);
         }
       else
         {
           //listeverpos.push_back(patient);
         }
         for(culture=0;culture<(int)cultureperpatient.at(patient).size();culture++)
            {
             if(culturearray.at(cultureperpatient.at(patient).at(culture)).at(2)==1)
               {
                listdefpos.push_back(patient);
                temp=1;
                break;
               }
            }
       if(temp==0&&max(admissionstate.at(patient),acquisition.at(patient))>0)
          {
           listtemppos.push_back(patient);
          }
      }

}


void addelementtovector(vector <int> & v,int patientid)
   {
    v.push_back(patientid);
   }

void deleteindexelementvector(vector <int> & v,int index)
   {
    v.at(index)=   v.at(v.size()-1);
    v.pop_back();
   }

 void poscultureposatadmission(vector< vector<int> >& culturearray,
                    vector< vector<int> >& cultureperpatient,vector <int> & admissionstate)
 {
   int patient,culture,culturenumber;
   for(patient=0;patient<(int)cultureperpatient.size();patient++)
   {
     admissionstate.at(patient)=0;
     for(culture=0;culture<(int)cultureperpatient.at(patient).size();culture++)
        {
         culturenumber=   cultureperpatient.at(patient).at(culture);
         if(culturearray.at(culturenumber).at(2)==1)
            {
             admissionstate.at(patient)=1;
            }
        }
   }
 }
 
 /*
void pastinfections(vector< vector<int>> infections,vector <int> admissionday,vector <int> dischargeday,int maxdate){
  int patient, day;
  for(patient = 0; patient < admissionday.size(); patient++){
	// Patient is present and got infected before
	acqday = acquisitionday.at(patient); 
    if(!is.nan(acqday) && day >= acqday && day <= dischargeday[patient])){
	   infections.at(0).at(acqday).push_back(patient); 	 
    }
    // Patient was present and got infected
    if(!is.nan(acqday) && day > dischargeday[patient])){
	   infections.at(1).at(day).push_back(patient); 	 
    }
  }
}
* */
