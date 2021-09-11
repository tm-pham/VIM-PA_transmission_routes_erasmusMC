#include <iostream>
#include <vector>
#include "stdhdr.h"

using namespace std;
void truepositiveandfalsenegative(int *truepositive,int *falsenegative,vector< vector<int> >& culturearray,vector <int> & admissionday,
                                 vector<vector<int> > &colstatus,int startperiod, int endperiod)
{
 /** can be optimized as truepositive never changes during updates **/
 // int or unsigned int?
 //cout << "truepositiveandfalsenegative function begin." << endl;
 int patient,day,result,status;
 unsigned int culture;
 *truepositive=0;
 *falsenegative=0;
 cout <<"size="<<culturearray.size()<<endl;
 for(culture=0;culture<culturearray.size();culture++)
    {
      //cout <<"culture="<<culture<<endl;
      patient=culturearray.at(culture).at(0);
      day=culturearray.at(culture).at(1);
      //cout<<"day="<<day<<endl;
      //cout<<"patient="<<patient<<", admissionday="<<admissionday.at(patient)<<endl;
      if(day>=startperiod&&day<=endperiod)
        {

         day-=admissionday.at(patient);

         result=culturearray.at(culture).at(2);
         //cout<<"day="<<day<<", result="<<result<<", grootte="<<colstatus.at(patient).size()<<endl;
         if(day<(int)colstatus.at(patient).size()){
               status=colstatus.at(patient).at(day);
               //cout<<"day="<<day<<", result="<<result<<", colstatus="<<status<<endl;
               //-1, already colonized, 0, no acquisition during that day, 1 acquisition during that day.
               if(status==-1)
                 {
                  if(result==1)*truepositive+=1;
                  if(result==0){
					  //cout <<"culture="<<culture<<endl;
					  //cout<<"patient="<<patient<<endl;
					  //cout<<"day="<<day<<", result="<<result<<", colstatus="<<status<<endl;
					  *falsenegative+=1;
				  }
                 }
           }else//culture on day discharge
            {
             status=colstatus.at(patient).at(day-1);
             if(status==-1||status==1)
                 {
                  if(result==1)*truepositive+=1;
                  if(result==0){
					  //cout <<"culture="<<culture<<endl;
					  //cout<<"patient="<<patient<<endl;
					  //cout<<"day="<<day<<", result="<<result<<", colstatus="<<status<<endl;
					  *falsenegative+=1;
				  }
                 }
            }

        }
     }
    //cout << "end truepositiveandfalsenegative function."<<endl;
}

void falsenegativefunction(int *falsenegative,vector< vector<int> >& culturearray,vector <int> & admissionday,
                                 vector<vector<int> > &colstatus,int startperiod, int endperiod)
{
 /** can be optimized as truepositive never changes during updates **/

 int patient,day,result,status;
 unsigned int culture;
 *falsenegative=0;
 //cout <<"size="<<culturearray.size()<<endl;
 for(culture=0;culture<culturearray.size();culture++)
    {
      //cout <<"culture="<<culture<<endl;
      patient=culturearray.at(culture).at(0);
      day=culturearray.at(culture).at(1);
      //cout<<"patient="<<patient<<endl;
      if(day>=startperiod&&day<=endperiod)
        {

         day-=admissionday.at(patient);

         result=culturearray.at(culture).at(2);
         //cout<<"day="<<day<<", result="<<result<<"grootte="<<colstatus.at(patient).size()<<endl;
         if(day<(int)colstatus.at(patient).size())
           {
               status=colstatus.at(patient).at(day);
               //cout<<"day="<<day<<", result="<<result<<", colstatus="<<status<<endl;
               //-1, already colonized, 0, no acquisition during that day, 1 acquisition during that day.
               if(status==-1)
                 {
                  if(result==0)*falsenegative+=1;
                 }
           }
         else//culture on day discharge
            {
             status=colstatus.at(patient).at(day-1);
             if(status==-1||status==1)
                 {
                  if(result==0)*falsenegative+=1;
                 }
            }

        }
     }
    //cout << "end file"<<endl;
}



void numberposatadmisison(int *numberpos,int *numberneg,vector <int> & admissionstate,vector <int> & admissionday,int startperiod, int endperiod)
{
  unsigned int patient;
  int day;
  *numberpos=0;
  *numberneg=0;
  for(patient=0;patient<admissionstate.size();patient++)
    {
     day=admissionday.at(patient);
     if(day>=startperiod&&day<endperiod)
        {
          if(admissionstate.at(patient)==1){*numberpos+=1;}
          else{*numberneg+=1;}
        }
    }
}
