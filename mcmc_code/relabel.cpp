#include <iostream>
#include <vector>
#include <algorithm>
#include "readcsvfileto2Darrayofint.h"
using namespace std;

bool sortFunc( const vector<int>& p1,
               const vector<int>& p2 ) {
 return p1.at(2) < p2.at(2);
}


struct myclass {
  bool operator() (vector<int> k,vector<int> l) { return (k.at(0)<l.at(0));}
} myobject2;


void relabeldates(vector< vector<int> >& admissionarray,vector< vector<int> >& culturearray,int *maxdate)
{
 unsigned int patientday,culture;
 int smallestdate=admissionarray.at(0).at(1);
 *maxdate=admissionarray.at(0).at(1);
 for(patientday=1;patientday<admissionarray.size();patientday++)
    {
     if(admissionarray.at(patientday).at(1)<smallestdate) smallestdate=admissionarray.at(patientday).at(1);
     if(admissionarray.at(patientday).at(1)>*maxdate) *maxdate=admissionarray.at(patientday).at(1);
    }
 *maxdate-=smallestdate-1;// day i runs from i to i+1
 cout<< "smallest date =" << smallestdate << endl;
 for(patientday=0;patientday<admissionarray.size();patientday++)
    {
     //cout << "admission before relabeling=" << admissionarray.at(patientday).at(1) << endl;	
     admissionarray.at(patientday).at(1)-=smallestdate;
     //cout << "admission after relabeling="<<admissionarray.at(patientday).at(1) << endl;
    }
  for(culture=0;culture<culturearray.size();culture++)
    {
     culturearray.at(culture).at(1)-=smallestdate;
     if(culturearray.at(culture).at(1)<0) cout << "culture before first admission"<< "\n";
    }

}



void relabelpatientID(vector< vector<int> >& admissionarray,vector< vector<int> >& culturearray,int *numberofpatients)
{
 std::sort (admissionarray.begin(), admissionarray.end(), myobject2);
 vector<int> v;
 v.push_back (0);//first patient is given label 0;
 *numberofpatients=1;
 unsigned int patientday=1,culture;
 while(patientday<admissionarray.size())
      {
       if(admissionarray.at(patientday).at(0)==admissionarray.at(patientday-1).at(0))
         {
          v.push_back(v.at(v.size()-1));
         }
       else
          {
             v.push_back(v.at(v.size()-1)+1);
             *numberofpatients+=1;
          }
       //if(*numberofpatients==745)cout<<"original ID 745="<<admissionarray.at(patientday).at(0)<<endl;
       patientday++;
      }
int match;
for(culture=0;culture<culturearray.size();culture++)
    {
     match=0;
     for(patientday=0;patientday<admissionarray.size();patientday++)
        {
         if(culturearray.at(culture).at(0)==admissionarray.at(patientday).at(0))
           {
            culturearray.at(culture).at(0)=v.at(patientday);
            match=1;
            break;
           }
        }
     if(match==0)cout <<"no admission data for patient: cnr="<<culture<<" \n";
    }


for(patientday=0;patientday<v.size();patientday++){
    admissionarray.at(patientday).at(0)=v.at(patientday);
    //cout << "v.at("<<patientday<<")="<<v.at(patientday)<<"\n";
  }
}


void relabelwards(vector< vector<int> >& array,int *numberofwards)
{
  unsigned int patientday;
  std::sort (array.begin(), array.end(), sortFunc);
  vector<int> v;
  v.push_back (0);//first ward is given label 0;
  patientday=1;
  *numberofwards=1;
  
 while(patientday<array.size())
      {
       if(array.at(patientday).at(2)==array.at(patientday-1).at(2))
         {
          v.push_back(v.at(v.size()-1));
         }
       else
          {
             v.push_back(v.at(v.size()-1)+1);
             *numberofwards+=1;
          }
       patientday++;
      }

 for(patientday=0;patientday<v.size();patientday++)
  {
    array.at(patientday).at(2)=v.at(patientday);
  }
}
