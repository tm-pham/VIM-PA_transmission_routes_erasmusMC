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
                    vector <int> & admissionday, int multipleparameters)
{
  if(multipleparameters==0)
  {

  unsigned int day,culture,patient;
  int day2,day3,unit,pat,moredaysinfected=-10, culturenumber;//,startchange=-1,endchange=-1;
  int startchangeculture=-1,endchangeculture=-1,startchangeFI=-1,endchangeFI=-1;
  if(oldadmissionstate==1&&newadmissionstate==1)
     {
      //do nothing
     }

  if(oldadmissionstate==0&&newadmissionstate==1)
     {
      *logl+=log(param.f)-log(1.-param.f);
      if(oldacquisition==0)
        {
         // startchange=admissionday.at(patid);
         // endchange=admissionday.at(patid)+whereabouts.at(patid).size()-1;

          startchangeculture=admissionday.at(patid);
          endchangeculture=admissionday.at(patid)+whereabouts.at(patid).size();
          startchangeFI=startchangeculture;
          endchangeFI=endchangeculture-1;

          moredaysinfected=1;
              //colstatus: -1, already colonized, no acquisition during that day, 1 acquisition during that day.
          for(day=0;day<whereabouts.at(patid).size();day++)
             {
              unit=whereabouts.at(patid).at(day);
              day3=admissionday.at(patid)+day;
              if(day3<endperiod)*logl-=logpnoacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3));
              I.at(unit).at(day3)+=1;
              colstatus.at(patid).at(day)=-1;
             }
        }
      else//oldacquisition==1
        {
          //startchange=admissionday.at(patid);
          //endchange=admissionday.at(patid)+oldacquisitionday;

          startchangeculture=admissionday.at(patid);
          endchangeculture=admissionday.at(patid)+oldacquisitionday;
          startchangeFI=startchangeculture;
          endchangeFI=endchangeculture;

          moredaysinfected=1;
          for(day2=0;day2<oldacquisitionday;day2++)
             {
              unit=whereabouts.at(patid).at(day2);
              day3=admissionday.at(patid)+day2;
             if(day3<endperiod)*logl-=logpnoacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3));
              I.at(unit).at(day3)+=1;
              colstatus.at(patid).at(day2)=-1;
             }
          unit=whereabouts.at(patid).at(oldacquisitionday);
          day3=admissionday.at(patid)+oldacquisitionday;
          if(day3<endperiod)*logl-=logpacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3));
          I.at(unit).at(day3)+=1;
          colstatus.at(patid).at(oldacquisitionday)=-1;
          numberofacquisitions.at(unit).at(day3)-=1;
        }
     }
  if(oldadmissionstate==1&&newadmissionstate==0)
     {
      *logl+=log(1.-param.f)-log(param.f);
      if(newacquisition==0)
         {
           //startchange=admissionday.at(patid);
           //endchange=admissionday.at(patid)+whereabouts.at(patid).size()-1;


          startchangeculture=admissionday.at(patid);
          endchangeculture=admissionday.at(patid)+whereabouts.at(patid).size();
          startchangeFI=startchangeculture;
          endchangeFI=endchangeculture-1;

           moredaysinfected=0;
           //colstatus: -1, already colonized, 0 no acquisition during that day, 1 acquisition during that day.
           for(day=0;day<whereabouts.at(patid).size();day++)
             {
               unit=whereabouts.at(patid).at(day);
               day3=admissionday.at(patid)+day;
               I.at(unit).at(day3)-=1;
               if(day3<endperiod)*logl+=logpnoacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3));
               colstatus.at(patid).at(day)=0;
             }
         }
       else//newacquisition==1
         {
          // cout<<"newacquisition"<<endl;
           //startchange=admissionday.at(patid);
           //endchange=admissionday.at(patid)+newacquisitionday;


          startchangeculture=admissionday.at(patid);
          endchangeculture=admissionday.at(patid)+newacquisitionday;
          startchangeFI=startchangeculture;
          endchangeFI=endchangeculture;


           moredaysinfected=0;
           for(day2=0;day2<newacquisitionday;day2++)
             {
              unit=whereabouts.at(patid).at(day2);
              day3=admissionday.at(patid)+day2;
              I.at(unit).at(day3)-=1;
              if(day3<endperiod)*logl+=logpnoacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3));
              colstatus.at(patid).at(day2)=0;
             }
           colstatus.at(patid).at(newacquisitionday)=1;
           unit=whereabouts.at(patid).at(newacquisitionday);
           day3=admissionday.at(patid)+newacquisitionday;
           I.at(unit).at(day3)-=1;
           if(day3<endperiod)*logl+=logpacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3));
           numberofacquisitions.at(unit).at(day3)+=1;
         }
     }
  if(oldadmissionstate==0&&newadmissionstate==0)
     {
       if(newacquisition==0&&oldacquisition==0)
         {
          // do nothing
         }
       if(newacquisition==1&&oldacquisition==0)
         {
          //startchange=admissionday.at(patid)+newacquisitionday+1;
          //endchange=admissionday.at(patid)+whereabouts.at(patid).size()-1;


          startchangeculture=admissionday.at(patid)+newacquisitionday+1;
          endchangeculture=admissionday.at(patid)+whereabouts.at(patid).size();
          startchangeFI=startchangeculture;
          endchangeFI=endchangeculture-1;


          moredaysinfected=1;
          //colstatus: -1, already colonized, 0 no acquisition during that day, 1 acquisition during that day.
          for(day=newacquisitionday+1;day<whereabouts.at(patid).size();day++)
             {
              unit=whereabouts.at(patid).at(day);
              day3=admissionday.at(patid)+day;
              if(day3<endperiod)*logl-=logpnoacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3));
              //cout << "in changeloglandI: "<<logpnoacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3))<<endl;
              I.at(unit).at(day3)+=1;
              colstatus.at(patid).at(day)=-1;
             }
          unit=whereabouts.at(patid).at(newacquisitionday);
          day3=admissionday.at(patid)+newacquisitionday;
          colstatus.at(patid).at(newacquisitionday)=1;
          numberofacquisitions.at(unit).at(day3)+=1;

          if(day3<endperiod)*logl+=logpacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3))-logpnoacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3));
           //cout << "in changeloglandI: "<<logpacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3))-logpnoacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3))<<endl;
         }
       if(newacquisition==0&&oldacquisition==1)
         {
          //startchange=admissionday.at(patid)+oldacquisitionday+1;
          //endchange=admissionday.at(patid)+whereabouts.at(patid).size()-1;


          startchangeculture=admissionday.at(patid)+oldacquisitionday+1;
          endchangeculture=admissionday.at(patid)+whereabouts.at(patid).size();
          startchangeFI=startchangeculture;
          endchangeFI=endchangeculture-1;


          moredaysinfected=0;
          //colstatus: -1, already colonized, 0 no acquisition during that day, 1 acquisition during that day.
          for(day=oldacquisitionday+1;day<whereabouts.at(patid).size();day++)
             {
              unit=whereabouts.at(patid).at(day);
              day3=  admissionday.at(patid)+day;
              I.at(unit).at(day3)-=1;
              if(day3<endperiod)*logl+=logpnoacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3));
              colstatus.at(patid).at(day)=0;
             }
          colstatus.at(patid).at(oldacquisitionday)=0;
          unit=whereabouts.at(patid).at(oldacquisitionday);
          day3=admissionday.at(patid)+oldacquisitionday;
          numberofacquisitions.at(unit).at(day3)-=1;
          if(day3<endperiod)*logl+=logpnoacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3))-logpacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3));
         }
       if(newacquisition==1&&oldacquisition==1)
         {
           if(oldacquisitionday>newacquisitionday)
             {
                 //startchange=admissionday.at(patid)+newacquisitionday+1;
                 //endchange=admissionday.at(patid)+oldacquisitionday;

                 startchangeculture=admissionday.at(patid)+newacquisitionday+1;
                 endchangeculture=admissionday.at(patid)+oldacquisitionday;
                 startchangeFI=startchangeculture;
                 endchangeFI=endchangeculture;


                 moredaysinfected=1;
                 //colstatus: -1, already colonized, 0 no acquisition during that day, 1 acquisition during that day.
                 for(day2=newacquisitionday+1;day2<oldacquisitionday;day2++)
                    {
                      unit=whereabouts.at(patid).at(day2);
                      day3=admissionday.at(patid)+day2;
                      if(day3<endperiod)*logl-=logpnoacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3));
                      I.at(unit).at(day3)+=1;
                      colstatus.at(patid).at(day2)=-1;
                    }

                 unit=whereabouts.at(patid).at(newacquisitionday);
                 day3=admissionday.at(patid)+newacquisitionday;
                 colstatus.at(patid).at(newacquisitionday)=1;
                 if(day3<endperiod)*logl+=logpacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3))-logpnoacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3));

                 unit=whereabouts.at(patid).at(oldacquisitionday);
                 day3=admissionday.at(patid)+oldacquisitionday;
                 colstatus.at(patid).at(oldacquisitionday)=-1;
                 if(day3<endperiod)*logl-=logpacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3));
                  I.at(unit).at(day3)+=1;
             }
          // if(oldacquisitionday==newacquisitionday)  {startchange=0;endchange=-1;}
           if(oldacquisitionday<newacquisitionday)

           //else//(oldacquisitionday<=newacquisitionday)
             {
                 //startchange=admissionday.at(patid)+oldacquisitionday+1;
                 //endchange=admissionday.at(patid)+newacquisitionday;

                 startchangeculture=admissionday.at(patid)+oldacquisitionday+1;
                 endchangeculture=admissionday.at(patid)+newacquisitionday;
                 startchangeFI=startchangeculture;
                 endchangeFI=endchangeculture;


                 moredaysinfected=0;
                  //colstatus: -1, already colonized, 0 no acquisition during that day, 1 acquisition during that day.
                 for(day2=oldacquisitionday+1;day2<newacquisitionday;day2++)
                    {
                      unit=  whereabouts.at(patid).at(day2);
                      day3=  admissionday.at(patid)+day2;
                      I.at(unit).at(day3)-=1;
                      colstatus.at(patid).at(day2)=0;
                      if(day3<endperiod)*logl+=logpnoacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3));
                    }
                 unit=whereabouts.at(patid).at(newacquisitionday);
                 day3=admissionday.at(patid)+newacquisitionday;
                 colstatus.at(patid).at(newacquisitionday)=1;
                 I.at(unit).at(day3)-=1;
                 if(day3<endperiod)*logl+=logpacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3));

                 unit=whereabouts.at(patid).at(oldacquisitionday);
                 day3=admissionday.at(patid)+oldacquisitionday;
                 colstatus.at(patid).at(oldacquisitionday)=0;
                 if(day3<endperiod)*logl+=logpnoacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3))-logpacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3));

             }
           numberofacquisitions.at(whereabouts.at(patid).at(oldacquisitionday)).at(admissionday.at(patid)+oldacquisitionday)-=1;
           numberofacquisitions.at(whereabouts.at(patid).at(newacquisitionday)).at(admissionday.at(patid)+newacquisitionday)+=1;
         }
     }

//change logl due to cultures performed at changing patient
for(culture=0;culture<cultureperpatient.at(patid).size();culture++)
             {
              culturenumber=cultureperpatient.at(patid).at(culture);
              if(culturearray.at(culturenumber).at(1)>=max(startperiod,startchangeculture)&&culturearray.at(culturenumber).at(1)<=min(endperiod,endchangeculture))

                      {
                       // colonization status of patient was changed during each culture
                       if(culturearray.at(cultureperpatient.at(patid).at(culture)).at(2)==0)
                         {
                           if( moredaysinfected==1)
                          // before change, patient was negative and negative culture
                          // after change, patient was positive and negative culture
                             {
                                 *logl+=log(1.0-param.phi);
                                 //cout <<log(1.0-param.phi)<<endl;
                             }
                           if(moredaysinfected==0)
                           // before change, patient was positive and negative culture
                          // after change, patient was negative and negative culture
                             {
                                 *logl-=log(1.0-param.phi);
                             }
                         }
                        else//culture positive
                         {
                            if( moredaysinfected==1)
                           // before change, patient was negative and positive culture
                           // after change, patient was positive and positive culture
                             {
                                 *logl+=log(param.phi)+1000000.0;
                             }
                          if( moredaysinfected==0)
                           // before change, patient was positive and positive culture
                           // after change, patient was negative and positive culture
                            {
                              *logl-=1000000.0+log(param.phi);
                            }
                         }
                      }
             }

 // change likelihood for patients exposed to a different force of infection.
          for(day2=max(startperiod,startchangeFI);day2<=min(endperiod-1,endchangeFI);day2++)
             {
              unit=whereabouts.at(patid).at(day2-admissionday.at(patid));
              for(patient=0;patient<present.at(unit).at(day2).size();patient++)

                 {
                    pat=present.at(unit).at(day2).at(patient);
                    if(pat!=patid)
                      {//only effects patients other than the patients whose colonization status changed
                      //-1, already colonized, no acquisition during that day, 1 acquisition during that day.
                       if(colstatus.at(pat).at(day2-admissionday.at(pat))==1)
                         {
                            if( moredaysinfected==0)
                            {
                            *logl+=logpacquisition(param,I.at(unit).at(day2),N.at(unit).at(day2))-
                                   logpacquisition(param,I.at(unit).at(day2)+1,N.at(unit).at(day2));

                            }
                            if( moredaysinfected==1)
                             {
                                *logl+=logpacquisition(param,I.at(unit).at(day2),N.at(unit).at(day2))-
                                       logpacquisition(param,I.at(unit).at(day2)-1,N.at(unit).at(day2));
                             }
                         }
                       if(colstatus.at(pat).at(day2-admissionday.at(pat))==0)
                         {
                                if( moredaysinfected==0)
                            {
                              *logl+=logpnoacquisition(param,I.at(unit).at(day2),N.at(unit).at(day2))-
                                      logpnoacquisition(param,I.at(unit).at(day2)+1,N.at(unit).at(day2));
                            }
                              if( moredaysinfected==1)
                            {
                              *logl+=logpnoacquisition(param,I.at(unit).at(day2),N.at(unit).at(day2))-
                                      logpnoacquisition(param,I.at(unit).at(day2)-1,N.at(unit).at(day2));
                               // cout<<"qqq: "<<  logpnoacquisition(param,I.at(unit).at(day2),N.at(unit).at(day2))-
                                 //     logpnoacquisition(param,I.at(unit).at(day2)-1,N.at(unit).at(day2))<<endl;
                            }
                         }
                      }
                 }
             }
  }
 else//multpleparameters==1
 {
   unsigned int day,culture,patient;
  int day2,day3,unit,pat,moredaysinfected=-10, culturenumber;//,startchange=-1,endchange=-1;
  int startchangeculture=-1,endchangeculture=-1,startchangeFI=-1,endchangeFI=-1;
  if(oldadmissionstate==1&&newadmissionstate==1)
     {
      //do nothing
     }

  if(oldadmissionstate==0&&newadmissionstate==1)
     {
      *logl+=log(param.f)-log(1.-param.f);
      if(oldacquisition==0)
        {
         // startchange=admissionday.at(patid);
         // endchange=admissionday.at(patid)+whereabouts.at(patid).size()-1;

          startchangeculture=admissionday.at(patid);
          endchangeculture=admissionday.at(patid)+whereabouts.at(patid).size();
          startchangeFI=startchangeculture;
          endchangeFI=endchangeculture-1;

          moredaysinfected=1;
              //colstatus: -1, already colonized, no acquisition during that day, 1 acquisition during that day.
          for(day=0;day<whereabouts.at(patid).size();day++)
             {
              unit=whereabouts.at(patid).at(day);
              day3=admissionday.at(patid)+day;
              if(day3<endperiod)*logl-=logpnoacquisitionward(param,I.at(unit).at(day3),N.at(unit).at(day3),unit);
              I.at(unit).at(day3)+=1;
              colstatus.at(patid).at(day)=-1;
             }
        }
      else//oldacquisition==1
        {
          //startchange=admissionday.at(patid);
          //endchange=admissionday.at(patid)+oldacquisitionday;

          startchangeculture=admissionday.at(patid);
          endchangeculture=admissionday.at(patid)+oldacquisitionday;
          startchangeFI=startchangeculture;
          endchangeFI=endchangeculture;

          moredaysinfected=1;
          for(day2=0;day2<oldacquisitionday;day2++)
             {
              unit=whereabouts.at(patid).at(day2);
              day3=admissionday.at(patid)+day2;
             if(day3<endperiod)*logl-=logpnoacquisitionward(param,I.at(unit).at(day3),N.at(unit).at(day3),unit);
              I.at(unit).at(day3)+=1;
              colstatus.at(patid).at(day2)=-1;
             }
          unit=whereabouts.at(patid).at(oldacquisitionday);
          day3=admissionday.at(patid)+oldacquisitionday;
          if(day3<endperiod)*logl-=logpacquisitionward(param,I.at(unit).at(day3),N.at(unit).at(day3),unit);
          I.at(unit).at(day3)+=1;
          colstatus.at(patid).at(oldacquisitionday)=-1;
          numberofacquisitions.at(unit).at(day3)-=1;
        }
     }
  if(oldadmissionstate==1&&newadmissionstate==0)
     {
      *logl+=log(1.-param.f)-log(param.f);
      if(newacquisition==0)
         {
           //startchange=admissionday.at(patid);
           //endchange=admissionday.at(patid)+whereabouts.at(patid).size()-1;


          startchangeculture=admissionday.at(patid);
          endchangeculture=admissionday.at(patid)+whereabouts.at(patid).size();
          startchangeFI=startchangeculture;
          endchangeFI=endchangeculture-1;

           moredaysinfected=0;
           //colstatus: -1, already colonized, 0 no acquisition during that day, 1 acquisition during that day.
           for(day=0;day<whereabouts.at(patid).size();day++)
             {
               unit=whereabouts.at(patid).at(day);
               day3=admissionday.at(patid)+day;
               I.at(unit).at(day3)-=1;
               if(day3<endperiod)*logl+=logpnoacquisitionward(param,I.at(unit).at(day3),N.at(unit).at(day3),unit);
               colstatus.at(patid).at(day)=0;
             }
         }
       else//newacquisition==1
         {
          // cout<<"newacquisition"<<endl;
           //startchange=admissionday.at(patid);
           //endchange=admissionday.at(patid)+newacquisitionday;


          startchangeculture=admissionday.at(patid);
          endchangeculture=admissionday.at(patid)+newacquisitionday;
          startchangeFI=startchangeculture;
          endchangeFI=endchangeculture;


           moredaysinfected=0;
           for(day2=0;day2<newacquisitionday;day2++)
             {
              unit=whereabouts.at(patid).at(day2);
              day3=admissionday.at(patid)+day2;
              I.at(unit).at(day3)-=1;
              if(day3<endperiod)*logl+=logpnoacquisitionward(param,I.at(unit).at(day3),N.at(unit).at(day3),unit);
              colstatus.at(patid).at(day2)=0;
             }
           colstatus.at(patid).at(newacquisitionday)=1;
           unit=whereabouts.at(patid).at(newacquisitionday);
           day3=admissionday.at(patid)+newacquisitionday;
           I.at(unit).at(day3)-=1;
           if(day3<endperiod)*logl+=logpacquisitionward(param,I.at(unit).at(day3),N.at(unit).at(day3),unit);
           numberofacquisitions.at(unit).at(day3)+=1;
         }
     }
  if(oldadmissionstate==0&&newadmissionstate==0)
     {
       if(newacquisition==0&&oldacquisition==0)
         {
          // do nothing
         }
       if(newacquisition==1&&oldacquisition==0)
         {
          //startchange=admissionday.at(patid)+newacquisitionday+1;
          //endchange=admissionday.at(patid)+whereabouts.at(patid).size()-1;


          startchangeculture=admissionday.at(patid)+newacquisitionday+1;
          endchangeculture=admissionday.at(patid)+whereabouts.at(patid).size();
          startchangeFI=startchangeculture;
          endchangeFI=endchangeculture-1;


          moredaysinfected=1;
          //colstatus: -1, already colonized, 0 no acquisition during that day, 1 acquisition during that day.
          for(day=newacquisitionday+1;day<whereabouts.at(patid).size();day++)
             {
              unit=whereabouts.at(patid).at(day);
              day3=admissionday.at(patid)+day;
              if(day3<endperiod)*logl-=logpnoacquisitionward(param,I.at(unit).at(day3),N.at(unit).at(day3),unit);
              //cout << "in changeloglandI: "<<logpnoacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3))<<endl;
              I.at(unit).at(day3)+=1;
              colstatus.at(patid).at(day)=-1;
             }
          unit=whereabouts.at(patid).at(newacquisitionday);
          day3=admissionday.at(patid)+newacquisitionday;
          colstatus.at(patid).at(newacquisitionday)=1;
          numberofacquisitions.at(unit).at(day3)+=1;

          if(day3<endperiod)*logl+=logpacquisitionward(param,I.at(unit).at(day3),N.at(unit).at(day3),unit)-logpnoacquisitionward(param,I.at(unit).at(day3),N.at(unit).at(day3),unit);
           //cout << "in changeloglandI: "<<logpacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3))-logpnoacquisition(param,I.at(unit).at(day3),N.at(unit).at(day3))<<endl;
         }
       if(newacquisition==0&&oldacquisition==1)
         {
          //startchange=admissionday.at(patid)+oldacquisitionday+1;
          //endchange=admissionday.at(patid)+whereabouts.at(patid).size()-1;


          startchangeculture=admissionday.at(patid)+oldacquisitionday+1;
          endchangeculture=admissionday.at(patid)+whereabouts.at(patid).size();
          startchangeFI=startchangeculture;
          endchangeFI=endchangeculture-1;


          moredaysinfected=0;
          //colstatus: -1, already colonized, 0 no acquisition during that day, 1 acquisition during that day.
          for(day=oldacquisitionday+1;day<whereabouts.at(patid).size();day++)
             {
              unit=whereabouts.at(patid).at(day);
              day3=  admissionday.at(patid)+day;
              I.at(unit).at(day3)-=1;
              if(day3<endperiod)*logl+=logpnoacquisitionward(param,I.at(unit).at(day3),N.at(unit).at(day3),unit);
              colstatus.at(patid).at(day)=0;
             }
          colstatus.at(patid).at(oldacquisitionday)=0;
          unit=whereabouts.at(patid).at(oldacquisitionday);
          day3=admissionday.at(patid)+oldacquisitionday;
          numberofacquisitions.at(unit).at(day3)-=1;
          if(day3<endperiod)*logl+=logpnoacquisitionward(param,I.at(unit).at(day3),N.at(unit).at(day3),unit)-logpacquisitionward(param,I.at(unit).at(day3),N.at(unit).at(day3),unit);
         }
       if(newacquisition==1&&oldacquisition==1)
         {
           if(oldacquisitionday>newacquisitionday)
             {
                 //startchange=admissionday.at(patid)+newacquisitionday+1;
                 //endchange=admissionday.at(patid)+oldacquisitionday;

                 startchangeculture=admissionday.at(patid)+newacquisitionday+1;
                 endchangeculture=admissionday.at(patid)+oldacquisitionday;
                 startchangeFI=startchangeculture;
                 endchangeFI=endchangeculture;


                 moredaysinfected=1;
                 //colstatus: -1, already colonized, 0 no acquisition during that day, 1 acquisition during that day.
                 for(day2=newacquisitionday+1;day2<oldacquisitionday;day2++)
                    {
                      unit=whereabouts.at(patid).at(day2);
                      day3=admissionday.at(patid)+day2;
                      if(day3<endperiod)*logl-=logpnoacquisitionward(param,I.at(unit).at(day3),N.at(unit).at(day3),unit);
                      I.at(unit).at(day3)+=1;
                      colstatus.at(patid).at(day2)=-1;
                    }

                 unit=whereabouts.at(patid).at(newacquisitionday);
                 day3=admissionday.at(patid)+newacquisitionday;
                 colstatus.at(patid).at(newacquisitionday)=1;
                 if(day3<endperiod)*logl+=logpacquisitionward(param,I.at(unit).at(day3),N.at(unit).at(day3),unit)-logpnoacquisitionward(param,I.at(unit).at(day3),N.at(unit).at(day3),unit);

                 unit=whereabouts.at(patid).at(oldacquisitionday);
                 day3=admissionday.at(patid)+oldacquisitionday;
                 colstatus.at(patid).at(oldacquisitionday)=-1;
                 if(day3<endperiod)*logl-=logpacquisitionward(param,I.at(unit).at(day3),N.at(unit).at(day3),unit);
                  I.at(unit).at(day3)+=1;
             }
          // if(oldacquisitionday==newacquisitionday)  {startchange=0;endchange=-1;}
           if(oldacquisitionday<newacquisitionday)

           //else//(oldacquisitionday<=newacquisitionday)
             {
                 //startchange=admissionday.at(patid)+oldacquisitionday+1;
                 //endchange=admissionday.at(patid)+newacquisitionday;

                 startchangeculture=admissionday.at(patid)+oldacquisitionday+1;
                 endchangeculture=admissionday.at(patid)+newacquisitionday;
                 startchangeFI=startchangeculture;
                 endchangeFI=endchangeculture;


                 moredaysinfected=0;
                  //colstatus: -1, already colonized, 0 no acquisition during that day, 1 acquisition during that day.
                 for(day2=oldacquisitionday+1;day2<newacquisitionday;day2++)
                    {
                      unit=  whereabouts.at(patid).at(day2);
                      day3=  admissionday.at(patid)+day2;
                      I.at(unit).at(day3)-=1;
                      colstatus.at(patid).at(day2)=0;
                      if(day3<endperiod)*logl+=logpnoacquisitionward(param,I.at(unit).at(day3),N.at(unit).at(day3),unit);
                    }
                 unit=whereabouts.at(patid).at(newacquisitionday);
                 day3=admissionday.at(patid)+newacquisitionday;
                 colstatus.at(patid).at(newacquisitionday)=1;
                 I.at(unit).at(day3)-=1;
                 if(day3<endperiod)*logl+=logpacquisitionward(param,I.at(unit).at(day3),N.at(unit).at(day3),unit);

                 unit=whereabouts.at(patid).at(oldacquisitionday);
                 day3=admissionday.at(patid)+oldacquisitionday;
                 colstatus.at(patid).at(oldacquisitionday)=0;
                 if(day3<endperiod)*logl+=logpnoacquisitionward(param,I.at(unit).at(day3),N.at(unit).at(day3),unit)-logpacquisitionward(param,I.at(unit).at(day3),N.at(unit).at(day3),unit);

             }
           numberofacquisitions.at(whereabouts.at(patid).at(oldacquisitionday)).at(admissionday.at(patid)+oldacquisitionday)-=1;
           numberofacquisitions.at(whereabouts.at(patid).at(newacquisitionday)).at(admissionday.at(patid)+newacquisitionday)+=1;
         }
     }

//change logl due to cultures performed at changing patient
for(culture=0;culture<cultureperpatient.at(patid).size();culture++)
             {
              culturenumber=cultureperpatient.at(patid).at(culture);
              if(culturearray.at(culturenumber).at(1)>=max(startperiod,startchangeculture)&&culturearray.at(culturenumber).at(1)<=min(endperiod,endchangeculture))

                      {
                       // colonization status of patient was changed during each culture
                       if(culturearray.at(cultureperpatient.at(patid).at(culture)).at(2)==0)
                         {
                           if( moredaysinfected==1)
                          // before change, patient was negative and negative culture
                          // after change, patient was positive and negative culture
                             {
                                 *logl+=log(1.0-param.phi);
                                 //cout <<log(1.0-param.phi)<<endl;
                             }
                           if(moredaysinfected==0)
                           // before change, patient was positive and negative culture
                          // after change, patient was negative and negative culture
                             {
                                 *logl-=log(1.0-param.phi);
                             }
                         }
                        else//culture positive
                         {
                            if( moredaysinfected==1)
                           // before change, patient was negative and positive culture
                           // after change, patient was positive and positive culture
                             {
                                 *logl+=log(param.phi)+1000000.0;
                             }
                          if( moredaysinfected==0)
                           // before change, patient was positive and positive culture
                           // after change, patient was negative and positive culture
                            {
                              *logl-=1000000.0+log(param.phi);
                            }
                         }
                      }
             }

 // change likelihood for patients exposed to a different force of infection.
          for(day2=max(startperiod,startchangeFI);day2<=min(endperiod-1,endchangeFI);day2++)
             {
              unit=whereabouts.at(patid).at(day2-admissionday.at(patid));
              for(patient=0;patient<present.at(unit).at(day2).size();patient++)

                 {
                    pat=present.at(unit).at(day2).at(patient);
                    if(pat!=patid)
                      {//only effects patients other than the patients whose colonization status changed
                      //-1, already colonized, no acquisition during that day, 1 acquisition during that day.
                       if(colstatus.at(pat).at(day2-admissionday.at(pat))==1)
                         {
                            if( moredaysinfected==0)
                            {
                            *logl+=logpacquisitionward(param,I.at(unit).at(day2),N.at(unit).at(day2),unit)-
                                   logpacquisitionward(param,I.at(unit).at(day2)+1,N.at(unit).at(day2),unit);

                            }
                            if( moredaysinfected==1)
                             {
                                *logl+=logpacquisitionward(param,I.at(unit).at(day2),N.at(unit).at(day2),unit)-
                                       logpacquisitionward(param,I.at(unit).at(day2)-1,N.at(unit).at(day2),unit);
                             }
                         }
                       if(colstatus.at(pat).at(day2-admissionday.at(pat))==0)
                         {
                                if( moredaysinfected==0)
                            {
                              *logl+=logpnoacquisitionward(param,I.at(unit).at(day2),N.at(unit).at(day2),unit)-
                                      logpnoacquisitionward(param,I.at(unit).at(day2)+1,N.at(unit).at(day2),unit);
                            }
                              if( moredaysinfected==1)
                            {
                              *logl+=logpnoacquisitionward(param,I.at(unit).at(day2),N.at(unit).at(day2),unit)-
                                      logpnoacquisitionward(param,I.at(unit).at(day2)-1,N.at(unit).at(day2),unit);
                               // cout<<"qqq: "<<  logpnoacquisition(param,I.at(unit).at(day2),N.at(unit).at(day2))-
                                 //     logpnoacquisition(param,I.at(unit).at(day2)-1,N.at(unit).at(day2))<<endl;
                            }
                         }
                      }
                 }
             }
 }


}


