#include <string>
#include <sstream>
#include <iostream>
#include <vector>
#include <fstream>
#include <time.h>
#include "stdhdr.h"
#include "readcsvfileto2Darrayofint.h"
#include "relabel.h"
#include "pacquisition.h"
#include "loglikelihood.h"
#include "changeloglandI.h"
#include "createusefuldata.h"
#include "truepositiveandfalsenegative.h"
#include <boost/math/distributions/exponential.hpp>
#include <boost/math/distributions/uniform.hpp>
#include <boost/random/mersenne_twister.hpp>
#include <boost/math/special_functions/gamma.hpp>
#include "samplefromnormal.h"
#include "environment.h"
#include <time.h>

/***********************************************************************
 * MCMC SIMULATION FOR 
 * Background transmission,
 * Cross-transmission and
 * Environmental contamination after discharge
 ***********************************************************************/
using namespace std;

/***********************************************************************
 * SORTING function
 * sort based on second element
 **********************************************************************/
  bool sortFunc1( const vector<int>& p1,const vector<int>& p2 ){
    return p1.at(1) < p2.at(1);
  }

/***********************************************************************
 * MAIN function
 **********************************************************************/
  
  int main(){
	  cout << "Starting the Code."<<"\n";
  /**********************************************************************
   * DECLARE VARIABLES
   *********************************************************************/
    time_t timer = time(NULL);
  // extern int parallelism_enabled=0; // Multithread if = 1
  
  int inew;
  int numberimportations;
  double forceofinfection;
  int nchange=0;
  double error = 1e-11;
  
  /// First defined in PROCESSING INPUT DATA
  int numberofpatients,numberofwards,maxdate,unit;
  
  /// Declare variables for data augmentation
  int factoraugmented,changeaugmented,newfactoraugmented;
  
  /// Loglikelihood computation
  double oldlogl;
  
  int newadmissionstate,newacquisitionday,newacquisition,oldadmissionstate,oldacquisitionday,oldacquisition;
  
  int dummy,truepositive,falsenegative,posatadmission,negatadmission,event,change;
  double newa,newb,newlogl,p,r,x,y,q;//logp,
  double newc, newnu, newmu; 
  
  /// Auxiliary variable for data-augmentation step
  int thesize,donothing,index,i,ward;
  
  cout << "Before first vector declaration \n"; 
  /// Multiple wards
  vector<double> newalpha(numberofwards),newbeta(numberofwards), newgamma(numberofwards), newnuvector(numberofwards), newmuvector(numberofwards);
  
  /// For computing relative contribution 
  int acqday; 
  double contralpha, contrbeta, contrE, contrCrossT, contrEnv;
  
  
  // For startline output
  int out = 0; 
  /**********************************************************************
   * DEFINE PARAMETERS FOR MCMC
   *********************************************************************/
  int thinning=10,thinningscreen=100000;
  int thinningfoi=100000;
  int onlyaugmented=0; /// if 1 only augmented data are updated
  /*frequencydependence=0 means density dependence,
   *frequencydependence=1 means frequency dependence */
  int frequencydependece =1;
  long int numberofupdates=2030000;
  
  /// parameter w = probability that new colonization time is an admission
  double w=0.2;
  
  /**********************************************************************
  * Initial standarddeviation proposal distribution for
  * alpha, beta
  * gamma(c), mu, nu, E0 (added to Martins code)
  **********************************************************************/
  double sigmaa=0.2; double sigmab=0.12; 
  double sigmac=0.15, sigmanu = 0.2, sigmamu=0.12, sigmaE0=0.2;
  // For adaptation of acceptance ratio for gamma in Hamiltonian MCMC
  double newsigmac = 0.12, newmuc = 0.0, propratio = 1.0, newmub=0.0, newsigmab=0.12;
  /* sigmaa and sigmab are modified such that acceptance rate more or
   * less between lowerboundacceptance and upperboundacceptance */
  double lowerboundacceptance=0.05;
  double upperboundacceptance=0.5;
  /*updatetochangesigma determines number of updates to check whether
   *acceptancerate for a and b are reasonable */
  int updatestochangesigma=100;
  
  /// Define seed for the random number generator
  int seed=40;
  /*******************************
   * change to
   * ENG eng(std::time(0));
   * to have random starting of chain
   *******************************/
  
  /**********************************************************************
   * Initial values of MCMC chain
   * --------------------------------------------------------------------
   * alpha = a, beta = b, importation rate = f, sensitivity = phi
   * epsilon = c, nu, mu, E0 (parameters for env. cont.)
   **********************************************************************/
  parameters param;
  param.a=0.01;
  param.b=0.01;
  param.f=0.0;
  param.phi=1.0;
  param.frequencydependence=frequencydependece;
  
  param.c=0.01;
  param.nu=0.1;
  param.mu=0.01;
  double meanPrev = 0.15;
  param.E0 = (param.nu/param.mu)*meanPrev;
  double foi = param.a + param.b*meanPrev+param.c*param.E0;
  
  
  /**********************************************************************
   * ADDED: Parameters for environmental contamination
   * --------------------------------------------------------------------
   * addingenvironment = 1 if environmental contamination should be added
   * to force of infection, 0 otherwise
   **********************************************************************/
  int addingenvironment = 0;
  int endogenous = 1, exogenous = 1;
  double newE0; double meanE;
  int day;
  
  /// Define startperiod and endperiod
  int startperiod=0; /// defined with first date set to day zero
  int endperiod=200; /// defined with first date set to day zero
  
  /// Added caccepted=0, nuaccepted, muaccepted
  /// counts accepted updates for alpha and beta
  long int aaccepted=0,baccepted=0,caccepted=0, nuaccepted=0, muaccepted=0;
  /**********************************************************************
   * Determine prior distributions
   * --------------------------------------------------------------------
   * Sensitivity test has as prior distribution a beta distribution with
   * parameters aa and bb.
   * Parameters endogenous route and cross transmisison (alpha and beta)
   * have as prior an exponential ditribution with parameter lambda
   *********************************************************************/
    double aa=1.,bb=1.,lambda=0.001, lambda2=1.0;
  
  /**********************************************************************
   * PRIOR DISTRIBUTION for
   *--------------------------------------------------------------------
   * alpha, beta
   * gamma, nu, mu, E0 (added to Martins code)
   *********************************************************************/
  boost::math::exponential_distribution<>prioralpha(lambda);  
  boost::math::exponential_distribution<>priorbeta(lambda);
  boost::math::exponential_distribution<>priorgamma(lambda);
  boost::math::exponential_distribution<>priornu(lambda);
  //boost::math::exponential_distribution<>priormu(lambda); string priormustring="Exponential(0.001)";
  boost::math::uniform_distribution<>priormu(0,2); string priormustring="U(0,2)";
  boost::math::exponential_distribution<>priorE0(lambda);
  
  /// Does each ward has its own parameters for alpha, beta or not?
  int multipleparameters=0;
  
  /**********************************************************************
   * INPUT DATA
   *********************************************************************/
  /*
  string env = ""; string icu = "AspireICU"; string hosp = "ArtifSim";
  string resultspath="/home/julius_id/tpham/"+hosp+"/Results/"+icu+"/lowSens/";
  string savefile = resultspath + "mytestresults" + icu + env + ".txt";
  string patientsfile = resultspath + "mypatients" + icu + env + ".txt";
  string foifile = resultspath + "myfoiresults" + icu + env + ".txt";
  string prevfile = resultspath + "myprevresults" + icu + env + ".txt";
  string impfile = resultspath + "myimpresults" + icu + env + ".txt";
  string Nfile = resultspath + "myNresults" + icu + env + ".txt";
  string acqfile = resultspath + "myacqresults" + icu + env + ".txt";
  string startline = resultspath + "mystartline" + icu + env + ".txt";
  string presentpatients = resultspath + "mypresent" + icu + env + ".txt";
  string patientsacqfile = resultspath + "mypatientsacq" + icu + env + ".txt";
  string colstatusfile = resultspath + "colstatus" + icu + env + ".txt";
  // Input File
  string datapath = "/home/julius_id/tpham/" + hosp + "/Data/" + icu + "/";
  string admissiondata = datapath + "admDates" + icu + "_lowSens.txt";
  string culturedata = datapath + "cultureresults" + icu + "_lowSens.txt";
  */

  /*
  string env = ""; string icu = "7121BeforeAfter"; string hosp = "Besancon";
  string resultspath="/home/julius_id/tpham/"+hosp+"Hospital/Results/7121/NoEnv/TC/";
  string savefile = resultspath + "mytestresults" + icu + env + ".txt";
  string patientsfile = resultspath + "mypatients" + icu + env + ".txt";
  string foifile = resultspath + "myfoiresults" + icu + env + ".txt";
  //string envfile = resultspath + "myenvresults" + icu + env + ".txt";
  //string envstayfile=resultspath + "myenvstayresults" + icu + env + ".txt";
  //string envdisfile=resultspath + "myenvdisresults" + icu + env + ".txt";
  string prevfile = resultspath + "myprevresults" + icu + env + ".txt";
  string impfile = resultspath + "myimpresults" + icu + env + ".txt";
  string Nfile = resultspath + "myNresults" + icu + env + ".txt";
  string acqfile = resultspath + "myacqresults" + icu + env + ".txt";
  string startline = resultspath + "mystartline" + icu + env + ".txt";
  string presentpatients = resultspath + "mypresent" + icu + env + ".txt";
  string patientsacqfile = resultspath + "mypatientsacq" + icu + env + ".txt";
  string colstatusfile = resultspath + "colstatus" + icu + env + ".txt";
  // Input File
  string datapath = "/home/julius_id/tpham/" + hosp + "Hospital/Data/";
  string admissiondata = datapath + "admDates" + hosp + icu + ".txt";
  string culturedata = datapath + "cultureresults" + hosp + icu + ".txt";
  */

  
  string icu = "2"; string hosp = "_with_VIM_SDD_";
  //string resultspath="/Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/Combacte/RotterdamProject/results/VIM_results/" + icu + "/";
  string resultspath="/home/julius_id/tpham/RotterdamProject/Results/Nonoutbreak" + icu + "/";
  string savefile = resultspath + "mytestresults" +hosp + icu + ".txt";
  string patientsfile = resultspath + "mypatients" +hosp + icu + ".txt";
  string foifile = resultspath + "myfoiresults" +hosp + icu + ".txt";
  string prevfile = resultspath + "myprevresults" +hosp + icu + ".txt";
  string impfile = resultspath + "myimpresults" +hosp + icu + ".txt";
  string Nfile = resultspath + "myNresults" +hosp + icu + ".txt";
  string acqfile = resultspath + "myacqresults" +hosp + icu + ".txt";
  string startline = resultspath + "mystartline" +hosp + icu + ".txt";
  string presentpatients = resultspath + "mypresent" +hosp + icu + ".txt";
  string patientsacqfile = resultspath + "mypatientsacq" +hosp + icu + ".txt";
  string colstatusfile = resultspath + "colstatus" +hosp + icu + ".txt";
  // Input File
  //string datapath = "/Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/Combacte/RotterdamProject/data/MCMC_data/";
  string datapath = "/home/julius_id/tpham/RotterdamProject/Data/";
  string admissiondata = datapath + "admDates" + hosp  + icu + ".txt";
  string culturedata = datapath + "cultureresults" + hosp  + icu + ".txt";


  /*
  string icu = "1"; string hosp = "NeoDeco";
  string resultspath="/home/julius_id/tpham/ArtifSim/Results/"+hosp+"/"+icu+"/";
  string savefile = resultspath + "mytestresults" +hosp + icu + ".txt";
  string patientsfile = resultspath + "mypatients" +hosp + icu + ".txt";
  string foifile = resultspath + "myfoiresults" +hosp + icu + ".txt";
  string prevfile = resultspath + "myprevresults" +hosp + icu + ".txt";
  string impfile = resultspath + "myimpresults" +hosp + icu + ".txt";
  string Nfile = resultspath + "myNresults" +hosp + icu + ".txt";
  string acqfile = resultspath + "myacqresults" +hosp + icu + ".txt";
  string startline = resultspath + "mystartline" +hosp + icu + ".txt";
  string presentpatients = resultspath + "mypresent" +hosp + icu + ".txt";
  string patientsacqfile = resultspath + "mypatientsacq" +hosp + icu + ".txt";
  string colstatusfile = resultspath + "colstatus" +hosp + icu + ".txt";
  // Input File
  string datapath = "/home/julius_id/tpham/ArtifSim/Data/ScreeningScheme/" + hosp + "/" +  icu + "/";
  string admissiondata = datapath + "admDates" + hosp + "_" + icu + ".txt";
  string culturedata = datapath + "cultureresults" + hosp + "_" + icu + ".txt";
  */
 
  
  /*
  string env = ""; string icu = "13"; string hosp = "Bedwise_";
  string resultspath="/home/julius_id/tpham/ArtifSim/Results/Bedwise/" + icu + "/BC/";
  string savefile = resultspath + "mytestresults" + hosp + icu + env + ".txt";
  string meanParam = resultspath + "meanParam" + hosp + icu + env + ".txt";
  string foifile = resultspath + "myfoiresults" + hosp + icu + env + ".txt";
  string envfile = resultspath + "myenvresults" + hosp + icu + env + ".txt";
  string envstayfile=resultspath + "myenvstayresults" + hosp + icu + env + ".txt";
  string envdisfile=resultspath + "myenvdisresults" + hosp + icu + env + ".txt";
  string prevfile = resultspath + "myprevresults" + hosp + icu + env + ".txt";
  string impfile = resultspath + "myimpresults" + hosp + icu + env + ".txt";
  string Nfile = resultspath + "myNresults" + hosp + icu + env + ".txt";
  string acqfile = resultspath + "myacqresults" + hosp + icu + env + ".txt";
  string startline = resultspath + "mystartline" + hosp + icu + env + ".txt";
  string patientsfile = resultspath + "mypatients" + hosp + icu + env + ".txt";
  string colstatusfile = resultspath + "colstatus" + hosp + icu + env + ".txt";
  // Input File
  string datapath = "/home/julius_id/tpham/ArtifSim/Data/Bedwise/";
  string admissiondata = datapath + "admDates" + hosp + icu + ".txt";
  string culturedata = datapath + "cultureresults" + hosp + icu + ".txt";
  string previouspatdata = datapath + "previousPat" + hosp + icu + ".txt";
  */
  
  cout << "Definition of input and output files \n"; 
  /**********************************************************************
   * PROCESSING INPUT DATA
   *********************************************************************/
    
  /**********************************************************************
  * culturedata is of form: patid, date, result
  * admissiondata is of form: patid, date, roomnumber
  * Assume rooms are labeled with integers
  * Dates are in integers
  * Patient ID are integers
  * day with the label i runs from day i: 12.00 -> day i+1, 11.59
  **********************************************************************/
  cout << "Admission File: " << admissiondata << endl; 
  cout << "Culture File: " << culturedata << endl;
  cout << "Number of updates: " << numberofupdates << endl;
  cout << "Mean prevalence = " << meanPrev << endl;
  
  vector< vector<int> > admissionarray,culturearray;
  READCSVFILETO2DARRAYOFINT(admissiondata,admissionarray);
  READCSVFILETO2DARRAYOFINT(culturedata,culturearray);
  
//  cout << "**************************************"<<endl;
//  cout << "Print admissionarray" << endl;
//  cout << "**************************************"<<endl;
//  printarray(admissionarray, admissiondata); 
//  cout << "**************************************"<<endl;
//  cout << "Print culturearray" << endl;
//  cout << "**************************************"<<endl;
//  printarray(culturearray,culturedata); 
  
  cout << "**************************************"<<endl;
  cout << "Processing data for MCMC simulation" << endl;
  cout << "**************************************"<<endl;
  /// OUTPUT: Print which routes are considered for simulation
  cout << "Processing data or MCMC simulation. " << endl;
  if(addingenvironment){
    cout << "Estimating parameters for endogenous route, cross-transmission AND environmental contamination." << endl;
    cout << "Prior mu: " << priormustring << endl;
  } else cout << "Estimating parameters for endogenous route and cross-transmission." << endl;
  
  cout << "Relabel wards" << endl;
  /// relabelwards: Wards are relabeled with integers: 0, 1,2,3,...,N-1
  relabelwards(admissionarray,&numberofwards);
  cout << "numberofpatients="<<numberofpatients<<", numberofwards="<<numberofwards<<", maxdate="<<maxdate<<"\n";
  /// relabelpatientID: Patient ID are relabeled: 0, 1, 2, 3, ...., M-1
  relabelpatientID(admissionarray,culturearray,&numberofpatients);
  cout << "numberofpatients="<<numberofpatients<<", numberofwards="<<numberofwards<<", maxdate="<<maxdate<<"\n";
  /// relabeldates: Dates are relabeled 0, 1, 2, ...., T-1
  relabeldates(admissionarray,culturearray,&maxdate);
  
  cout << "numberofpatients="<<numberofpatients<<", numberofwards="<<numberofwards<<", maxdate="<<maxdate<<"\n";
  cout <<"length culturearray="<<culturearray.size()<<endl;
  cout <<"length admissionarray="<<admissionarray.size()<<endl;
  
  /*factoraugmented: Number of augmentation steps
   *changed later on if numberofiteration > changeaugmented */
  factoraugmented=numberofpatients*10; 
  changeaugmented=2;
  newfactoraugmented=25; //newfactoraugmented=1+numberofpatients/4;
  cout <<"newfactoraugmented="<<newfactoraugmented<<endl;
  
  /// Initialize parameter vectors now since unit is defined
  for(unit=0;unit<numberofwards;unit++){
    (param.alphavector).push_back(param.a);
    (param.betavector).push_back(param.b);
    
    (param.gammavector).push_back(param.c);
    (param.nuvector).push_back(param.nu);
    (param.muvector).push_back(param.mu);
  }
  
  /// N = Number of patients per day
  vector<vector<int> > N(numberofwards, vector<int>(maxdate,0));
  /* whereabouts:
   * For each patient it includes a vector with days on which the patient
   * is present. Note whereabouts of patient i startperiod at
   * admissionday of i **/
  vector<vector<int> > whereabouts(numberofpatients);
  /* colstatus:
   * note colstatus of patient i start at admissionday of i
   * -1, already colonized,
   * 0,  no acquisition during that day,
   * 1,  acquisition during that day. **/
  vector<vector<int> > colstatus(numberofpatients);
  /// admissionday and dischargeday per patient
  vector<int> admissionday(numberofpatients,maxdate+10),dischargeday(numberofpatients,-1);
  std::sort (admissionarray.begin(), admissionarray.end(), sortFunc1);
  /* numberofpatientsperward:
   * Fill vectors N, whereabouts, admissionday, dischargeday according to observed data. */
  numberofpatientsperward(N,admissionarray,whereabouts,admissionday,dischargeday);
  /* present:
   * per ward, per day a list of which patients are present in the unit. */
  vector<vector<vector<int> > > present (numberofwards,vector<vector<int> >(maxdate,vector <int>()));
  whopresentwhen(present, admissionarray);
  
  /// Vector with patient Ids that are never, definitely or temporary positive
  vector<int> listneverpos,listdefpos,listtemppos;
  vector<vector<int> > cultureperpatient(numberofpatients);
  /// admissionstate: 0 uncolonized upon admission, 1 colonized upon admission 
  vector<int> admissionstate(numberofpatients, 1);
  /// acquisition: 0=no acquisition, 1 acquisition */
  vector<int> acquisition(numberofpatients,0);
  /// acquisitionday> acquisition is during that day (at end of that specific day (11.59 next day))
  vector<int> acquisitionday(numberofpatients,-1);
  /// I = Number of colonized patients per day
  vector<vector<int> > I(numberofwards, vector<int>(maxdate,0));
  /// numberofacquisitions = Number of acquisitions per day
  vector<vector<int> > numberofacquisitions(numberofwards, vector<int>(maxdate,0));
  /// daysatrisk = Number of days at risk per ward
  vector<int> daysatrisk(numberofwards);
  /// totalacquisitions = Number of acquisitions per ward
  vector<int> totalacquisitions(numberofwards);
  
  
  cultureresultsperpatient(culturearray,cultureperpatient);
  ///cout << "Culture per patient:" << endl;
  ///printarray(cultureperpatient,"");
  poscultureposatadmission(culturearray,cultureperpatient,admissionstate);
  ///printarray(colstatus,"");
  numberofinfectiouspatientsperward(I,whereabouts,colstatus,admissionday,acquisitionday,admissionstate,acquisition,numberofacquisitions);
  ///printarray(colstatus,"");
  int patient,indexday;
  /// Define the vectors linstneverpos, listdefpos, listtempos
  defineneverposdefpostemppos(listneverpos,listdefpos,listtemppos,admissionstate,acquisition,culturearray,cultureperpatient);
  cout<<"listneverpos,listdefpos,listtemppos="<<listneverpos.size()<<", "<<listdefpos.size()<<", "<<listtemppos.size()<<endl;
  
  //Generate Random number for exponential prior distribution for environmental contamination
  //boost::mt19937 gen;
  
   cout << "**************************************"<<endl;
   cout << "Print culturearray" << endl;
   cout << "**************************************"<<endl;
   //printarray(culturearray,culturedata); 
  
  
  
  /**********************************************************************
   * ADDED: Initilaization of environmental contamination
   * --------------------------------------------------------------------
   * E: Vector for environmental contamination for each ward and each day
   * E_stay: Vector for env.cont. for each ward and each day contributed
   * by present patients
   * E_dis: Vector for env.cont. for each ward and each day contributed
   * by discharged patients
   * *******************************************************************/
  vector<vector<double> > E(numberofwards, vector<double>(maxdate+1,0));
  vector<vector<double> > E_stay(numberofwards, vector<double>(maxdate+1,0));
  vector<vector<double> > E_dis(numberofwards, vector<double>(maxdate+1,0));
  if(addingenvironment == 1){
    cout << "E0 = " << param.E0 << "\n";
    /// Calculate environmental load with initial number of infectious patients, parameters and bacterial load E0
    updateenvironment2(param,I,N,admissionstate,acquisition,acquisitionday,admissionday,dischargeday,E,E_stay,E_dis,numberofwards,maxdate);
    cout << "Initial update of environment was successful." << endl;
  }
  
  /// Update endperiod (added to Martins code)
  endperiod = maxdate;
  cout <<"voor logl: maxdate="<<maxdate<<" startperiod="<<startperiod<<" endperiod="<<endperiod<<endl;
    
  /// First loglikelihood computation
  double logl=loglikelihood(acquisitionday,acquisition,culturearray,N,I,numberofacquisitions,admissionstate,param,startperiod,endperiod,admissionday,multipleparameters,E,addingenvironment);
  cout << "logl = " << logl << "\n";
  cout << "na logl" <<endl;
  
  numberofinfectiouspatients2(I,whereabouts,colstatus,admissionday,acquisitionday,admissionstate,acquisition,numberofacquisitions,maxdate,numberofwards);
  
  /// possibleacquisitiondays: determine possible days of acquisition per patient 
  vector<int> possibleacquistiondays(numberofpatients);
  cout << "Determine possible acquisition days." << endl; 
  determinepossibleacquisitiondays(possibleacquistiondays,culturearray,whereabouts,admissionday);
  cout << "Possible acquisition days ended." << endl; 
  
  typedef boost::mt19937  ENG;    /* Mersenne Twister: NOT SEEDED YET */
  ENG eng(seed);
  
  /// Initial parameters for multiple wards
  parameters paramnew;
  for(unit=0;unit<numberofwards;unit++)
  {
    (paramnew.alphavector).push_back(param.a);
    (paramnew.betavector).push_back(param.b);
    
    (paramnew.gammavector).push_back(param.c);
    (paramnew.nuvector).push_back(param.nu);
    (paramnew.muvector).push_back(param.mu);
  }
  
  /**********************************************************************
    * Definition of file streams for saving output
  *********************************************************************/
  ofstream myfile; myfile.open(savefile.c_str());
  ofstream mypatients; mypatients.open(patientsfile.c_str());
  
  ofstream mystartfile; mystartfile.open(startline.c_str());
  
  ofstream myfoifile; myfoifile.open(foifile.c_str());
  ofstream myprevfile; myprevfile.open(prevfile.c_str());
  ofstream myNfile; myNfile.open(Nfile.c_str());
  ofstream myacqfile; myacqfile.open(acqfile.c_str());
  ofstream myimpfile; myimpfile.open(impfile.c_str());
  ofstream mypatientsacqfile; mypatientsacqfile.open(patientsacqfile.c_str());
  ofstream mypresentfile; mypresentfile.open(presentpatients.c_str());
  
  //ofstream myenvfile; myenvfile.open(envfile.c_str());
  //ofstream myenvstayfile; myenvstayfile.open(envstayfile.c_str());
  //ofstream myenvdisfile; myenvdisfile.open(envdisfile.c_str());
  //ofstream colstat; colstat.open(colstatusfile.c_str());
  
    
  /// Header of mytestresults
  myfile <<"i,f,phi,logl,nImp,meanPrev,alpha,beta,eps,nu,mu,E0,TP,FN,aacceptrate,bacceptrate,cacceptrate,muacceptrate,sigmaa,sigmab,sigmac,sigmamu,acq,daysatr,contralpha,contrbeta,contrE,contrCrossT,contrEnv"<<"\n";
  
  /// Computing true positives and false negatives
  cout << "Computing true positives and false negatives." << endl;
  truepositiveandfalsenegative(&truepositive,&falsenegative,culturearray,admissionday,colstatus,startperiod,endperiod);
  cout << "True positives="<<truepositive<<endl;
  cout << "False negatives="<<falsenegative<<endl;
  if(addingenvironment == 1){
    cout << "E0 = " << param.E0 << "\n";
    updateenvironment2(param,I,N,admissionstate,acquisition,acquisitionday,admissionday,dischargeday,E,E_stay,E_dis,numberofwards,maxdate);
    cout << "Initial update of environment was successful." << endl;
  }

  /*
  cout << "Possible acquisition days per patient:\n";
  for(int patient=0;patient<possibleacquistiondays.size();patient++){
    cout << "Patient " << patient << ": " << possibleacquistiondays.at(patient) << "\n";
  }
  */


  /**********************************************************************
   *  start the MCMC algorithm.
   *********************************************************************/
  cout << "**************************"<<endl;
  cout <<"start MCMC\n";
  cout << "**************************"<<endl;
  i=0;
  /// this part ensures that the acceptance rate of alpha and beta is approximately between 0.05 and 0.5
  while(i<numberofupdates){
    if(onlyaugmented!=1)
    {
      change=0;
      
      if(i==updatestochangesigma)
      {
        if(endogenous==1){
          if((double)aaccepted/(double)updatestochangesigma<lowerboundacceptance)
          {
            sigmaa=sigmaa/samplefromrealuniform(1.5, 2.5, &eng);change=1;
            ///cout<<"Change a. Lower bound. \n";
          }
          if((double)aaccepted/(double)updatestochangesigma>upperboundacceptance)
          {
            sigmaa=sigmaa*samplefromrealuniform(1.5, 2.5, &eng);change=1;
            ///cout<<"Change a. Upper bound. \n";
          }
        }
        
        if(exogenous==1){
          if((double)baccepted/(double)updatestochangesigma<lowerboundacceptance)
          {
            sigmab=sigmab/samplefromrealuniform(1.5, 2.5, &eng);change=1;
            ///cout<<"Change b. Lower bound.\n";
          }
          if((double)baccepted/(double)updatestochangesigma>upperboundacceptance)
          {
            sigmab=sigmab*samplefromrealuniform(1.5, 2.5, &eng);change=1;
            ///cout<<"Change b. Upper bound. \n";
          }
        }
        
        if(addingenvironment == 1){
          if((double)caccepted/(double)updatestochangesigma<lowerboundacceptance)
          {
            sigmac=sigmac/samplefromrealuniform(1.5, 2.5, &eng);change=1;
            ///cout<<"Change c. Lower bound. \n";
          }
          if((double)caccepted/(double)updatestochangesigma>upperboundacceptance)
          {
            sigmac=sigmac*samplefromrealuniform(1.5, 2.5, &eng);change=1;
            ///cout<<"Change c. Upper bound. \n";
          }
          if((double)muaccepted/(double)updatestochangesigma<lowerboundacceptance)
          {
            sigmamu=sigmamu/samplefromrealuniform(1.5, 2.5, &eng);change=1;
            ///cout<<"Change mu. Lower bound. \n";
          }
          if((double)muaccepted/(double)updatestochangesigma>upperboundacceptance)
          {
            sigmamu=sigmamu*samplefromrealuniform(1.5, 2.5, &eng);change=1;
            ///cout<<"Change mu. Upper bound.\n";
          }
        }
      if(change==1){i=0;aaccepted=0;baccepted=0;caccepted=0;nuaccepted=0;muaccepted=0;nchange+=1;}
      }
      
      if(i==0) inew=updatestochangesigma;
      else inew=i;

      if(i==changeaugmented) factoraugmented=newfactoraugmented;
      
      /**update param.a;*****/
          if(i>0)
          {
            if(multipleparameters==0)
            {
              foi = (double)endogenous*param.a + (double)exogenous*param.b*meanPrev + (double)addingenvironment*param.c*meanE;
              if(addingenvironment == 0){
                
                /**update param.a ***/
                  paramnew.b=param.b;

                  paramnew.c=param.c;
                  paramnew.nu=param.nu;
                  paramnew.mu=param.mu;
                  paramnew.E0=param.E0;
                  
                  paramnew.frequencydependence= param.frequencydependence;
                  paramnew.phi=param.phi;
                  paramnew.f=param.f;
                  
                  if(endogenous){
                    newa=abs(param.a+samplefromnormal(0,sigmaa,&eng));
                    paramnew.a=newa;
                    
                    newlogl=loglikelihood(acquisitionday,acquisition,culturearray,N,I,numberofacquisitions,admissionstate,paramnew,startperiod,endperiod,admissionday,multipleparameters,E,addingenvironment);
                    
                    double logp=min(0.,newlogl+log(pdf(prioralpha,newa))-logl-log(pdf(prioralpha,param.a)));
                    p=exp(logp);
                    p=min(1.,exp(newlogl-logl)*pdf(prioralpha,newa)/pdf(prioralpha,param.a));
                    r=samplefromrealuniform(0, 1, &eng);
                    if(r<p){param.a=newa;logl=newlogl;aaccepted+=1;}
                  }
                  /** End update a **/
                    
                    
                  /**update param.b ***/
                    
                  paramnew.a=param.a;
                  if(exogenous){
                    newmub = (foi-paramnew.a)/meanPrev; 
                    newsigmab = sqrt(sigmaa*sigmaa/(meanPrev*meanPrev)+sigmab*sigmab);
                    newb=abs(samplefromnormal(newmub,newsigmab,&eng));
                    
                    paramnew.b=newb;

                    newlogl=loglikelihood(acquisitionday,acquisition,culturearray,N,I,numberofacquisitions,admissionstate,paramnew,startperiod,endperiod,admissionday,multipleparameters,E,addingenvironment);
                    //cout <<"param.b="<<param.b<<", logl="<<logl<<", newparam.b="<<paramnew.b<<", newlogl="<<newlogl<<endl;
                    //logp=min(0.,newlogl+log(pdf(priorbeta,newb))-logl-log(pdf(priorbeta,param.b)));
                    //p=exp(logp);
                    // Multiplicative term of proposal distribution is still missing!
                      
                    propratio = exp((newb*newb-param.b*param.b + 2*newmub*(param.b-newb))/(2*newsigmab*newsigmab));
                    p=min(1.,propratio*exp(newlogl-logl)*pdf(priorbeta,newb)/pdf(priorbeta,param.b));
                    r=samplefromrealuniform(0, 1, &eng);
                    if(r<p){param.b=newb;logl=newlogl;baccepted+=1;}
                  }
                  
                  /** End update b **/
              }
              
              if(addingenvironment == 1){
                
                /**update param.a ***/
                  paramnew.b=param.b;

                  paramnew.c=param.c;
                  paramnew.nu=param.nu;
                  paramnew.mu=param.mu;
                  paramnew.E0=param.E0;
                  
                  paramnew.frequencydependence= param.frequencydependence;
                  paramnew.phi=param.phi;
                  paramnew.f=param.f;
                  
                  if(endogenous){
                    newa=abs(param.a+samplefromnormal(0,sigmaa,&eng));
                    paramnew.a=newa;
                    
                    newlogl=loglikelihood(acquisitionday,acquisition,culturearray,N,I,numberofacquisitions,admissionstate,paramnew,startperiod,endperiod,admissionday,multipleparameters,E,addingenvironment);
                    
                    double logp=min(0.,newlogl+log(pdf(prioralpha,newa))-logl-log(pdf(prioralpha,param.a)));
                    p=exp(logp);
                    p=min(1.,exp(newlogl-logl)*pdf(prioralpha,newa)/pdf(prioralpha,param.a));
                    r=samplefromrealuniform(0, 1, &eng);
                    if(r<p){param.a=newa;logl=newlogl;aaccepted+=1;}
                  }
                  /** End update a **/
                    
                    
                    /**update param.b ***/
                    paramnew.a=param.a;
                  if(exogenous){
                    newb=abs(param.b + samplefromnormal(0,sigmab,&eng));
                    paramnew.b=newb;

                    newlogl=loglikelihood(acquisitionday,acquisition,culturearray,N,I,numberofacquisitions,admissionstate,paramnew,startperiod,endperiod,admissionday,multipleparameters,E,addingenvironment);
                    //cout <<"param.b="<<param.b<<", logl="<<logl<<", newparam.b="<<paramnew.b<<", newlogl="<<newlogl<<endl;
                    //logp=min(0.,newlogl+log(pdf(priorbeta,newb))-logl-log(pdf(priorbeta,param.b)));
                    //p=exp(logp);
                    
                    p=min(1.,exp(newlogl-logl)*pdf(priorbeta,newb)/pdf(priorbeta,param.b));
                    r=samplefromrealuniform(0, 1, &eng);
                    if(r<p){param.b=newb;logl=newlogl;baccepted+=1;}
                  }
                  /** End update b **/
                    
                  /**update param.nu **/
                  /// param.nu is fixed throughout the simulation
                  /** End update nu **/
                    
                  /**update param.mu ***/
                  paramnew.b = param.b;
                  paramnew.nu = param.nu;
                  
                  newmu=abs(param.mu + samplefromnormal(0,sigmamu,&eng));
                  paramnew.mu=newmu;
                  
                  if(addingenvironment ==1) updateenvironment2(paramnew,I,N,admissionstate,acquisition,acquisitionday,admissionday,dischargeday,E,E_stay,E_dis,numberofwards,maxdate);
                  
                  newlogl=loglikelihood(acquisitionday,acquisition,culturearray,N,I,numberofacquisitions,admissionstate,paramnew,startperiod,endperiod,admissionday,multipleparameters,E,addingenvironment);
                  //cout <<"param.mu="<<param.mu<<", logl="<<logl<<", newparam.mu="<<paramnew.mu<<", newlogl="<<newlogl<<endl;
                  p=min(1.,exp(newlogl-logl)*pdf(priormu,newmu)/pdf(priormu,param.mu));
                  r=samplefromrealuniform(0, 1, &eng);
                  if(r<p){param.mu=newmu;logl=newlogl;muaccepted+=1;}
                  
                  /** End update mu **/
                    
                  /**update param.c ***/
                  paramnew.mu = param.mu;
                  meanE = (paramnew.nu/paramnew.mu)*meanPrev;
                  newsigmac=sqrt(sigmaa*sigmaa/(meanE*meanE) + sigmab*sigmab*(meanPrev*meanPrev)/(meanE*meanE) + sigmac*sigmac);
                  newmuc=(foi-paramnew.a-paramnew.b*meanPrev)/meanE;
                  newc=abs(samplefromnormal(newmuc, newsigmac,&eng));
                  paramnew.c=newc;

                  newlogl=loglikelihood(acquisitionday,acquisition,culturearray,N,I,numberofacquisitions,admissionstate,paramnew,startperiod,endperiod,admissionday,multipleparameters,E,addingenvironment);
                  //cout <<"param.c="<<param.c<<", logl="<<logl<<", newparam.c="<<paramnew.c<<", newlogl="<<newlogl<<endl;
                  // Adjust the acceptance ratio, since proposal density is not symmetric anymore.
                  propratio=exp((paramnew.c*paramnew.c-param.c*param.c+2*newmuc*(param.c-paramnew.c))/(2*newsigmac*newsigmac));
                  p=min(1.,propratio*exp(newlogl-logl)*pdf(priorgamma,newc)/pdf(priorgamma,param.c));
                  r=samplefromrealuniform(0, 1, &eng);
                  if(r<p){param.c=newc;logl=newlogl;caccepted+=1;}
                  
                  /** End update c **/
              }

              
            }// End single parameter
            else //multiple parameters
            {
              /** start update a **/
                
                for(unit=0;unit<numberofwards;unit++)
                {
                  newalpha.at(unit)=abs(param.alphavector.at(unit)+samplefromnormal(0,sigmaa,&eng));
                  
                  for(ward=0;ward<numberofwards;ward++)
                  {
                    paramnew.alphavector.at(ward)=param.alphavector.at(ward);
                    paramnew.betavector.at(ward)=param.betavector.at(ward);
                    paramnew.gammavector.at(ward)=param.gammavector.at(ward);
                    paramnew.nuvector.at(ward)=param.nuvector.at(ward);
                    paramnew.muvector.at(ward)=param.muvector.at(ward);
                  }
                  paramnew.alphavector.at(unit)=newalpha.at(unit);
                  //cout <<"unita="<<unit<<endl;
                  //paramnew.b=param.b;
                  paramnew.phi=param.phi;
                  paramnew.frequencydependence= param.frequencydependence;
                  paramnew.f=param.f;

                  newlogl=loglikelihood(acquisitionday,acquisition,culturearray,N,I,numberofacquisitions,admissionstate,paramnew,startperiod,endperiod,admissionday,multipleparameters,E,addingenvironment);
                  //cout <<"param.a="<<param.a<<", logl="<<logl<<", newparam.a="<<paramnew.a<<", newlogl="<<newlogl<<endl;
                  //logp=min(0.,newlogl+log(pdf(prioralpha,newa))-logl-log(pdf(prioralpha,param.a)));
                  //p=exp(logp);
                  p=min(1.,exp(newlogl-logl)*pdf(prioralpha,newalpha.at(unit))/pdf(prioralpha,param.alphavector.at(unit)));
                  r=samplefromrealuniform(0, 1, &eng);
                  if(r<p){param.alphavector.at(unit)=newalpha.at(unit);logl=newlogl;}
                  /** End update a **/
                    //cout <<"unitb="<<unit<<endl;
                  /**update param.b **/
                    newbeta.at(unit)=abs(param.betavector.at(unit)+samplefromnormal(0,sigmab,&eng));
                  
                  for(ward=0;ward<numberofwards;ward++)
                  {
                    paramnew.alphavector.at(ward)=param.alphavector.at(ward);
                    paramnew.betavector.at(ward)=param.betavector.at(ward);
                    paramnew.gammavector.at(ward)=param.gammavector.at(ward);
                    paramnew.nuvector.at(ward)=param.nuvector.at(ward);
                    paramnew.muvector.at(ward)=param.muvector.at(ward);
                  }
                  paramnew.betavector.at(unit)=newbeta.at(unit);
                  paramnew.frequencydependence= param.frequencydependence;

                  newlogl=loglikelihood(acquisitionday,acquisition,culturearray,N,I,numberofacquisitions,admissionstate,paramnew,startperiod,endperiod,admissionday,multipleparameters,E,addingenvironment);
                  
                  p=min(1.,exp(newlogl-logl)*pdf(priorbeta,newb)/pdf(priorbeta,param.b));
                  r=samplefromrealuniform(0, 1, &eng);
                  if(r<p){param.betavector.at(unit)=newbeta.at(unit);logl=newlogl;}
                  /** End update b **/
                    
                    // ADDED
                  /**update param.c **/
                    newgamma.at(unit)=abs(param.gammavector.at(unit)+samplefromnormal(0,sigmac,&eng));
                  
                  for(ward=0;ward<numberofwards;ward++)
                  {
                    paramnew.alphavector.at(ward)=param.alphavector.at(ward);
                    paramnew.betavector.at(ward)=param.betavector.at(ward);
                    paramnew.gammavector.at(ward)=param.gammavector.at(ward);
                    paramnew.nuvector.at(ward)=param.nuvector.at(ward);
                    paramnew.muvector.at(ward)=param.muvector.at(ward);
                  }
                  paramnew.gammavector.at(unit)=newgamma.at(unit);
                  paramnew.frequencydependence= param.frequencydependence;

                  newlogl=loglikelihood(acquisitionday,acquisition,culturearray,N,I,numberofacquisitions,admissionstate,paramnew,startperiod,endperiod,admissionday,multipleparameters,E,addingenvironment);
                  
                  p=min(1.,exp(newlogl-logl)*pdf(priorgamma,newc)/pdf(priorgamma,param.c));
                  r=samplefromrealuniform(0, 1, &eng);
                  if(r<p){param.gammavector.at(unit)=newgamma.at(unit);logl=newlogl;}
                  /** End update c **/
                    
                    
                  /**update param.mu **/
                  newmuvector.at(unit)=abs(param.muvector.at(unit)+samplefromnormal(0,sigmamu,&eng));
                  
                  for(ward=0;ward<numberofwards;ward++){
                    paramnew.alphavector.at(ward)=param.alphavector.at(ward);
                    paramnew.betavector.at(ward)=param.betavector.at(ward);
                    paramnew.gammavector.at(ward)=param.gammavector.at(ward);
                    paramnew.nuvector.at(ward)=param.nuvector.at(ward);
                    paramnew.muvector.at(ward)=param.muvector.at(ward);
                  }
                  paramnew.muvector.at(unit)=newmuvector.at(unit);
                  paramnew.frequencydependence= param.frequencydependence;

                  newlogl=loglikelihood(acquisitionday,acquisition,culturearray,N,I,numberofacquisitions,admissionstate,paramnew,startperiod,endperiod,admissionday,multipleparameters,E,addingenvironment);
                  
                  p=min(1.,exp(newlogl-logl)*pdf(priormu,newmu)/pdf(priormu,param.mu));
                  r=samplefromrealuniform(0, 1, &eng);
                  if(r<p){param.muvector.at(unit)=newmuvector.at(unit);logl=newlogl;}
                  /** End update mu **/
                }
            }// End update multiple parameters
            
            /**calculate number of false negative tests,
            could be changed in changeloglandI as well, to avoid running over all culures, but gain limited
            **/
            //truepositiveandfalsenegative(&truepositive,&falsenegative,culturearray,admissionday,colstatus,startperiod,endperiod);
            falsenegativefunction(&falsenegative,culturearray,admissionday,colstatus,startperiod,endperiod);
            //cout << "TP="<<truepositive<<" FN="<<falsenegative<<endl;
            
            /**********************************************************************************
             * Update param.phi
             * Sample from beta-distribution, see web appendix AJE Worby
             * Suppose X~Gamma(1,α) and Y~Gamma(1,β). Then Z=X/(X+Y) has distribution Beta(α,β)
             *********************************************************************************/
            x=samplefromgamma(aa+truepositive,1.,&eng);
            y=samplefromgamma(bb+falsenegative,1.,&eng);
            logl-=truepositive*log(param.phi)+falsenegative*log(1.-param.phi);
            param.phi=x/(x+y);
            logl+=truepositive*log(param.phi)+falsenegative*log(1.-param.phi);
            /*End update phi*/
            /* NOTE: Code can be made slightly faster by updating falsenegative, posatadmission and negatadmission in file changeloglandI.cpp */

            /**********************************************************************************
             * Update param.f
             * Sample from beta-distribution, see web appendix AJE Worby
             * Suppose X~Gamma(1,α) and Y~Gamma(1,β). Then Z=X/(X+Y) has distribution Beta(α,β)
             *********************************************************************************/
            numberposatadmisison(&posatadmission,&negatadmission, admissionstate,admissionday,startperiod, endperiod);
            x=samplefromgamma(aa+posatadmission,1.,&eng);
            y=samplefromgamma(bb+negatadmission,1.,&eng);

            logl-=posatadmission*log(param.f)+negatadmission*log(1.-param.f);
            param.f=x/(x+y);
            logl+=posatadmission*log(param.f)+negatadmission*log(1.-param.f);
          }
    }
    
    /**********************************************************************************
      Update environmental contamination after parameter update
    **********************************************************************************/
    if(addingenvironment ==1) updateenvironment2(param,I,N,admissionstate,acquisition,acquisitionday,admissionday,dischargeday,E,E_stay,E_dis,numberofwards,maxdate);

    if(onlyaugmented==1) newlogl=loglikelihood(acquisitionday,acquisition,culturearray,N,I,numberofacquisitions,admissionstate,param,startperiod,endperiod,admissionday,multipleparameters,E,addingenvironment);
    
    /**********************************************************************************
     * Update augmented data
     *********************************************************************************/
        for(dummy=0;dummy<factoraugmented;dummy++){
          donothing=0;
          event=samplefromintuniform(0,2,&eng);
          switch(event){
            case 0: /// move a colonization time
              thesize=(int)(listdefpos.size()+listtemppos.size());
            if (thesize>0)
            {
              index=samplefromintuniform(0,thesize-1,&eng);
              if(index<(int)listdefpos.size())
              {
                patient=listdefpos.at(index);
              }
              else
              {
                patient=listtemppos.at(index-listdefpos.size());
              }
              r=samplefromrealuniform(0, 1, &eng);
              if(r<w){ /// positive on admission 
                newadmissionstate=1;
                newacquisition=0;
                newacquisitionday=0;
              }
              else{ /// negative at admission*/
                  if(possibleacquistiondays.at(patient)<=0){donothing=1;}
                else{
                  newadmissionstate=0;
                  newacquisition=1;
                  indexday=samplefromintuniform(0,possibleacquistiondays.at(patient)-1,&eng);
                  newacquisitionday=indexday;
                }
              }
              q=1;
              //if(admissionstate.at(patient)==1&newadmissionstate==1)q=1.;
              if(admissionstate.at(patient)==0&newadmissionstate==1)q=(1.-w)/w/(double)(possibleacquistiondays.at(patient));
              //if(admissionstate.at(patient)==0&newadmissionstate==0)q=1.;
              if(admissionstate.at(patient)==1&newadmissionstate==0)q=w*(double)(possibleacquistiondays.at(patient))/(1-w);
            }
            else{donothing=1;}
            break;
            case 1: /// add a colonization time 
              if(listneverpos.size()>0)
              {
                index=samplefromintuniform(0,(int)listneverpos.size()-1,&eng);
                patient=listneverpos.at(index);
                r=samplefromrealuniform(0, 1, &eng);
                if(r<w){ /// patient admitted positive
                    newadmissionstate=1;
                    newacquisition=0;
                    newacquisitionday=0;
                    q=(double)listneverpos.size()/w/(double)(listtemppos.size()+1);
                }
                else{ /// patient uncolonized upon admission */
                  newadmissionstate=0; 
                  newacquisition=1;
                  indexday=samplefromintuniform(0,possibleacquistiondays.at(patient)-1,&eng);
                  newacquisitionday=indexday;
                  q=(double)listneverpos.size()*possibleacquistiondays.at(patient)/(1.-w)/(double)(listtemppos.size()+1);
                }
              }
            else{donothing=1;}
            break;
            case 2: /// remove a colonzation time */
              if(listtemppos.size()>0){
                index=samplefromintuniform(0,(int)listtemppos.size()-1,&eng);
                patient=listtemppos.at(index);
                newadmissionstate=0;
                newacquisition=0;
                newacquisitionday=0;
                if(admissionstate.at(patient)==1){
                  q=listtemppos.size()*w/(double)(listneverpos.size()+1);
                }
                else{
                  q=listtemppos.size()*(1-w)/(double)(listneverpos.size()+1)/(possibleacquistiondays.at(patient));
                }
              }
            else{donothing=1;}
            break;
            default:  donothing=1;break;
          }
          if(donothing==0){ ///something is changing!
            oldacquisition= acquisition.at(patient);
            oldacquisitionday=acquisitionday.at(patient);
            oldadmissionstate=admissionstate.at(patient);
            oldlogl=logl;
            
            /*
              changeloglandI(patient,oldacquisitionday,oldadmissionstate,oldacquisition,newacquisitionday,newadmissionstate,newacquisition,
                             startperiod,endperiod,&logl,param,present,I,N,colstatus,culturearray,cultureperpatient,whereabouts,numberofacquisitions,admissionday,multipleparameters);
            */

            // Change vectors acquisition to be used in function loglikelihood
            if(addingenvironment == 1){
              acquisition.at(patient)=newacquisition;
              acquisitionday.at(patient)=newacquisitionday;
              admissionstate.at(patient)=newadmissionstate;
              /// Change I according to changes in acquisition, acquisitionday, admissionstate etc.
              numberofinfectiouspatients2(I,whereabouts,colstatus,admissionday,acquisitionday,admissionstate,acquisition,numberofacquisitions,maxdate,numberofwards);
              
              updateenvironment2(param,I,N,admissionstate,acquisition,acquisitionday,admissionday,dischargeday,E,E_stay,E_dis,numberofwards,maxdate);
              // Include Environmenmtal Contamination E, use "normal" loglikelihood function for added environmental contamination
              logl = loglikelihood(acquisitionday,acquisition,culturearray,N,I,numberofacquisitions,admissionstate,param,startperiod,endperiod,admissionday,multipleparameters,E,addingenvironment);
            } else{
              changeloglandI(patient,oldacquisitionday,oldadmissionstate,oldacquisition,newacquisitionday,newadmissionstate,newacquisition,
                             startperiod,endperiod,&logl,param,present,I,N,colstatus,culturearray,cultureperpatient,whereabouts,numberofacquisitions,admissionday,multipleparameters);
            }
            p=min(1.,exp(logl-oldlogl)*q);
            r=samplefromrealuniform(0, 1, &eng);
            
            if(r>p){/// no change, recover originalvalues
              // Change back (recover original values)
              if(addingenvironment == 1){
                acquisition.at(patient)=oldacquisition;
                acquisitionday.at(patient)=oldacquisitionday;
                admissionstate.at(patient)=oldadmissionstate;
                numberofinfectiouspatients2(I,whereabouts,colstatus,admissionday,acquisitionday,admissionstate,acquisition,numberofacquisitions,maxdate,numberofwards);
                
                updateenvironment2(param,I,N,admissionstate,acquisition,acquisitionday,admissionday,dischargeday,E,E_stay,E_dis,numberofwards,maxdate);
                logl = loglikelihood(acquisitionday,acquisition,culturearray,N,I,numberofacquisitions,admissionstate,param,startperiod,endperiod,admissionday,multipleparameters,E,addingenvironment);
              } else{
                changeloglandI(patient,newacquisitionday,newadmissionstate,newacquisition,oldacquisitionday,oldadmissionstate,oldacquisition,startperiod,endperiod,&logl,param,present,I,N,colstatus,culturearray,cultureperpatient,whereabouts,numberofacquisitions,admissionday,multipleparameters);
              }
            }
            else{
              if(addingenvironment == 0){
                acquisition.at(patient)=newacquisition;
                acquisitionday.at(patient)=newacquisitionday;
                admissionstate.at(patient)=newadmissionstate;
              }
              
              if(event==1&&listneverpos.size()>0)
              {
                addelementtovector(listtemppos,patient);
                deleteindexelementvector(listneverpos,index);
              }
              if(event==2&&listtemppos.size()>0)
              {
                addelementtovector(listneverpos,patient);
                deleteindexelementvector(listtemppos,index);
              }
            }
          }
          // cout<< "logl in augmented data = "<<logl<<"\n";
        }
    
    /// Compute mean prevalence 
    int itotal=0, ntotal=0;
    for(unit=0;unit<numberofwards;unit++){
      for(day=0;day<maxdate;day++){
        itotal += I.at(unit).at(day); 
        ntotal += N.at(unit).at(day);
      }
    }
    meanPrev = (double)itotal/(double)ntotal;
    
  
    if(i/thinning==(i+thinning-1)/thinning){
      /** Computing contribution of each transmission route *************/
      /** ONLY FOR FREQUENCY DEPENDENCE *********************************/
      numberofinfectiouspatients2(I,whereabouts,colstatus,admissionday,acquisitionday,admissionstate,acquisition,numberofacquisitions,maxdate,numberofwards);
      contralpha=0.0; contrbeta=0.0; contrE=0.0; contrCrossT=0.0; contrEnv=0.0;
      double betaPart=0.0, envPart=0.0;
      numberimportations = 0;
      
      for(unit=0;unit<numberofwards;unit++){
        for(patient=0; patient<numberofpatients; patient++){
          if(admissionstate.at(patient)==1) numberimportations += 1;
          if(acquisition.at(patient)==1){
            acqday = admissionday.at(patient)+acquisitionday.at(patient);
            try{
              forceofinfection = (double)endogenous*param.a+(double)exogenous*param.b*((double)I.at(unit).at(acqday)/(double)N.at(unit).at(acqday))+(double)addingenvironment*param.c*E.at(unit).at(acqday);
            } catch(const std::out_of_range& e1) {
              cout <<"Out of range in forceofinfection: "<<"Patient "<<patient<<", admissionday="<< admissionday.at(patient)<<", dischargeday="<<dischargeday.at(patient)<<", acquisitionday="<<acqday<< endl;
            }
            contralpha+=(double)endogenous*param.a/forceofinfection;
            try{ 
              betaPart=(double)exogenous*param.b*((double)I.at(unit).at(acqday)/(double)N.at(unit).at(acqday))/forceofinfection;
              contrbeta+=betaPart;
            } catch(const std::out_of_range& e2) {
              cout<<"Out of range in contrbeta: "<<"Patient "<<patient<<", admissionday="<<admissionday.at(patient)<<", dischargeday="<<dischargeday.at(patient)<<", acquisitionday="<<acqday<<endl;
            }
            if(isnan(betaPart)){
              cout<<"NA in contrbeta: "<<"Patient "<<patient<<", admissionday="<<admissionday.at(patient)<<", dischargeday="<<dischargeday.at(patient)<<", acquisitionday="<<acqday<<endl;
            }
            try{
              contrE+=(double)addingenvironment*param.c*E.at(unit).at(acqday)/forceofinfection;
            } catch(const std::out_of_range& e3) {
              cout<<"Out of range in contrE: "<<"Patient "<<patient<<", admissionday="<<admissionday.at(patient)<<", dischargeday="<<dischargeday.at(patient)<<", acquisitionday="<<acqday<<endl;
            }
            try{
              contrCrossT+=betaPart+(double)addingenvironment*param.c*(E_stay.at(unit).at(acqday))/forceofinfection;
            } catch(const std::out_of_range& e4) {
              cout<<"Out of range in contrCrossT."<<"Patient "<<patient<<", admissionday="<<admissionday.at(patient)<<", dischargeday="<<dischargeday.at(patient)<<", acquisitionday="<<acqday<<endl;
            }
            try{
              envPart = (double)addingenvironment*param.c*(E.at(unit).at(acqday)-E_stay.at(unit).at(acqday))/forceofinfection;
              contrEnv+= envPart;
            } catch(const std::out_of_range& e5) {
              cout<<"Out of range in contrEnv."<<"Patient "<<patient<<", admissionday="<<admissionday.at(patient)<<", dischargeday="<<dischargeday.at(patient)<<", acquisitionday="<<acqday<<endl;
            }
            //if(envPart<0) cout<<"EnvPart negative. EnvPart="<<envPart<<", Patient "<<patient<<", admissionday="<<admissionday.at(patient)<<", dischargeday="<<dischargeday.at(patient)<<", acquisitionday="<<acqday<<endl;
          }
        }
      }
      /*
      if(contralpha==0 || contrbeta==0){
        myfoifile << i <<","<<param.a<<","<<param.b<<",";
        mypatients << "i = " << i << endl; 
        for(unit=0;unit<numberofwards;unit++){
          for(patient=0; patient<numberofpatients-1; patient++){
            mypatients << admissionday.at(patient) << "," << dischargeday.at(patient) << "," << acquisitionday.at(patient) << "," << possibleacquistiondays.at(patient) << endl; 
            myimpfile << admissionstate.at(patient) << ",";
            if(acquisition.at(patient)==1) mypatientsacqfile << admissionday.at(patient) + acquisitionday.at(patient) <<",";
            else mypatientsacqfile << "-1,";
          }
          mypatients << admissionday.at(numberofpatients-1) << "," << dischargeday.at(numberofpatients-1) << "," << acquisitionday.at(numberofpatients-1) << "," << possibleacquistiondays.at(numberofpatients-1) << endl; 
          if(acquisition.at(numberofpatients-1)==1) mypatientsacqfile << admissionday.at(numberofpatients-1) + acquisitionday.at(numberofpatients-1) << endl;
          else mypatientsacqfile << "-1" << endl;
          myimpfile << admissionstate.at(numberofpatients-1) << endl;
          
          for(day=0;day<maxdate-1;day++){
            myfoifile <<(double)endogenous*param.a+(double)exogenous*param.b*((double)I.at(unit).at(day)/(double)N.at(unit).at(day))+(double)addingenvironment*param.c*E.at(unit).at(day)<<",";
            myprevfile <<I.at(unit).at(day)<<",";
            myNfile <<N.at(unit).at(day)<<",";
            myacqfile << numberofacquisitions.at(unit).at(day) << ",";
            for(int p=0; p < present.at(unit).at(day).size()-1;p++){
              mypresentfile << present.at(unit).at(day).at(p) << ","; 
            }
            mypresentfile << present.at(unit).at(day).at(present.at(unit).at(day).size()-1) << endl; 
          }
          myfoifile <<param.a+param.b*((double)I.at(unit).at(maxdate-1)/(double)N.at(unit).at(maxdate-1))+(double)addingenvironment*param.c*E.at(unit).at(maxdate-1)<<endl;
          
          myprevfile <<I.at(unit).at(maxdate-1)<<endl;
          myNfile <<N.at(unit).at(maxdate-1)<<endl;
          myacqfile << numberofacquisitions.at(unit).at(maxdate-1) << endl;
        }
      }
      */ 
      totaldaysatrisk(daysatrisk,totalacquisitions,numberofacquisitions,N,I,startperiod,endperiod);
      myfile <<i<<",";
      myfile <<param.f<<",";
      myfile <<param.phi<<",";
      myfile <<logl<<",";
      myfile <<numberimportations<<",";
      myfile <<meanPrev<<",";
      myfile <<param.a<<",";
      myfile <<param.b<<",";
      myfile <<param.c<<",";
      myfile <<param.nu<<",";
      myfile <<param.mu<<",";
      myfile <<param.E0<<",";
      
      myfile <<truepositive<<","<<falsenegative<<",";
      myfile <<(double)aaccepted/(double)inew<<","<<(double)baccepted/(double)inew<<",";
      myfile <<(double)caccepted/(double)inew<<","<<(double)muaccepted/(double)inew<<",";
      myfile <<(double)sigmaa<<","<<(double)sigmab<<","<<(double)sigmac<<","<<(double)sigmamu<<",";
      
      for(unit=0;unit<numberofwards;unit++){
        //myfile <<"acq["<<unit<<"]="<<totalacquisitions.at(unit);
        myfile <<totalacquisitions.at(unit);
        myfile <<", daysatr["<<unit<<"]="<<daysatrisk.at(unit)<<",";
      }
      
      myfile <<contralpha<<",";
      myfile <<contrbeta<<",";
      myfile <<contrE<<", ";
      myfile <<contrCrossT<<", ";
      myfile <<contrEnv<<"\n";
    } // End if(i/thinning==(i+thinning-1)/thinning)
    
 
    if(i/thinningfoi==(i+thinningfoi-1)/thinningfoi){
        myfoifile << i <<","<<param.a<<","<<param.b<<",";
        //mypatients << "i = " << i << endl; 
        for(unit=0;unit<numberofwards;unit++){
            for(patient=0; patient<numberofpatients-1; patient++){
                mypatients << admissionday.at(patient) << "," << dischargeday.at(patient) << "," << acquisitionday.at(patient) << "," << possibleacquistiondays.at(patient) << endl; 
                myimpfile << admissionstate.at(patient) << ",";
                if(acquisition.at(patient)==1) mypatientsacqfile << admissionday.at(patient) + acquisitionday.at(patient) <<",";
                else mypatientsacqfile << "-1,";
            }
            mypatients << admissionday.at(numberofpatients-1) << "," << dischargeday.at(numberofpatients-1) << "," << acquisitionday.at(numberofpatients-1) << "," << possibleacquistiondays.at(numberofpatients-1) << endl; 
            if(acquisition.at(numberofpatients-1)==1) mypatientsacqfile << admissionday.at(numberofpatients-1) + acquisitionday.at(numberofpatients-1) << endl;
            else mypatientsacqfile << "-1" << endl;
            myimpfile << admissionstate.at(numberofpatients-1) << endl;
        
            for(day=0;day<maxdate-1;day++){
                myfoifile <<(double)endogenous*param.a+(double)exogenous*param.b*((double)I.at(unit).at(day)/(double)N.at(unit).at(day))+(double)addingenvironment*param.c*E.at(unit).at(day)<<",";
                myprevfile <<I.at(unit).at(day)<<",";
                myNfile <<N.at(unit).at(day)<<",";
                myacqfile << numberofacquisitions.at(unit).at(day) << ",";
                for(int p=0; p < present.at(unit).at(day).size()-1;p++){
                    mypresentfile << present.at(unit).at(day).at(p) << ","; 
                }
                mypresentfile << present.at(unit).at(day).at(present.at(unit).at(day).size()-1) << endl; 
            }
            myfoifile <<param.a+param.b*((double)I.at(unit).at(maxdate-1)/(double)N.at(unit).at(maxdate-1))+(double)addingenvironment*param.c*E.at(unit).at(maxdate-1)<<endl;
        
            myprevfile <<I.at(unit).at(maxdate-1)<<endl;
            myNfile <<N.at(unit).at(maxdate-1)<<endl;
            myacqfile << numberofacquisitions.at(unit).at(maxdate-1) << endl;
      }
    }
        
     
    if(i/thinningscreen==(i+thinningscreen-1)/thinningscreen){
      cout<<"i="<<i<<", ";
      cout <<"a="<<param.a<<", ";
      cout <<"b="<<param.b<<", ";
      cout <<"f="<<param.f<<", ";
      // ADDED
      cout <<"c="<<param.c<<", ";
      cout <<"nu="<<param.nu<<", ";
      cout <<"mu="<<param.mu<<", ";
      cout <<"E0="<<param.E0<<", ";
      // END ADDED
      cout <<"phi="<<param.phi<<", ";
      cout <<"TP="<<truepositive<<", FN="<<falsenegative<<", ";
      cout <<"aa="<<(double)aaccepted/(double)inew<<", ba="<<(double)baccepted/(double)inew<<", ";
      cout <<"ca="<<(double)caccepted/(double)inew<<", nua="<<(double)nuaccepted/(double)inew<<", mua="<<(double)muaccepted/(double)inew<<", ";
      // ADDED sigmac, sigmanu, sigmamu
      cout <<"sa="<<(double)sigmaa<<", sb="<<(double)sigmab<<", sc="<<(double)sigmac<<",snu="<<(double)sigmanu<<", smu="<<(double)sigmamu<<", ";
      
      totaldaysatrisk(daysatrisk,totalacquisitions,numberofacquisitions,N,I,startperiod,endperiod);
      for(unit=0;unit<numberofwards;unit++)
      {
        cout <<"acq["<<unit<<"]="<<totalacquisitions.at(unit)<<", ";
        cout <<"daysatr["<<unit<<"]="<<daysatrisk.at(unit)<<", ";
      }
      cout <<"numImp="<<numberimportations<<", ";
      cout <<"meanPrev="<<meanPrev<<", ";
      cout <<"logl="<<logl<<"\n ";
    } // End if(i/thinningscreen==(i+thinningscreen-1)/thinningscreen)
    i+=1;
  } // End while(i<numberofupdates)
  
  cout<<"Number of changes (sigma) = "<<nchange<<endl;
  mystartfile<<nchange*(double)updatestochangesigma/10+1<<endl;
  cout<<"Line = "<< nchange*(double)updatestochangesigma/10 + 1<<endl;
  
  /**********************************************************************
   * Compute running time
   * *******************************************************************/
  time_t timer2 = time(NULL);
  double seconds = difftime(timer2, timer);
  cout<< "Time to compute = "<<seconds<<"s\n";
  mystartfile<<seconds<<endl;
  /*********************************************************************/
    
  /// CLOSE STREAMS
  mystartfile.close();
  myfile.close();
  
  myfoifile.close();
  myacqfile.close();myimpfile.close();
  myprevfile.close();
  myNfile.close();
  mypatients.close();
  
  //myenvfile.close();
  //myenvstayfile.close();
  //myenvdisfile.close();
  
  /*
  
  colstat.close();
  */
    
    return 0;
}
