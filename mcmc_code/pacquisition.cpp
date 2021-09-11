#include <algorithm>
#include <math.h>
#include "stdhdr.h"
#include <boost/math/special_functions/gamma.hpp>

using namespace std;
// ADDED environmental contamination E
// ADDED exact computation of probabilities


inline void pacquisitionbyroute(parameters param, int I, int N, double E, double &endogenous, double &exogenous, double &env, double &pinfection, double error){
  long double A,B,D,EE,F,x,foi,diff,current;
  unsigned long long int fak = 1;
  unsigned int i=1;
  double cases = 0; double test1= 0.0, test2 =0.0;

  diff = ((param.nu/param.mu)*(double)I/(double)N-E);
  A = param.a + (param.b + param.c*param.nu/param.mu)*(double)I/(double)N;
  B = (param.c/param.mu)*diff;
  if(diff>0){
	cases = 1.0;
	  //cout << "Case diff >0     ";
	EE = (1.0/param.mu)*exp(B)*pow(B,-A/param.mu);
	//cout <<"A="<<A<<"; B="<<B<<"; EE="<<EE<<"; I="<<I<<"; N="<<N<<"; E="<<E<<"; mu="<<param.mu<<"; gamma="<<param.c<<"\n";
	x = (boost::math::tgamma_lower(A/param.mu, B) - boost::math::tgamma_lower(A/param.mu, B*exp(-param.mu)));
  }else{
    if(diff<0){
		cases = 2.0;
		//cout << "Case diff <0     ";
	  	EE = exp(B)/param.mu;
	  	x = (param.mu/A)*(1.0-exp(-A));

	  	for(i=1; i<10; i++){
			fak *= i;
			x+=(pow(-B, i)/(double)fak)*(1.0-exp(-A-param.mu*i))*(1.0/(A/param.mu+i));
		}

	  	do{
			//cout << "fak=" << fak << endl;
			i+=1; fak *= i;
			current = (pow(-B, i)/(double)fak)*(1.0-exp(-A-param.mu*i))*(1.0/(A/param.mu+i));
			x+=current;
			//cout << "i = " << i << endl;
			//cout << "current = " << current << endl;
			//cout << "x = " << x << endl;
			if(isinf(current)){
				cout << "ERROR: current is infinite! " << current << endl;
				cout << "A=" << A << ", B="<< B << ", mu=" << param.mu << ", fak=" << fak << endl;
				//exit(0);
			}
		} while(abs(current) > error && i < 50);

	}else{
		cases = 3.0;
	  //cout << "Case diff =0    ";
	  x = (1.0/A)*(1.0-exp(-A));
	  EE = 1.0;
	}
  }
  endogenous = param.a*EE*x;
  exogenous = param.b*((double)I/(double)N)*EE*x;
  foi = A+B*(exp(-param.mu)-1.0);
  pinfection = 1.0-exp(-foi);
  env = pinfection - endogenous - exogenous;
  if(env < 0) {
	  cout << "cases = " << cases << endl;
	  cout << "env<0: A=" << A << ", B="<< B << ", mu=" << param.mu << ", EE=" << EE << ", x="<<x << endl;
	  test1 = boost::math::tgamma_lower(A/param.mu, B); test2 = boost::math::tgamma_lower(A/param.mu, B*exp(-param.mu));
	  cout << "test1 = " << test1 << ", test2 = " << test2 << endl;
	  cout << "endogenous=" << endogenous << ", exogenous=" << exogenous << ", env=" << env << ", pinfection=" << pinfection << endl;
	  //exit(0);
  }

}

inline void foi1(parameters param,int I,int N,double E,int addingenvironment,double &foi){
  if(param.mu==0.0 && param.a==0.0 && param.b==0.0 && param.c==0.0){
	foi = 0.0;
  } else{
  double A, B;
  double diff = ((param.nu/param.mu)*(double)I/(double)N-E);

  A = param.a + (param.b + (double)addingenvironment*param.c*param.nu/param.mu)*(double)I/(double)N;
  B = (param.c/param.mu)*diff;
  foi = A+(double)addingenvironment*B*(exp(-param.mu)-1.0);
  if(isnan(A) || isnan(B) || isnan(foi)){
	cout << "E="<<E<<"; A="<<A<<"; B="<<B<<"; foi="<<foi<<"; param.a="<<param.a<<"; N="<<N<<endl;
	cout << "param.b="<<param.b<<" ;param.c="<<param.c<<"; param.nu="<<param.nu<<"; param.mu="<<param.mu<<"; I="<<I<<endl;
	exit(0);
  }
  }
}

inline void foi0(parameters param,int I,int N,double E,int addingenvironment,double &foi){
	double A, B;
	double diff = ((param.nu/param.mu)*(double)I-E);

	A = param.a + (param.b + (double)addingenvironment*param.c*param.nu/param.mu)*(double)I;
	B = (param.c/param.mu)*diff;
	foi = A+(double)addingenvironment*B*(exp(-param.mu)-1.0);
}

double pacquisition(parameters param,int I,int N,double E,int addingenvironment)
{
	double foi;
    if(param.frequencydependence==1)  foi1(param,I,N,E,addingenvironment,foi);
    if(param.frequencydependence==0)  foi0(param,I,N,E,addingenvironment,foi);
    //cout << "Result in pacquisition = " << 1.0 -exp(-foi) << endl;
    return 1.0 -exp(-foi);
}

double pnoacquisition(parameters param,int I,int N,double E,int addingenvironment)
{
	double foi;
    if(param.frequencydependence==1)  foi1(param,I,N,E,addingenvironment,foi);
    if(param.frequencydependence==0)  foi0(param,I,N,E,addingenvironment,foi);
    //cout << "Result in pnoacquisition = " << exp(-foi) << endl;
    return exp(-foi);
}

double logpacquisition(parameters param,int I,int N,double E,int addingenvironment)
{
	double foi;
    if(param.frequencydependence==1)  foi1(param,I,N,E,addingenvironment,foi);
    if(param.frequencydependence==0)  foi0(param,I,N,E,addingenvironment,foi);
    //cout << "FOI in logpacquisition = " << foi << endl;
    //cout << "Result in logpacquisition = " << log(1.0 -exp(-foi)) << endl;
	if(foi == 0.0){
	    return -1000000.0;
	}
	double logl = log(1.0 -exp(-foi));
    if(isnan(foi) || isinf(foi) || isnan(logl) || isinf(-foi) || isinf(-logl)){
		cout <<" foi="<<foi<<"; logl = "<< logl<< endl;
	}
    return log(1.0 -exp(-foi));
}

double logpnoacquisition(parameters param,int I,int N,double E,int addingenvironment)
{
	double foi;
    if(param.frequencydependence==1)  foi1(param,I,N,E,addingenvironment,foi);
    if(param.frequencydependence==0)  foi0(param,I,N,E,addingenvironment,foi);
    //cout << "FOI in logpnoacquisition = " << foi << endl;
    //cout << "Result in logpnoacquisition = " << -foi << endl;
    if(isnan(foi) || isinf(foi)){
		cout <<" foi="<<foi<<endl;
	}
    return -foi;
}


/*
double pacquisition(parameters param,int I,int N,double E,int addingenvironment)
{
	double endogenous=0.0, exogenous=0.0, env=0.0, pinfection=0.0;
	pacquisitionbyroute(param, I, N, E, endogenous, exogenous, env, pinfection);
    return pinfection;
}

double pnoacquisition(parameters param,int I,int N,double E,int addingenvironment)
{
	double endogenous=0.0, exogenous=0.0, env=0.0, pinfection=0.0;
	pacquisitionbyroute(param, I, N, E, endogenous, exogenous, env, pinfection);
    return 1.0- pinfection;
}

double logpacquisition(parameters param,int I,int N,double E,int addingenvironment)
{
	double endogenous=0.0, exogenous=0.0, env=0.0, pinfection=0.0;
	pacquisitionbyroute(param, I, N, E, endogenous, exogenous, env, pinfection);
    return log(pinfection);
}

double logpnoacquisition(parameters param,int I,int N,double E,int addingenvironment)
{
	double endogenous=0.0, exogenous=0.0, env=0.0, pinfection=0.0;
	pacquisitionbyroute(param, I, N, E, endogenous, exogenous, env, pinfection);
    return log(1.0- pinfection);
}
* */

// Adjustments for pacquisitionward still missing!

// Simple computation
/*
double pacquisition(parameters param,int I,int N,double E,int addingenvironment)
{
    if(param.frequencydependence==1)  return 1.0-exp(-param.a-param.b*(double)I/(double)N-(double)addingenvironment*param.c*E);
    if(param.frequencydependence==0)  return 1.0-exp(-param.a-param.b*(double)I-(double)addingenvironment*param.c*E);
}

double pnoacquisition(parameters param,int I,int N,double E,int addingenvironment)
{
    if(param.frequencydependence==1) return exp(-param.a-param.b*(double)I/(double)N-(double)addingenvironment*param.c*E);
    if(param.frequencydependence==0) return exp(-param.a-param.b*(double)I-(double)addingenvironment*param.c*E);
}

double logpacquisition(parameters param,int I,int N,double E,int addingenvironment)
{
    if(param.frequencydependence==1)return log(1.0-exp(-param.a-param.b*(double)I/(double)N-(double)addingenvironment*param.c*E));
    if(param.frequencydependence==0)return log(1.0-exp(-param.a-param.b*(double)I-(double)addingenvironment*param.c*E));
}

double logpnoacquisition(parameters param,int I,int N,double E,int addingenvironment)
{
    if(param.frequencydependence==1)return -param.a-param.b*(double)I/(double)N-(double)addingenvironment*param.c*E;
    if(param.frequencydependence==0)return -param.a-param.b*(double)I-(double)addingenvironment*param.c*E;
}
*/

double pacquisitionward(parameters param,int I,int N,int unit,double E,int addingenvironment)
{
    if(param.frequencydependence==1)return 1.0-exp(-param.alphavector.at(unit)-param.betavector.at(unit)*(double)I/(double)N-(double)addingenvironment*param.c*E);
    if(param.frequencydependence==0)return 1.0-exp(-param.alphavector.at(unit)-param.betavector.at(unit)*(double)I-(double)addingenvironment*param.c*E);
}

double pnoacquisitionward(parameters param,int I,int N,int unit,double E,int addingenvironment)
{
    if(param.frequencydependence==1)return exp(-param.alphavector.at(unit)-param.betavector.at(unit)*(double)I/(double)N-(double)addingenvironment*param.c*E);
    if(param.frequencydependence==0)return exp(-param.alphavector.at(unit)-param.betavector.at(unit)*(double)I-(double)addingenvironment*param.c*E);
}

double logpacquisitionward(parameters param,int I,int N,int unit,double E,int addingenvironment)
{
    if(param.frequencydependence==1)return log(1.0-exp(-param.alphavector.at(unit)-param.betavector.at(unit)*(double)I/(double)N-(double)addingenvironment*param.c*E));
    if(param.frequencydependence==0)return log(1.0-exp(-param.alphavector.at(unit)-param.betavector.at(unit)*(double)I-(double)addingenvironment*param.c*E));
}

double logpnoacquisitionward(parameters param,int I,int N,int unit,double E,int addingenvironment)
{
    if(param.frequencydependence==1)return -param.alphavector.at(unit)-param.betavector.at(unit)*(double)I/(double)N-(double)addingenvironment*param.c*E;
    if(param.frequencydependence==0)return -param.alphavector.at(unit)-param.betavector.at(unit)*(double)I-(double)addingenvironment*param.c*E;
}

// OLD: DO NOT DELETE
double pacquisition(parameters param,int I,int N)
{
    if(param.frequencydependence==1)  return 1.0-exp(-param.a-param.b*(double)I/(double)N);
    if(param.frequencydependence==0)  return 1.0-exp(-param.a-param.b*(double)I);
}

double pnoacquisition(parameters param,int I,int N)
{
    if(param.frequencydependence==1) return exp(-param.a-param.b*(double)I/(double)N);
    if(param.frequencydependence==0) return exp(-param.a-param.b*(double)I);
}

double logpacquisition(parameters param,int I,int N)
{
    if(param.frequencydependence==1)return log(1.0-exp(-param.a-param.b*(double)I/(double)N));
    if(param.frequencydependence==0)return log(1.0-exp(-param.a-param.b*(double)I));
}

double logpnoacquisition(parameters param,int I,int N)
{
    if(param.frequencydependence==1)return -param.a-param.b*(double)I/(double)N;
    if(param.frequencydependence==0)return -param.a-param.b*(double)I;
}


double pacquisitionward(parameters param,int I,int N,int unit)
{
    if(param.frequencydependence==1)return 1.0-exp(-param.alphavector.at(unit)-param.betavector.at(unit)*(double)I/(double)N);
    if(param.frequencydependence==0)return 1.0-exp(-param.alphavector.at(unit)-param.betavector.at(unit)*(double)I);
}

double pnoacquisitionward(parameters param,int I,int N,int unit)
{
    if(param.frequencydependence==1)return exp(-param.alphavector.at(unit)-param.betavector.at(unit)*(double)I/(double)N);
    if(param.frequencydependence==0)return exp(-param.alphavector.at(unit)-param.betavector.at(unit)*(double)I);
}

double logpacquisitionward(parameters param,int I,int N,int unit)
{
    if(param.frequencydependence==1)return log(1.0-exp(-param.alphavector.at(unit)-param.betavector.at(unit)*(double)I/(double)N));
    if(param.frequencydependence==0)return log(1.0-exp(-param.alphavector.at(unit)-param.betavector.at(unit)*(double)I));
}

double logpnoacquisitionward(parameters param,int I,int N,int unit)
{
    if(param.frequencydependence==1)return -param.alphavector.at(unit)-param.betavector.at(unit)*(double)I/(double)N;
    if(param.frequencydependence==0)return -param.alphavector.at(unit)-param.betavector.at(unit)*(double)I;
}
