#ifndef _DSS_PACQUISITION_H_
#define _DSS_PACQUISITION_H_

#include <algorithm>
#include <math.h>
#include "stdhdr.h"
#include <boost/math/special_functions/gamma.hpp>

using namespace std;

inline void pacquisitionbyroute(parameters param, int I, int N, double E, double &endogenous, double &exogenous, double &env, double &pinfection, double error);
inline void foi1(parameters param,int I,int N,double E,int addingenvironment,double &foi);
inline void foi0(parameters param,int I,int N,double E,int addingenvironment,double &foi);

double pacquisition(parameters param,int I,int N,double E, int addingenvironment);
double pnoacquisition(parameters param,int I,int N,double E, int addingenvironment);
double logpacquisition(parameters param,int I,int N,double E, int addingenvironment);
double logpnoacquisition(parameters param,int I,int N,double E, int addingenvironment);

double pacquisitionward(parameters param,int I,int N,int unit,double E, int addingenvironment);
double pnoacquisitionward(parameters param,int I,int N,int unit,double E, int addingenvironment);
double logpacquisitionward(parameters param,int I,int N,int unit,double E, int addingenvironment);
double logpnoacquisitionward(parameters param,int I,int N,int unit,double E, int addingenvironment);

double pacquisition(parameters param,int I,int N);
double pnoacquisition(parameters param,int I,int N);
double logpacquisition(parameters param,int I,int N);
double logpnoacquisition(parameters param,int I,int N);

double pacquisitionward(parameters param,int I,int N,int unit);
double pnoacquisitionward(parameters param,int I,int N,int unit);
double logpacquisitionward(parameters param,int I,int N,int unit);
double logpnoacquisitionward(parameters param,int I,int N,int unit);

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

inline void foi1(parameters param,int I,int N,double E,int addingenvironment, double &foi){
  if(param.mu==0.0 && param.a==0.0 && param.b==0.0 && param.c==0.0){
	foi = 0.0;
  } else{
  double A, B; 
  double diff = ((param.nu/param.mu)*(double)I/(double)N-E);
  //double diff = ((param.nu)*(double)I/(double)N-E);
  
  A = param.a + (param.b + (double)addingenvironment*param.c*param.nu/param.mu)*(double)I/(double)N;
  //A = param.a + (param.b + (double)addingenvironment*param.c*param.nu)*(double)I/(double)N;
  B = (param.c/param.mu)*diff;
  foi = A+(double)addingenvironment*B*(exp(-param.mu)-1.0);
  if(isnan(A) || isnan(B) || isnan(foi)){
	cout << "E="<<E<<"; A="<<A<<"; B="<<B<<"; foi="<<foi<<"; param.a="<<param.a<<"; N="<<N<<endl;
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


#endif //_DSS_PACQUISITION_H_
