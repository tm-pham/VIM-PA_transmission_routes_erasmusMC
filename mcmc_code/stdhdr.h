#ifndef DSS_STDHDR_H
#define DSS_STDHDR_H
#include <string>
#include <sstream>
#include <iostream>
#include <vector>
#include <fstream>
using namespace std;


typedef struct
       {
         double a; //endogenous rate
         double b; // b*I/N = cross transmission rate
         double f; // P(colonized on admission)
         double phi;// P(culture false negative)
	 double c; //environmental contribution rate
	 double nu; // shedding rate of patients
	 double mu; // Clearence rate of bacteria from the environment
	 // double xi; // Shedding rate of prior bed occupants
	 double E0; // Initial bacerial load
         int frequencydependence;

         vector<double> betavector;
         vector<double> alphavector;
	 vector<double> gammavector;
 	 vector<double> nuvector;
	 vector<double> muvector;
         vector<double> covariatesparaters;
       }parameters;




#endif //_DSS_STDHDR_H_
