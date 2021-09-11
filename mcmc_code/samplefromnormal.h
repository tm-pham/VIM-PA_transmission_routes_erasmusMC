#ifndef _DSS_SAMPLEFROMNORMAL_H_
#define _DSS_SAMPLEFROMNORMAL_H_
#include <boost/random/normal_distribution.hpp>
#include <boost/math/distributions/exponential.hpp>
//#include <boost/random.hpp>
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/variate_generator.hpp>
#include <boost/random/uniform_int_distribution.hpp>
#include <boost/random/gamma_distribution.hpp>
#include <boost/random/uniform_int.hpp>
#include <boost/random/uniform_real.hpp>
//#include <boost/random/uniform_real_distribution.hpp>


double samplefromnormal(double mean, double sigma,boost::mt19937 *ENG);

double samplefromgamma(double a, double b,boost::mt19937 *ENG);

double samplefromrealuniform(double startperiod, double endperiod, boost::mt19937 *ENG);

double samplefromexponential(double a, boost::mt19937 *ENG);

int samplefromintuniform(int startperiod, int endperiod, boost::mt19937 *ENG);

#endif //_DSS_SAMPLEFROMNORMAL_H_



