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


double samplefromnormal(double mean, double sigma,boost::mt19937 *ENG)
{
  boost::normal_distribution<double> norm_dist(mean, sigma);
  const double value = norm_dist.operator () ((*ENG));
  return value;
}

double samplefromgamma(double a, double b,boost::mt19937 *ENG)
{
  boost::gamma_distribution<double> gamma_dist(a,b);
  const double value = gamma_dist.operator () ((*ENG));
  return value;
}

double samplefromrealuniform(double startperiod, double endperiod, boost::mt19937 *ENG)
{
  boost::uniform_real<double> uniform_real(startperiod,endperiod);
  const double value = uniform_real.operator () ((*ENG));
  return value;
}

double samplefromexponential(double a, boost::mt19937 *ENG)
{
  boost::exponential_distribution<double> exp_dist(a);
  const double value = exp_dist.operator () ((*ENG));
  return value;
}

int samplefromintuniform(int startperiod, int endperiod, boost::mt19937 *ENG)
{
  boost::uniform_int<int> rg(startperiod,endperiod);
  const double value = rg.operator () ((*ENG));
  return value;
}

