library(Rcpp)
Sys.setenv("PKG_CXXFLAGS"="-fopenmp")
Sys.setenv("PKG_LIBS"="-fopenmp")

code='
#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>

// [[Rcpp::export]]
double long_computation_omp2(int nb, int threads=1) {
#ifdef _OPENMP
if ( threads > 0 )
omp_set_num_threads( threads );
REprintf("Number of threads=%i\\n", omp_get_max_threads());
#endif
Progress p(nb, true);
double sum = 0;
#pragma omp parallel for schedule(dynamic)   
for (int i = 0; i < nb; ++i) {
double thread_sum = 0;
if ( ! Progress::check_abort() ) {
  p.increment(); // update progress
  for (int j = 0; j < nb; ++j) {
    thread_sum += R::dlnorm(i+j, 0.0, 1.0, 0);
  }
}
sum += thread_sum;
}

return sum + nb;
}
'

sourceCpp(code=code)
s <- long_computation_omp2(10000, 4)
