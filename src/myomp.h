#ifdef _OPENMP
  #include <omp.h>
  #if _OPENMP >= 201511
    #define monotonic_dynamic monotonic:dynamic // #4786
  #else
    #define monotonic_dynamic dynamic
  #endif
#else
  // for machines with compilers void of openmp support
  #define omp_get_num_threads()  1
  #define omp_get_thread_num()   0
  #define omp_get_max_threads()  1
  #define omp_get_thread_limit() 1
  #define omp_get_num_procs()    1
  #define omp_set_nested(a)   // empty statement to remove the call
  #define omp_get_wtime()        0
  #define MY_OPENMP              0
#endif

