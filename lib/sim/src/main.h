#ifdef CPPOUTPUT
extern "C"
{
#endif //CPPOUTPUT

#ifndef MAIN_H
#define MAIN_H

#if defined(DLL_EXPORTS)
# define DLL_API __declspec( dllexport )
#elif defined(DLL_IMPORTS)
# define DLL_API __declspec( dllimport )
#else
# define DLL_API
#endif

DLL_API void run_sim_main (
  const char *herd_file,
  const char *parameter_file,
  const char *output_file,
  double fixed_rng_value,
  int verbosity,
  int seed
);

#endif // MAIN_H

#ifdef CPPOUTPUT
}
#endif //CPPOUTPUT

