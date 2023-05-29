#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

gsl_rng *RNG;

double **temperatures, *minuti_luce;

double *carrying_capacity1, *carrying_capacity2, *initial_adults, capture_rate_for_equations, carrying_capacity_for_equations;

int indice_parametri;

void read_vector(double *data,char *file_name);
void read_vector_int(int *data,char *file_name);
void read_table_double(double **data,char *file_name, int nrow, int ncol);
void read_table_double_tab(double **data,char *file_name, int nrow, int ncol);

#define defined_initial_adults 50
#define t_start_simulation 90
#define tmax 210

double nE, dA, muE, tauE, tauL, muL, tauP, muP, muA, diap;

void set_mosquito_parameters(double T, double L);
void simulate_culex(int indice_comune, double *t, int day, double *y);

double capture_rate;

#define n_eq 4

#define numero_classipop_da_scrivere 4

#define threshold_per_aggregare 3

#define t_switch_carrying_capacity 90

#define capture_rate 0.108 //Li et al. 2016, 3.29^2/100
#define beta 4.61

#define costante_per_lik 1
