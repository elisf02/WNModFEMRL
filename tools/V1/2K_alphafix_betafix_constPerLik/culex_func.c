#include <stdio.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_odeiv2.h>
#include <stdio.h>
#include <stdlib.h>
#include "culex_header.h"
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <math.h>
#include <string.h>

int eq_sys (double t, const double y[], double f[], void *params){
  
  f[0]=nE*dA*y[3]-(muE+tauE)*y[0];
  f[1]=tauE*y[0]-(tauL+muL*(1+(y[1]/carrying_capacity_for_equations)))*y[1];
  f[2]=tauL*y[1]-(tauP+muP)*y[2];
  f[3]=(1.-diap)*0.5*tauP*y[2]-muA*y[3];
  
  return GSL_SUCCESS;
}

void set_mosquito_parameters(double T, double L){
	nE=200.;//Marini et al 2016, Ewing et al. 2016

  dA=0.;
  if(T>=10)
    dA=0.122*pow(log10(T-9.),1.76); //Ewing et al. 2016
	//dA=0.2;

  double threshold1, threshold2, ni0, ni1, ni2;
  ni0=0.095;
  ni1=22.88;
  ni2=7.;
  threshold1=ni1+ni2/2*log(1/ni0);
  threshold2=ni1-ni2/2*log(1/ni0);

  if(T>threshold2 & T<threshold1)
    muE=ni0*exp(((T-ni1)/ni2)*((T-ni1)/ni2)) ;//Ewing et al. 2016
  else
    muE=1;

  double a, b, b_m;
  a=2.2*pow(10,-3);
  b=1.77;
  b_m=1/60.;
  threshold1=pow((b_m/a),(1./b));
  if(T>threshold1)
    tauE=a*pow(T,b);
  else
    tauE=b_m;

	tauL=1./(93.24-6.83*T+0.13*(T*T));//miei calcoli da Loetti et al. 2011

  muL=2.727219e-02+3.058728e-09*exp(6.387534e-01*T);//miei calcoli da Loetti et al. 2011

  if(muL>1)
    muL=1;

  tauP=1./(20.174*exp(-0.096*T));//Marini et al. 2016

  muP=2.083669e-05*pow((T-2.078279e+01),4)+8.769783e-02;//Marini et al. 2016

  if(muP>1)
    muP=1;

  muA=beta/(151.6-4.57*T);
  if(muA<0)
    muA=0.;
  if(muA>1)
    muA=1.;

  a=829.85565352;
  b=30.07302372;
  double c;
  c=0.50152488;

  diap=-1+(1+exp((a-L)/b))/(1+c*exp((a-L)/b));//Marini et al. 2016
}

void simulate_culex(int indice_cluster, double *t, int day, double *y){
  
	double T, L;
	T=temperatures[indice_cluster][day+t_start_simulation];
  L=minuti_luce[day+t_start_simulation];

  //printf("Giorno %d, T=%e, L=%e \n", day+t_start_simulation, T, L);

	set_mosquito_parameters(T, L);

  if(day+t_start_simulation<180)
    diap=0;
	
	double h;
	int j,s;

	const gsl_odeiv2_step_type *Typ = gsl_odeiv2_step_rkf45;
	gsl_odeiv2_step *ste = gsl_odeiv2_step_alloc(Typ, n_eq);
	gsl_odeiv2_control *con = gsl_odeiv2_control_y_new(0.5, 1e-3);
	gsl_odeiv2_evolve *evo = gsl_odeiv2_evolve_alloc(n_eq);

	double *param;
	gsl_odeiv2_system sys = { eq_sys, NULL, n_eq, param};

	h=0.0000001;
	double tend=day+1;
	while(*t<tend){
		s=gsl_odeiv2_evolve_apply(evo, con, ste, &sys, t, tend, &h, y);

		for(j=0; j<n_eq; j++)
			if(y[j]<0)
				y[j]=0;


			if (s != GSL_SUCCESS){
				fprintf (stderr, "error: driver returned %d\n", s);
				fprintf (stderr, "%f\t%d\n", y[0], day);
				exit(1);
			}
	}
	gsl_odeiv2_evolve_free(evo);
	gsl_odeiv2_control_free(con);
	gsl_odeiv2_step_free(ste);

	return;
}

void read_table_double(double **data,char *file_name, int nrow, int ncol){
  FILE *fp;
  int i,j;
  fp=fopen(file_name,"r");
  for(i=0;i<nrow;i++){
    char *line=NULL;
    size_t len = 0;
    getline(&line, &len, fp);
    for(j=0;j<ncol;j++){
      sscanf(line,"%lf", &data[i][j]);
      line = (char *)strchr(line, ' ');/*tra le virgolette devi mettere il separatore*/
      line++;
    }
  }
  fclose(fp); 
return;		
}

void read_table_double_tab(double **data,char *file_name, int nrow, int ncol){
  FILE *fp;
  int i,j;
  fp=fopen(file_name,"r");
  for(i=0;i<nrow;i++){
    char *line=NULL;
    size_t len = 0;
    getline(&line, &len, fp);
    for(j=0;j<ncol;j++){
      sscanf(line,"%lf", &data[i][j]);
      line = (char *)strchr(line, '\t');/*tra le virgolette devi mettere il separatore*/
      line++;
    }
  }
  fclose(fp); 
return;		
}

void read_vector(double *data,char *file_name){
  FILE *fp;
  char * stringa;
  stringa=(char *)calloc(100,sizeof(char));
  fp=fopen(file_name,"r");
  int n;
  n=0;
  while (fscanf(fp, "%s", stringa)!=EOF){
    data[n++]=atof(stringa);
  }
  fclose(fp);
  free(stringa);
  return;

}

void read_vector_int(int *data,char *file_name){
  FILE *fp;
  char * stringa;
  stringa=(char *)calloc(100,sizeof(char));
  fp=fopen(file_name,"r");
  int n;
  n=0;
  while (fscanf(fp, "%s", stringa)!=EOF){
    data[n++]=atoi(stringa);
  }
  fclose(fp);
  free(stringa);
  return;

}
