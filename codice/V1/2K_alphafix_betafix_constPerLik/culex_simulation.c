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

int main(int argc, char *argv[]){
	
	if(argc<15){
    printf("ERROR! \nusage: culex_simulation \n[1]seed \n[2]file_temperatura \n[3]file_catture \n[4]file_trappole \n[5]numero_cluster \n[6]anno_inizio \n[7]anno_fine \n[8]vettore_date_catture \n[9]numero_massimo_catture \n[10]nome_file_output_dynamics \n[11]nome_file_output_catture \n[12]numero_simulazioni \n[13]file_parametri \n[14]ncol_file_parametri \n[15]nrow_file_parametri \n[16]file_luce \n[17] solo_un_cluster \n[18] quale_cluster_singolo \n");

    exit(0);
  }

  FILE *fwrite_dynamics, *fwrite_catture;
  fwrite_dynamics=fopen(argv[10],"w");
  fwrite_catture=fopen(argv[11],"w");

  printf("Ho aperto il file per scrivere\n");

  setenv("GSL_RNG_SEED", argv[1] , 1);
  gsl_rng_env_setup();
  RNG = gsl_rng_alloc (gsl_rng_default);

  printf("Seed settato, si parte!\n");

  int i, j;

  int numero_catture, numero_cluster, anno_inizio, anno_fine, *giorni_catture;
  numero_catture=atoi(argv[9]);
  numero_cluster=atoi(argv[5]);

  anno_inizio=atoi(argv[6]);
  anno_fine=atoi(argv[7]);
  int totale_anni, *anni_selezionati;
  totale_anni=anno_fine-anno_inizio+1;
  anni_selezionati=(int*)calloc(totale_anni,sizeof(int));
  anni_selezionati[0]=anno_inizio;
  for(i=1;i<totale_anni;i++)
    anni_selezionati[i]=anni_selezionati[(i-1)]+1;

  giorni_catture=(int*)calloc(numero_catture,sizeof(int));
  read_vector_int(giorni_catture,argv[8]);

  minuti_luce=(double*)calloc(365,sizeof(double));
  read_vector(minuti_luce,argv[16]);
    
  printf("Letto numero catture, numero cluster, anno inizio, anno fine, vettore giorni catture\n");
      
  double **observed_captured_mosquitoes, **active_traps;
  observed_captured_mosquitoes=(double**)calloc(numero_cluster,sizeof(double*));
  active_traps=(double**)calloc(numero_cluster,sizeof(double*));
  for(i=0;i<numero_cluster;i++){
    observed_captured_mosquitoes[i]=(double*)calloc(numero_catture,sizeof(double));
    active_traps[i]=(double*)calloc(numero_catture,sizeof(double));
  }

  printf("Allocata tabella per catture osservate\n");

  char nome_file_catture[100], nome_file_trappole[100], nome_file_temperature[100], dumb_string[100];

  double *simulated_captures;
  simulated_captures=(double*)calloc(numero_catture,sizeof(double));
      
  temperatures=(double**)calloc(numero_cluster,sizeof(double*));
  for(i=0;i<numero_cluster;i++){
    temperatures[i]=(double*)calloc(366,sizeof(double));
  }

  int switch_trappole_aperte, counter_for_capture;
  simulated_captures=(double*)calloc(numero_catture,sizeof(double*));

  int numero_simulazione, max_simulazioni;
  max_simulazioni=atoi(argv[12]);
  printf("Faccio %d simulazioni\n",max_simulazioni);

  double **tabella_output;
  tabella_output=(double**)calloc(numero_classipop_da_scrivere,sizeof(double*));
  for(i=0;i<numero_classipop_da_scrivere;i++)
    tabella_output[i]=(double*)calloc(tmax,sizeof(double));

  double **parametri;
  int nrow_parametri, ncol_parametri;
  ncol_parametri=atoi(argv[14]);
  nrow_parametri=atoi(argv[15]);
  parametri=(double**)calloc(nrow_parametri,sizeof(double*));
  for(i=0;i<nrow_parametri;i++)
    parametri[i]=(double*)calloc(ncol_parametri,sizeof(double));
  read_table_double(parametri,argv[13],nrow_parametri,ncol_parametri);
  printf("Letti i parametri\n");

  fflush(stdout);

  double *y;
  y=(double *)calloc(n_eq,sizeof(double));

  int cluster_iniziale, cluster_finale, totale_cluster_simulati;
  cluster_iniziale=0;
  cluster_finale=numero_cluster;
  if(atoi(argv[17])==1){
    cluster_iniziale=atoi(argv[18]);
    cluster_finale=atoi(argv[18])+1;
  }
  totale_cluster_simulati=cluster_finale-cluster_iniziale;

  int indice_anno, indice_cluster, selezione;

  printf("sto per simulare\n");

  for(numero_simulazione=0; numero_simulazione<max_simulazioni; numero_simulazione++){
    
  	selezione=gsl_rng_uniform_int(RNG, nrow_parametri);

    for(indice_anno=0;indice_anno<totale_anni;indice_anno++){
      strcpy(dumb_string,argv[3]);
      strcat(dumb_string,"%d");
      sprintf(nome_file_catture,dumb_string,anni_selezionati[indice_anno]);
      read_table_double(observed_captured_mosquitoes,nome_file_catture, numero_cluster, numero_catture);
      strcpy(dumb_string,argv[4]);
      strcat(dumb_string,"%d");
      sprintf(nome_file_trappole,dumb_string,anni_selezionati[indice_anno]);
      read_table_double(active_traps,nome_file_trappole, numero_cluster, numero_catture);
      strcpy(dumb_string,argv[2]);
      strcat(dumb_string,"%d");
      sprintf(nome_file_temperature,dumb_string,anni_selezionati[indice_anno]);
      read_table_double(temperatures,nome_file_temperature, numero_cluster, 365);
      for(indice_cluster=cluster_iniziale;indice_cluster<cluster_finale;indice_cluster++){

        for(i=0;i<n_eq;i++)
          y[i]=0;

        y[3]=parametri[selezione][2];
        counter_for_capture=0;

        for(i=0;i<numero_catture;i++)
          simulated_captures[i]=0.;

        for(i=0;i<numero_classipop_da_scrivere;i++)
          for(j=0;j<tmax;j++)
            tabella_output[i][j]=0.;

        for(j=0;j<numero_classipop_da_scrivere;j++)
          tabella_output[j][0]=y[j];

        for (i=0; i<tmax; i++){
          switch_trappole_aperte=0.;
          if(giorni_catture[counter_for_capture]==i+t_start_simulation)
            if(active_traps[indice_cluster][counter_for_capture]==1)
              switch_trappole_aperte=1.;

          if(i<=t_switch_carrying_capacity){
          	carrying_capacity_for_equations=parametri[selezione][0];
          	capture_rate_for_equations=capture_rate*switch_trappole_aperte;
          }
          else{
          	carrying_capacity_for_equations=parametri[selezione][1];
          	capture_rate_for_equations=capture_rate*switch_trappole_aperte;
          }

          
          double t;
          t=i;  
          simulate_culex(indice_cluster, &t, i, y);

          if(switch_trappole_aperte==1){
            simulated_captures[counter_for_capture]=gsl_ran_poisson(RNG, capture_rate_for_equations*dA*y[3]);
            //printf("Ho catturato %e zanzare di %e con capture rate %e e dA=%e\n", simulated_captures[counter_for_capture], y[3], capture_rate_for_equations, dA);
            if(simulated_captures[counter_for_capture]>y[3])
              simulated_captures[counter_for_capture]=y[3];
            y[3]=y[3]-simulated_captures[counter_for_capture];
          }
          if(giorni_catture[counter_for_capture]==i+t_start_simulation)
            counter_for_capture=counter_for_capture+1;

          tabella_output[0][i]=y[0];
          tabella_output[1][i]=y[1];
          tabella_output[2][i]=y[2];
          tabella_output[3][i]=y[3];
        }
        //printf("Ho fatto la simulazione %d\n",numero_simulazione);
        fflush(stdout);

        for(i=0;i<numero_classipop_da_scrivere;i++){
          for(j=0;j<tmax;j++)
            fprintf(fwrite_dynamics, "%d\t", (int)tabella_output[i][j]);
          fprintf(fwrite_dynamics,"\n");
        }
        for(i=0;i<numero_catture;i++){
          if(active_traps[indice_cluster][i]!=0)
            fprintf(fwrite_catture, "%d\t", (int)simulated_captures[i]);
          else
            fprintf(fwrite_catture, "NA\t");
        }
        fprintf(fwrite_catture,"\n");
      }
    }
  }   
}

