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
	
	if(argc<8){
    		printf("ERROR! \nusage: culex_mcmc \n[1]seed \n[2]file_temperatura \n[3]file_catture \n[4]file_trappole \n[5]numero_cluster \n[6]anno_inizio \n[7]anno_fine \n[8]vettore_date_catture \n[9]numero_massimo_catture \n[10]nome_file_per_salvare_parametri \n[11]max_iter_MCMC \n[12]sigma_big \n[13]sigma_small \n[14]file_luce \n[15] solo_un_cluster \n[16] quale_cluster_singolo \n");

    		exit(0);
  	}
  	
  	FILE *fwrite_params;
    const char *ciaone = NULL;
    ciaone=argv[10];
  	fwrite_params=fopen(ciaone,"w");
  	
  	printf("Ho aperto il file per scrivere\n");
  	
	setenv("GSL_RNG_SEED", argv[1] , 1);
  	gsl_rng_env_setup();
  	RNG = gsl_rng_alloc (gsl_rng_default);
  	
  	printf("Seed settato, si parte!\n");

  	int i,j;
  	
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
  	read_vector(minuti_luce,argv[14]);
  	
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

    double *simulated_captures, *simulated_aggregated_captures, *observed_aggregated_captures;
    simulated_captures=(double*)calloc(numero_catture,sizeof(double));
    simulated_aggregated_captures=(double*)calloc(numero_catture,sizeof(double));
    observed_aggregated_captures=(double*)calloc(numero_catture,sizeof(double));
    	
    temperatures=(double**)calloc(numero_cluster,sizeof(double*));
    for(i=0;i<numero_cluster;i++){
  		temperatures[i]=(double*)calloc(366,sizeof(double));
  	}

    int switch_trappole_aperte, counter_for_capture;
    long double l0, best_lik, lik_tmp;
    best_lik=0.;
    	
    double *y;
    y=(double *)calloc(n_eq,sizeof(double));

    int indice_anno, indice_cluster;
    indice_parametri=0;

    int cluster_iniziale, cluster_finale, totale_cluster_simulati;
    cluster_iniziale=0;
    cluster_finale=numero_cluster;
    if(atoi(argv[15])==1){
    	cluster_iniziale=atoi(argv[16]);
    	cluster_finale=atoi(argv[16])+1;
    }
    totale_cluster_simulati=cluster_finale-cluster_iniziale;


    carrying_capacity1=(double*)calloc(totale_cluster_simulati*totale_anni,sizeof(double));
    carrying_capacity2=(double*)calloc(totale_cluster_simulati*totale_anni,sizeof(double));
    initial_adults=(double*)calloc(totale_cluster_simulati*totale_anni,sizeof(double));
    double *best_carrying_capacity1, *best_carrying_capacity2, *best_initial_adults, *totale_catture, *totale_catture_normalizzato;
    best_carrying_capacity1=(double*)calloc(totale_cluster_simulati*totale_anni,sizeof(double));
    best_carrying_capacity2=(double*)calloc(totale_cluster_simulati*totale_anni,sizeof(double));
    best_initial_adults=(double*)calloc(totale_cluster_simulati*totale_anni,sizeof(double));
    totale_catture=(double*)calloc(totale_cluster_simulati*totale_anni,sizeof(double));
    totale_catture_normalizzato=(double*)calloc(totale_cluster_simulati*totale_anni,sizeof(double));

    
    printf("\nInizio il loop\n");
    fflush(stdout);
    int n_loop, iterazioni_starting_point;
    iterazioni_starting_point=0;
    while(iterazioni_starting_point<5000){
    	iterazioni_starting_point++;

    	l0=1.;
    	indice_parametri=0;
    	for(indice_anno=0;indice_anno<totale_anni;indice_anno++){
    		//printf("Simulo l'anno %d\n",anni_selezionati[indice_anno]);
    		strcpy(dumb_string,argv[3]);
    		strcat(dumb_string,"%d");
    		sprintf(nome_file_catture,dumb_string,anni_selezionati[indice_anno]);
    		//puts(nome_file_catture);
    		read_table_double(observed_captured_mosquitoes,nome_file_catture, numero_cluster, numero_catture);
    		strcpy(dumb_string,argv[4]);
    		strcat(dumb_string,"%d");
    		sprintf(nome_file_trappole,dumb_string,anni_selezionati[indice_anno]);
    		//puts(nome_file_trappole);
    		read_table_double(active_traps,nome_file_trappole, numero_cluster, numero_catture);
    		strcpy(dumb_string,argv[2]);
    		strcat(dumb_string,"%d");
    		sprintf(nome_file_temperature,dumb_string,anni_selezionati[indice_anno]);
    		//puts(nome_file_temperature);
    		read_table_double(temperatures,nome_file_temperature, numero_cluster, 365);
    		for(indice_cluster=cluster_iniziale;indice_cluster<cluster_finale;indice_cluster++){
				//printf("\nSimulo il cluster %d\n",indice_cluster+1);
    			//carrying_capacity_for_equations=carrying_capacity*totale_catture_normalizzato[indice_parametri];
    			initial_adults[indice_parametri]=pow(10,gsl_ran_flat(RNG, 0, 3));
    			carrying_capacity1[indice_parametri]=pow(10,gsl_ran_flat(RNG, 0, 5));
    			carrying_capacity2[indice_parametri]=pow(10,gsl_ran_flat(RNG, 0, 5));
    			
				for(i=0;i<n_eq;i++)
    				y[i]=0.;
    			for(i=0;i<numero_catture;i++)
    				simulated_captures[i]=0.;

    			y[3]=initial_adults[indice_parametri];
    			counter_for_capture=0;

    			for (i=0; i<tmax; i++){

    				switch_trappole_aperte=0.;
    				if(giorni_catture[counter_for_capture]==i+t_start_simulation)
    					if(active_traps[indice_cluster][counter_for_capture]==1)
    						switch_trappole_aperte=1.;


    				if(i<=t_switch_carrying_capacity){
    					carrying_capacity_for_equations=carrying_capacity1[indice_parametri];
    					capture_rate_for_equations=capture_rate*switch_trappole_aperte;
    				}
    				else{
    					carrying_capacity_for_equations=carrying_capacity2[indice_parametri];
    					capture_rate_for_equations=capture_rate*switch_trappole_aperte;
    				}

    				//printf("Giorno %d, E=%e, L=%E, P=%E, A=%e\n", (i+t_start_simulation),y[0], y[1], y[2], y[3]);

    				double t;
    				t=i;	
    				simulate_culex(indice_cluster, &t, i, y);

    				//printf("Giorno %d, E=%e, L=%E, P=%E, A=%e\n", (i+t_start_simulation),y[0], y[1], y[2], y[3]);

    				//printf("muE=%e, tauE=%e, muL=%e, tauL=%e, muP=%e, tauP=%e\ndA=%e, muA=%e, diap=%e\n", muE, tauE, muL, tauL, muP, tauP, dA, muA, diap);

    				if(switch_trappole_aperte==1){
    					y[3]=y[3]-capture_rate_for_equations*dA*y[3];
    					simulated_captures[counter_for_capture]=capture_rate_for_equations*dA*y[3];
    					//printf("Cattura giorno %d, dA*capture_rate=%e, A=%e, osservato=%e, simulato=%e\n",giorni_catture[counter_for_capture], dA*capture_rate_for_equations, y[3], observed_captured_mosquitoes[indice_cluster][counter_for_capture], capture_rate_for_equations*dA*y[3]);
    				}
    				if(giorni_catture[counter_for_capture]==i+t_start_simulation)
    					counter_for_capture=counter_for_capture+1;
    			}

    			int indice_per_aggregare, indice_vettore_aggregato;
    			indice_per_aggregare=0;
    			indice_vettore_aggregato=0;
    			for(i=0;i<numero_catture;i++){
    				simulated_aggregated_captures[i]=0.;
    				observed_aggregated_captures[i]=0.;
    			}
    			for(i=0;i<numero_catture;i++){
    				if(active_traps[indice_cluster][i]==1){
    					simulated_aggregated_captures[indice_vettore_aggregato]=simulated_aggregated_captures[indice_vettore_aggregato]+simulated_captures[i];
    					observed_aggregated_captures[indice_vettore_aggregato]=observed_aggregated_captures[indice_vettore_aggregato]+observed_captured_mosquitoes[indice_cluster][i];
    					indice_per_aggregare=indice_per_aggregare+1;
    					if(indice_per_aggregare>threshold_per_aggregare){
    						indice_per_aggregare=0;
    						indice_vettore_aggregato=indice_vettore_aggregato+1;
    					}
    				}
    			}

    			for(i=0;i<indice_vettore_aggregato;i++){
    				lik_tmp=gsl_ran_poisson_pdf(observed_aggregated_captures[i], simulated_aggregated_captures[i])*costante_per_lik;
    					if(observed_aggregated_captures[i]==0. && simulated_aggregated_captures[i]==0.)
    						lik_tmp=1.*costante_per_lik;
    					//printf("Aggregato osservato=%e, aggregato simulato=%e, lik=%Le\n",observed_aggregated_captures[i], simulated_aggregated_captures[i], lik_tmp);
    				l0=l0*lik_tmp;
    			}
    			indice_parametri=indice_parametri+1;
    		}
    	}
    	if(l0>best_lik){
    		best_lik=l0;
    		for(i=0;i<totale_cluster_simulati*totale_anni;i++){
    			best_carrying_capacity1[i]=carrying_capacity1[i];
    			best_carrying_capacity2[i]=carrying_capacity2[i];
    			best_initial_adults[i]=initial_adults[i];
    		}
    		printf("%d, lik=%Le\n", iterazioni_starting_point,best_lik);
    		fflush(stdout);
    	}			
    }

    if(best_lik==0){
    	printf("Non ce l'ho fatta\n");
    	fflush(stdout);
    	fprintf(fwrite_params,"0\n");
    }
    	
    printf("Esco dal loop in %d passi con loglik=%Le\n", iterazioni_starting_point, log10l(best_lik));
    for(i=0;i<totale_cluster_simulati*totale_anni;i++)
    	printf("Cluster %d, K1=%e, K2=%e, A0=%e\n", i+1, best_carrying_capacity1[i], best_carrying_capacity2[i], best_initial_adults[i]);
    	
    long double curr_lik;
    curr_lik=best_lik;
    double *curr_carrying_capacity1, *curr_carrying_capacity2,*curr_initial_adults;
    curr_carrying_capacity1=(double*)calloc(totale_cluster_simulati*totale_anni,sizeof(double));
    curr_carrying_capacity2=(double*)calloc(totale_cluster_simulati*totale_anni,sizeof(double));
    curr_initial_adults=(double*)calloc(totale_cluster_simulati*totale_anni,sizeof(double));

    for(i=0;i<totale_cluster_simulati*totale_anni;i++){
    	curr_carrying_capacity1[i]=best_carrying_capacity1[i];
    	curr_carrying_capacity2[i]=best_carrying_capacity2[i];
    	curr_initial_adults[i]=best_initial_adults[i];
    }
    	
    int iter_mcmc,max_iter_mcmc;
  	max_iter_mcmc=atof(argv[11]);
  	
  	double sigma_small, sigma_big;
  	sigma_small=atof(argv[13]);
  	sigma_big=atof(argv[12]);
  	
  	printf("Sigma small=%e, sigma big=%e\n", sigma_small, sigma_big);
  	
  	printf("\nParte MCMC\n");
  	fflush(stdout);
  	
  	int acceptance, ok_to_simulate, conteggio_per_risimulare;
  	acceptance=0;
  	conteggio_per_risimulare=0;
  	if(best_lik!=0.){
  		for(iter_mcmc=0;iter_mcmc<max_iter_mcmc;iter_mcmc++){
  		//printf("\nIterazione=%d\n", iter_mcmc);
  			ok_to_simulate=1;
  			l0=0.;

  			for(i=0;i<totale_cluster_simulati*totale_anni;i++){
  				carrying_capacity1[i]=curr_carrying_capacity1[i]+gsl_ran_gaussian(RNG, sigma_big);
  				carrying_capacity2[i]=curr_carrying_capacity2[i]+gsl_ran_gaussian(RNG, sigma_big);
  				initial_adults[i]=curr_initial_adults[i]+gsl_ran_gaussian(RNG, sigma_big);
  			}
  			for(i=0;i<totale_cluster_simulati*totale_anni;i++)
  				if(carrying_capacity1[i]<1 || carrying_capacity2[i]<1 || initial_adults[i]<1)
  					ok_to_simulate=0;

    		if(ok_to_simulate!=0){
				
				l0=1.;
    			indice_parametri=0;
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
    						y[i]=0.;
    					for(i=0;i<numero_catture;i++)
    						simulated_captures[i]=0.;

    					y[3]=initial_adults[indice_parametri];
    					counter_for_capture=0;

    					for (i=0; i<tmax; i++){

    						switch_trappole_aperte=0.;
    						if(giorni_catture[counter_for_capture]==i+t_start_simulation)
    							if(active_traps[indice_cluster][counter_for_capture]==1)
    								switch_trappole_aperte=1.;

    						if(i<=t_switch_carrying_capacity){
    							carrying_capacity_for_equations=carrying_capacity1[indice_parametri];
    							capture_rate_for_equations=capture_rate*switch_trappole_aperte;
    						}
    						else{
    							carrying_capacity_for_equations=carrying_capacity2[indice_parametri];
    							capture_rate_for_equations=capture_rate*switch_trappole_aperte;
    						}

    						double t;
    						t=i;	
    						simulate_culex(indice_cluster, &t, i, y);

    						if(switch_trappole_aperte==1){
    							y[3]=y[3]-capture_rate_for_equations*dA*y[3];
    							simulated_captures[counter_for_capture]=capture_rate_for_equations*dA*y[3];
    							//simulated_captures[counter_for_capture]=y[7];
    							//y[7]=0.;
    						}
    						if(giorni_catture[counter_for_capture]==i+t_start_simulation)
    							counter_for_capture=counter_for_capture+1;
    					}

    					int indice_per_aggregare, indice_vettore_aggregato;
    					indice_per_aggregare=0;
    					indice_vettore_aggregato=0;
    					for(i=0;i<numero_catture;i++){
    						simulated_aggregated_captures[i]=0.;
    						observed_aggregated_captures[i]=0.;
    					}
    					for(i=0;i<numero_catture;i++){
    						if(active_traps[indice_cluster][i]==1){
    							simulated_aggregated_captures[indice_vettore_aggregato]=simulated_aggregated_captures[indice_vettore_aggregato]+simulated_captures[i];
    							observed_aggregated_captures[indice_vettore_aggregato]=observed_aggregated_captures[indice_vettore_aggregato]+observed_captured_mosquitoes[indice_cluster][i];
    							indice_per_aggregare=indice_per_aggregare+1;
    							if(indice_per_aggregare>threshold_per_aggregare){
    								indice_per_aggregare=0;
    								indice_vettore_aggregato=indice_vettore_aggregato+1;
    							}
    						}
    					}

    					for(i=0;i<indice_vettore_aggregato;i++){
    						lik_tmp=gsl_ran_poisson_pdf(observed_aggregated_captures[i], simulated_aggregated_captures[i])*costante_per_lik;
    							if(observed_aggregated_captures[i]==0. && simulated_aggregated_captures[i]==0.)
    								lik_tmp=1.*costante_per_lik;
    						l0=l0*lik_tmp;
    					}
    					indice_parametri=indice_parametri+1;
    				}
    			}
    			//printf("Iter %d, Loglik=%Le, ", iter_mcmc, log10l(l0));
    		}

    		long double ratio;
      		ratio=l0/curr_lik;
      		//printf("ratio=%Le\n",ratio);
    		if(ratio>=1 || (ratio<1 && gsl_ran_flat(RNG,0,1)<ratio)){
    			//printf("Accetto\n");
    			for(i=0;i<totale_cluster_simulati*totale_anni;i++){
    				curr_carrying_capacity1[i]=carrying_capacity1[i];
    				curr_carrying_capacity2[i]=carrying_capacity2[i];
    				curr_initial_adults[i]=initial_adults[i];
    			}
    			curr_lik=l0;
    			acceptance=acceptance+1;
    			//conteggio_per_risimulare=0;
    		}
    		for(i=0;i<totale_cluster_simulati*totale_anni;i++)
    			fprintf(fwrite_params,"%e\t",curr_carrying_capacity1[i]);
    		for(i=0;i<totale_cluster_simulati*totale_anni;i++)
    			fprintf(fwrite_params,"%e\t",curr_carrying_capacity2[i]);
    		for(i=0;i<totale_cluster_simulati*totale_anni;i++)
    			fprintf(fwrite_params,"%e\t",curr_initial_adults[i]);
    		fprintf(fwrite_params,"%Le\n",log10l(curr_lik));
  		    fflush(fwrite_params);
  		}
  	}
  	
  	printf("Acceptance rate %e\n",(double)acceptance/(double)max_iter_mcmc);
}
