#ATTENZIONE: L'ORDINE DELLE CARTELLE SU DRIVE NON CORRISPONDE ALL'ORDINE DELLE CARTELLE
#NECESSARIO PER FAR CORRERE I CODICI.
#perché i codici funzionino la cartella su desktop dovrà contenere le seguenti cartelle:
#1. Output -> MCMC -> Plots (contiene glu output dell'MCMC e i plot del modello entomologico)
#          -> Simulazioni -> Plots (contiene gli output delle simulazioni e i plot del modello entomologico)
#2. Output_WNV -> MCMC -> Plots (contiene gli output dell'MCMC e i plot del modello epidemiologico a una specie)
#              -> Simulazioni -> Plots (contiene gli output delle simulazioni e i plot del modello epidemiologico a una specie)
#              -> Merli -> MCMC -> Plots (contiene gli output dell'MCMC e i plot del modello epidemiologico a due specie relativo ai merli)
#                       -> Simulazioni -> Plots (contiene gli output delle simulazioni e i plot del modello epidemiologico a due specie relativo ai merli)
#              -> Gazze -> MCMC -> Plots (contiene gli output dell'MCMC e i plot del modello epidemiologico a due specie relativo alle gazze)
#                       -> Simulazioni -> Plots (contiene gli output delle simulazioni del modello epidemiologico a due specie relativo alle gazze)
#              -> Germani -> MCMC -> Plots (contiene gli output dell'MCMC e i plot del modello epidemiologico a due specie relativo ai germani)
#                         -> Simulazioni -> Plots (contiene gli output delle simulazioni e i plot del modello epidemiologico a due specie relativo ai germani)
#3. per_mcmc -> zanzare
#ATTENZIONE: la cartella per_mcmc DEVE contenere tutti i file delle zanzare, (giorni cattura, light minutes, mean pool ecc)


#library(devtools)
#install_github("https://github.com/elisf02/WNModFEMRL.git", build_vignettes = T)

##set working directory del lavoro (cartella dove ho messo i file da richiamare e dove ho inserito cartelle output)
setwd('/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/WNModFEMRL/')

##richiamo il pacchetto
library(WNModFEMRL)
vignette("WNModFEMRL")

####EPIDEM a due specie functions

#####Merli----
WN_MCMC_and_Simu_2spec(seed = 0, 
                       max_iter_MCMC = 10000, 
                       numero_simulazioni = 100, 
                       n_iter_preliminary_lik = 1000, 
                       numero_cluster = 1,
                       anno_inizio = 2016, 
                       anno_fine = 2018, 
                       con_cosa_inizio = 1, 
                       
                       prior_pR = 0, #stimati da MCMC
                       prior_B0 = 0, #stimati da MCMC
                       prior_pR1 = 0, #stimati da MCMC
                       prior_B01 = 0, #stimati da MCMC
                       min_muB = 0, #stessi prior usati in 02_Epidemiological_1sp
                       max_muB = 0.022, #stessi prior usati in 02_Epidemiological_1sp
                       min_s = 5, #stessi prior usati in 02_Epidemiological_1sp
                       max_s = 20, #stessi prior usati in 02_Epidemiological_1sp
                       min_phi = 0.33, #stessi prior usati in 02_Epidemiological_1sp
                       max_phi = 0.66, #stessi prior usati in 02_Epidemiological_1sp
                       min_niB = 0.09, #stessi prior usati in 02_Epidemiological_1sp
                       max_niB = 1, #stessi prior usati in 02_Epidemiological_1sp
                       min_recB = 0.086, #stessi prior usati in 02_Epidemiological_1sp
                       max_recB = 0.33, #stessi prior usati in 02_Epidemiological_1sp
                       
                       #i prossimi sono i parametri della specie nota, cambiare a seconda della specie
                      
                       b11 = 0.35, #dad Rizzoli et al. 2015
                       muB1 = 0.013, #(numero medio di uova*0.5)*numero medio di cove/365
                       s1 = 6, #stimato da birth pulse function e dati di letteratura
                       phi1 = 0.35, #stimato da birth pulse function e dati di letteratura
                       niB1 = 1, #da Komar et al. 2003
                       recB1 = 0.22, #da Komar et al. 2003
                       
                       file_temperatura_name = "per_mcmc/temperatura_media_", 
                       file_numero_pool_name = "per_mcmc/totale_pool_cluster_", 
                       file_numero_pool_positivi_name = "per_mcmc/totale_pool_WNVpos_cluster_", 
                       file_pool_size_name = "per_mcmc/mean_pool_size_cluster_", 
                       file_zanzare_name = "per_mcmc/zanzare/adulti_medi_", 
                       vettore_date_catture = "per_mcmc/giorni_cattura.txt", 
                       
                       sigma_big = 7, 
                       sigma_small = 0.001, 
                       sigma_medio = 2,
                       
                       LocOutMCMC = "Output_WNV/Merli/MCMC", 
                       LocOutSimu = "Output_WNV/Merli/Simulazioni", 
                       NameOutParms = "parametri_2spec_Merli_", #CAMBIA NOME A SECONDA DELLA SPECIE
                       NameOutParmsPerSimu = "per_simulazione_2spec_Merli_",  #CAMBIA NOME A SECONDA DELLA SPECIE
                       NameOutDyn = "dynamics_2spec_Merli_") #CAMBIA NOME A SECONDA DELLA SPECIE

WNV_ParPlotFunc_2spec(anno_inizio = 2016,
                      anno_fine = 2018,
                      numero_cluster = 1,
                      con_cosa_inizio = 1, 
                      
                      OutLoc = "Output_WNV/Merli/MCMC",
                      FileAllParmsName = "parametri_2spec_Merli_",  #CAMBIA NOME A SECONDA DELLA SPECIE
                      FileParmsPerSimuName = "per_simulazione_2spec_Merli_") #CAMBIA NOME A SECONDA DELLA SPECIE


WN_PlotModelFit_2spec(anno_inizio = 2016,
                      anno_fine = 2018,
                      numero_cluster = 1, 
                      con_cosa_inizio = 1,
                      max_iter_MCMC = 10000,
                      tmax = 180,
                      
                      FolderSimu = "Output_WNV/Merli/Simulazioni/", 
                      FileDynName = "dynamics_2spec_Merli_", #CAMBIA NOME A SECONDA DELLA SPECIE
                      FolderPlotOut = "Output_WNV/Merli/Simulazioni/Plots/", 
                      PlotName = "ModelFit_2spec_Merli_", #CAMBIA NOME A SECONDA DELLA SPECIE
                      file_numero_pool_name = "per_mcmc/totale_pool_cluster_", 
                      file_numero_pool_positivi_name = "per_mcmc/totale_pool_WNVpos_cluster_", 
                      file_pool_size_name = "per_mcmc/mean_pool_size_cluster_",
                      vettore_date_catture = "per_mcmc/giorni_cattura.txt", 
                      
                      cex_main = 2,
                      cex_axes = 1.2,
                      cex_lab_axes = 1.2,
                      margins_plot = c(7, 7, 4, 2),
                      ymax_plot = 17)

WN_PlotDynamics_2spec(anno_inizio = 2016,
                      anno_fine = 2018,
                      numero_cluster = 1,
                      con_cosa_inizio = 1, 
                      
                      FolderSimu = "Output_WNV/Merli/Simulazioni/",
                      FileDynName = "dynamics_2spec_Merli_", 
                      FolderPlotOut = "Output_WNV/Merli/Simulazioni/Plots/",
                      PlotName = "Dynamics_MosqPop_2spec_Merli_", 
                      vettore_date_catture = "per_mcmc/giorni_cattura.txt",
                      
                      cex_main = 2, 
                      cex_axes = 1.2,
                      cex_lab_axes = 1.2,
                      margins_plot = c(7, 7, 4, 2),
                      what_plot = "mosquito_population",
                      to_plot_on_file = T)

#mosquito_population #mosquito_prevalence #infectious_mosquitoes
#all_bird_population #all_bird_prevalence #all_bird_seroprevalence
#avian_community #prevalence_avian_community #seroprevalence_avian_community
#known_bird_population #prev_known_bird_population #seroprev_known_bird_population

CheckMCMC_2spec(InLoc = "Output_WNV/Merli/MCMC/",
                FileName = "parametri_2spec_Merli_", 
                anno = 2018,
                quale_cluster = 1,
                OnFile = T,
                OutLoc = "Output_WNV/Merli/MCMC/Plots/", 
                OutFileName = "Check_Lik_2spec_Merli",
                con_cosa_inizio = 1)

dev.off()
check_birth_pulse(parms = "Output_WNV/Merli/MCMC/parametri_2spec_Merli_M_2018_1.txt", #c('muB' = 0.07,'s' = 4,'phi' = 0.6),
                  tmin = 90,
                  tmax = 244,
                  quante_specie = 2)

#####Gazze ----
WN_MCMC_and_Simu_2spec(seed = 0, 
                       max_iter_MCMC = 10000, 
                       numero_simulazioni = 100, 
                       n_iter_preliminary_lik = 1000, 
                       numero_cluster = 1,
                       anno_inizio = 2016, 
                       anno_fine = 2018, 
                       con_cosa_inizio = 1, 
                       
                       prior_pR = 0, #stimati da MCMC
                       prior_B0 = 0, #stimati da MCMC
                       prior_pR1 = 0, #stimati da MCMC
                       prior_B01 = 0, #stimati da MCMC
                       min_muB = 0, #stessi prior usati in 02_Epidemiological_1sp
                       max_muB = 0.022, #stessi prior usati in 02_Epidemiological_1sp
                       min_s = 5, #stessi prior usati in 02_Epidemiological_1sp
                       max_s = 20, #stessi prior usati in 02_Epidemiological_1sp
                       min_phi = 0.33, #stessi prior usati in 02_Epidemiological_1sp
                       max_phi = 0.66, #stessi prior usati in 02_Epidemiological_1sp
                       min_niB = 0.09, #stessi prior usati in 02_Epidemiological_1sp
                       max_niB = 1, #stessi prior usati in 02_Epidemiological_1sp
                       min_recB = 0.086, #stessi prior usati in 02_Epidemiological_1sp
                       max_recB = 0.33, #stessi prior usati in 02_Epidemiological_1sp
                       
                       #i prossimi sono i parametri della specie nota, cambiare a seconda della specie
                       
                       b11 = 0.1, #da Rizzoli et al. 2015
                       muB1 = 0.0075, #(numero medio di uova*0.5)*numero medio di cove/365
                       s1 = 6, #stimato da birth pulse function + dati letteratura
                       phi1 = 0.4, #stimato da birth pulse function + dati letteratura
                       niB1 = 0.66, #da Komar et al. 2003
                       recB1 = 0.2, #da Komar et al. 2003
                       
                       file_temperatura_name = "per_mcmc/temperatura_media_", 
                       file_numero_pool_name = "per_mcmc/totale_pool_cluster_", 
                       file_numero_pool_positivi_name = "per_mcmc/totale_pool_WNVpos_cluster_", 
                       file_pool_size_name = "per_mcmc/mean_pool_size_cluster_", 
                       file_zanzare_name = "per_mcmc/zanzare/adulti_medi_", 
                       vettore_date_catture = "per_mcmc/giorni_cattura.txt", 
                       
                       sigma_big = 8, 
                       sigma_small = 0.0001, 
                       sigma_medio = 1, 
                       
                       LocOutMCMC = "Output_WNV/Gazze/MCMC", 
                       LocOutSimu = "Output_WNV/Gazze/Simulazioni", 
                       NameOutParms = "parametri_2spec_Gazze_",  #CAMBIA NOME A SECONDA DELLA SPECIE
                       NameOutParmsPerSimu = "per_simulazione_2spec_Gazze_",  #CAMBIA NOME A SECONDA DELLA SPECIE
                       NameOutDyn = "dynamics_2spec_Gazze_") #CAMBIA NOME A SECONDA DELLA SPECIE

WNV_ParPlotFunc_2spec(anno_inizio = 2016,
                      anno_fine = 2018,
                      numero_cluster = 1,
                      con_cosa_inizio = 1, 
                      
                      OutLoc = "Output_WNV/Gazze/MCMC",
                      FileAllParmsName = "parametri_2spec_Gazze_",  #CAMBIA NOME A SECONDA DELLA SPECIE
                      FileParmsPerSimuName = "per_simulazione_2spec_Gazze_") #CAMBIA NOME A SECONDA DELLA SPECIE


WN_PlotModelFit_2spec(anno_inizio = 2016,
                      anno_fine = 2018,
                      numero_cluster = 1, 
                      con_cosa_inizio = 1,
                      max_iter_MCMC = 10000,
                      tmax = 180,
                      
                      FolderSimu = "Output_WNV/Gazze/Simulazioni/", 
                      FileDynName = "dynamics_2spec_Gazze_", #CAMBIA NOME A SECONDA DELLA SPECIE
                      FolderPlotOut = "Output_WNV/Gazze/Simulazioni/Plots/", 
                      PlotName = "ModelFit_2spec_Gazze_", #CAMBIA NOME A SECONDA DELLA SPECIE
                      file_numero_pool_name = "per_mcmc/totale_pool_cluster_", 
                      file_numero_pool_positivi_name = "per_mcmc/totale_pool_WNVpos_cluster_", 
                      file_pool_size_name = "per_mcmc/mean_pool_size_cluster_",
                      vettore_date_catture = "per_mcmc/giorni_cattura.txt", 
                      
                      cex_main = 2,
                      cex_axes = 1.2,
                      cex_lab_axes = 1.2,
                      margins_plot = c(7, 7, 4, 2),
                      ymax_plot = 17)

WN_PlotDynamics_2spec(anno_inizio = 2016,
                      anno_fine = 2018,
                      numero_cluster = 1,
                      con_cosa_inizio = 1, 
                      
                      FolderSimu = "Output_WNV/Gazze/Simulazioni/",
                      FileDynName = "dynamics_2spec_Gazze_",  #CAMBIA NOME A SECONDA DELLA SPECIE
                      FolderPlotOut = "Output_WNV/Gazze/Simulazioni/Plots/",
                      PlotName = "Dynamics_MosqInf_2spec_Gazze_",  #CAMBIA NOME A SECONDA DELLA SPECIE
                      vettore_date_catture = "per_mcmc/giorni_cattura.txt",
                      
                      cex_main = 2, 
                      cex_axes = 1.2,
                      cex_lab_axes = 1.2,
                      margins_plot = c(7, 7, 4, 2),
                      what_plot = "infectious_mosquitoes",
                      to_plot_on_file = T)

#mosquito_population #mosquito_prevalence #infectious_mosquitoes
#all_bird_population #all_bird_prevalence #all_bird_seroprevalence
#avian_community #prevalence_avian_community #seroprevalence_avian_community
#known_bird_population #prev_known_bird_population #seroprev_known_bird_population

CheckMCMC_2spec(InLoc = "Output_WNV/Gazze/MCMC/",
                FileName = "parametri_2spec_Gazze_",  #CAMBIA NOME A SECONDA DELLA SPECIE
                anno = 2018,
                quale_cluster =1,
                OnFile = T,
                OutLoc = "Output_WNV/Gazze/MCMC/Plots/", 
                OutFileName = "Check_Lik_2spec_Gazze",
                con_cosa_inizio = 1)

dev.off() #CAMBIA NOME A SECONDA DELLA SPECIE
check_birth_pulse(parms = "Output_WNV/Gazze/MCMC/parametri_2spec_Gazze_M_2018_1.txt", #c('muB' = 0.07,'s' = 4,'phi' = 0.6),
                  tmin = 90,
                  tmax = 244,
                  quante_specie = 2)

#####Germani ----
WN_MCMC_and_Simu_2spec(seed = 0, 
                       max_iter_MCMC = 10000, 
                       numero_simulazioni = 100, 
                       n_iter_preliminary_lik = 1000, 
                       numero_cluster = 1,
                       anno_inizio = 2016, 
                       anno_fine = 2018, 
                       con_cosa_inizio = 1, 
                       
                       prior_pR = 0, #stimati da MCMC
                       prior_B0 = 0, #stimati da MCMC
                       prior_pR1 = 0, #stimati da MCMC
                       prior_B01 = 0, #stimati da MCMC
                       min_muB = 0, #stessi prior usati in 02_Epidemiological_1sp
                       max_muB = 0.022, #stessi prior usati in 02_Epidemiological_1sp
                       min_s = 5, #stessi prior usati in 02_Epidemiological_1sp
                       max_s = 20, #stessi prior usati in 02_Epidemiological_1sp
                       min_phi = 0.33, #stessi prior usati in 02_Epidemiological_1sp
                       max_phi = 0.66, #stessi prior usati in 02_Epidemiological_1sp
                       min_niB = 0.09, #stessi prior usati in 02_Epidemiological_1sp
                       max_niB = 1, #stessi prior usati in 02_Epidemiological_1sp
                       min_recB = 0.086, #stessi prior usati in 02_Epidemiological_1sp
                       max_recB = 0.33, #stessi prior usati in 02_Epidemiological_1sp
                       
                       #i prossimi sono i parametri della specie nota, cambiare a seconda della specie
                       
                       b11 = 0.01, # da Hamer et al. 2009
                       muB1 = 0.014, #(numero medio di uova*0.5)*numero medio di cove/365
                       s1 = 10, #stimato da birth pulse function + dati letteratura
                       phi1 = 0.33, #stimato da birth pulse function + dati letteratura
                       niB1 = 0.66, #da Komar et al. 2003
                       recB1 = 0.25, #da Komar et al. 2003
                       
                       file_temperatura_name = "per_mcmc/temperatura_media_", 
                       file_numero_pool_name = "per_mcmc/totale_pool_cluster_", 
                       file_numero_pool_positivi_name = "per_mcmc/totale_pool_WNVpos_cluster_", 
                       file_pool_size_name = "per_mcmc/mean_pool_size_cluster_", 
                       file_zanzare_name = "per_mcmc/zanzare/adulti_medi_", 
                       vettore_date_catture = "per_mcmc/giorni_cattura.txt", 
                       
                       sigma_big = 6, 
                       sigma_small = 0.001, 
                       sigma_medio = 1, 
                       
                       LocOutMCMC = "Output_WNV/Germani/MCMC", 
                       LocOutSimu = "Output_WNV/Germani/Simulazioni", 
                       NameOutParms = "parametri_2spec_Germani_",  #CAMBIA NOME A SECONDA DELLA SPECIE
                       NameOutParmsPerSimu = "per_simulazione_2spec_Germani_",  #CAMBIA NOME A SECONDA DELLA SPECIE
                       NameOutDyn = "dynamics_2spec_Germani_") #CAMBIA NOME A SECONDA DELLA SPECIE

WNV_ParPlotFunc_2spec(anno_inizio = 2016,
                      anno_fine = 2018,
                      numero_cluster = 1,
                      con_cosa_inizio = 1, 
                      
                      OutLoc = "Output_WNV/Germani/MCMC",
                      FileAllParmsName = "parametri_2spec_Germani_",  #CAMBIA NOME A SECONDA DELLA SPECIE
                      FileParmsPerSimuName = "per_simulazione_2spec_Germani_") #CAMBIA NOME A SECONDA DELLA SPECIE


WN_PlotModelFit_2spec(anno_inizio = 2016,
                      anno_fine = 2018,
                      numero_cluster = 1, 
                      con_cosa_inizio = 1,
                      max_iter_MCMC = 10000,
                      tmax = 180,
                      
                      FolderSimu = "Output_WNV/Germani/Simulazioni/", 
                      FileDynName = "dynamics_2spec_Germani_", #CAMBIA NOME A SECONDA DELLA SPECIE
                      FolderPlotOut = "Output_WNV/Germani/Simulazioni/Plots/", 
                      PlotName = "ModelFit_2spec_Germani", #CAMBIA NOME A SECONDA DELLA SPECIE
                      file_numero_pool_name = "per_mcmc/totale_pool_cluster_", 
                      file_numero_pool_positivi_name = "per_mcmc/totale_pool_WNVpos_cluster_", 
                      file_pool_size_name = "per_mcmc/mean_pool_size_cluster_",
                      vettore_date_catture = "per_mcmc/giorni_cattura.txt", 
                      
                      cex_main = 2,
                      cex_axes = 1.2,
                      cex_lab_axes = 1.2,
                      margins_plot = c(7, 7, 4, 2),
                      ymax_plot = 17)

WN_PlotDynamics_2spec(anno_inizio = 2016,
                      anno_fine = 2018,
                      numero_cluster = 1,
                      con_cosa_inizio = 1, 
                      
                      FolderSimu = "Output_WNV/Germani/Simulazioni/",
                      FileDynName = "dynamics_2spec_Germani_",  #CAMBIA NOME A SECONDA DELLA SPECIE
                      FolderPlotOut = "Output_WNV/Germani/Simulazioni/Plots/",
                      PlotName = "Dynamics_MosqPop_2spec_Germani_",  #CAMBIA NOME A SECONDA DELLA SPECIE
                      vettore_date_catture = "per_mcmc/giorni_cattura.txt",
                      
                      cex_main = 2, 
                      cex_axes = 1.2,
                      cex_lab_axes = 1.2,
                      margins_plot = c(7, 7, 4, 2),
                      what_plot = "mosquito_population",
                      to_plot_on_file = T)

#mosquito_population #mosquito_prevalence #infectious_mosquitoes
#all_bird_population #all_bird_prevalence #all_bird_seroprevalence
#avian_community #prevalence_avian_community #seroprevalence_avian_community
#known_bird_population #prev_known_bird_population #seroprev_known_bird_population

CheckMCMC_2spec(InLoc = "Output_WNV/Germani/MCMC/",
                FileName = "parametri_2spec_Germani_",  #CAMBIA NOME A SECONDA DELLA SPECIE
                anno = 2018,
                quale_cluster = 1,
                OnFile = T,
                OutLoc = "Output_WNV/Germani/MCMC/Plots/", 
                OutFileName = "Check_Lik_2spec_Germani_",
                con_cosa_inizio = 1)

dev.off() #CAMBIA NOME A SECONDA DELLA SPECIE
check_birth_pulse(parms = "Output_WNV/Germani/MCMC/parametri_2spec_Germani_M_2018_1.txt", #c('muB' = 0.07,'s' = 4,'phi' = 0.6),
                  tmin = 90,
                  tmax = 244,
                  quante_specie = 2)


#####Cornacchie----
WN_MCMC_and_Simu_2spec(seed = 0, 
                       max_iter_MCMC = 10000, 
                       numero_simulazioni = 100, 
                       n_iter_preliminary_lik = 1000, 
                       numero_cluster = 1,
                       anno_inizio = 2016, 
                       anno_fine = 2018, 
                       con_cosa_inizio = 1, 
                       
                       prior_pR = 0, #stimati da MCMC
                       prior_B0 = 0, #stimati da MCMC
                       prior_pR1 = 0, #stimati da MCMC
                       prior_B01 = 0, #stimati da MCMC
                       min_muB = 0, #stessi prior usati in 02_Epidemiological_1sp
                       max_muB = 0.022, #stessi prior usati in 02_Epidemiological_1sp
                       min_s = 5, #stessi prior usati in 02_Epidemiological_1sp
                       max_s = 20, #stessi prior usati in 02_Epidemiological_1sp
                       min_phi = 0.33, #stessi prior usati in 02_Epidemiological_1sp
                       max_phi = 0.66, #stessi prior usati in 02_Epidemiological_1sp
                       min_niB = 0.09, #stessi prior usati in 02_Epidemiological_1sp
                       max_niB = 1, #stessi prior usati in 02_Epidemiological_1sp
                       min_recB = 0.086, #stessi prior usati in 02_Epidemiological_1sp
                       max_recB = 0.33, #stessi prior usati in 02_Epidemiological_1sp
                       
                       #i prossimi sono i parametri della specie nota, cambiare a seconda della specie
                       
                       b11 = 0.009, #da Savage et al. 2007
                       muB1 = 0.006, #(numero medio di uova*0.5)*numero medio di cove/365
                       s1 = 10, #stimato da birth pulse function e dati letteratura
                       phi1 = 0.33, #stimato da birth pulse function e dati letteratura
                       niB1 = 0.5, #da Komar et al. 2003
                       recB1 = 0.26, #da Komar et al. 2003
                       
                       file_temperatura_name = "per_mcmc/temperatura_media_", 
                       file_numero_pool_name = "per_mcmc/totale_pool_cluster_", 
                       file_numero_pool_positivi_name = "per_mcmc/totale_pool_WNVpos_cluster_", 
                       file_pool_size_name = "per_mcmc/mean_pool_size_cluster_", 
                       file_zanzare_name = "per_mcmc/zanzare/adulti_medi_", 
                       vettore_date_catture = "per_mcmc/giorni_cattura.txt", 
                       
                       sigma_big = 6, 
                       sigma_small = 0.001, 
                       sigma_medio = 1,
                       
                       LocOutMCMC = "Output_WNV/Cornacchie/MCMC", 
                       LocOutSimu = "Output_WNV/Cornacchie/Simulazioni", 
                       NameOutParms = "parametri_2spec_Cornacchie_", #CAMBIA NOME A SECONDA DELLA SPECIE
                       NameOutParmsPerSimu = "per_simulazione_2spec_Cornacchie_",  #CAMBIA NOME A SECONDA DELLA SPECIE
                       NameOutDyn = "dynamics_2spec_Cornacchie_") #CAMBIA NOME A SECONDA DELLA SPECIE

WNV_ParPlotFunc_2spec(anno_inizio = 2016,
                      anno_fine = 2018,
                      numero_cluster = 1,
                      con_cosa_inizio = 1, 
                      
                      OutLoc = "Output_WNV/Cornacchie/MCMC",
                      FileAllParmsName = "parametri_2spec_Cornacchie_",  #CAMBIA NOME A SECONDA DELLA SPECIE
                      FileParmsPerSimuName = "per_simulazione_2spec_Cornacchie_") #CAMBIA NOME A SECONDA DELLA SPECIE


WN_PlotModelFit_2spec(anno_inizio = 2016,
                      anno_fine = 2018,
                      numero_cluster = 1, 
                      con_cosa_inizio = 1,
                      max_iter_MCMC = 10000,
                      tmax = 180,
                      
                      FolderSimu = "Output_WNV/Cornacchie/Simulazioni/", 
                      FileDynName = "dynamics_2spec_Cornacchie_", #CAMBIA NOME A SECONDA DELLA SPECIE
                      FolderPlotOut = "Output_WNV/Cornacchie/Simulazioni/Plots/", 
                      PlotName = "ModelFit_2spec_Cornacchie_", #CAMBIA NOME A SECONDA DELLA SPECIE
                      file_numero_pool_name = "per_mcmc/totale_pool_cluster_", 
                      file_numero_pool_positivi_name = "per_mcmc/totale_pool_WNVpos_cluster_", 
                      file_pool_size_name = "per_mcmc/mean_pool_size_cluster_",
                      vettore_date_catture = "per_mcmc/giorni_cattura.txt", 
                      
                      cex_main = 2,
                      cex_axes = 1.2,
                      cex_lab_axes = 1.2,
                      margins_plot = c(7, 7, 4, 2),
                      ymax_plot = 17)

WN_PlotDynamics_2spec(anno_inizio = 2016,
                      anno_fine = 2018,
                      numero_cluster = 1,
                      con_cosa_inizio = 1, 
                      
                      FolderSimu = "Output_WNV/Cornacchie/Simulazioni/",
                      FileDynName = "dynamics_2spec_Cornacchie_", 
                      FolderPlotOut = "Output_WNV/Cornacchie/Simulazioni/Plots/",
                      PlotName = "Dynamics_KnowBirdSero_2spec_Cornacchie_", 
                      vettore_date_catture = "per_mcmc/giorni_cattura.txt",
                      
                      cex_main = 2, 
                      cex_axes = 1.2,
                      cex_lab_axes = 1.2,
                      margins_plot = c(7, 7, 4, 2),
                      what_plot = "seroprev_known_bird_population",
                      to_plot_on_file = T)

#mosquito_population #mosquito_prevalence #infectious_mosquitoes
#all_bird_population #all_bird_prevalence #all_bird_seroprevalence
#avian_community #prevalence_avian_community #seroprevalence_avian_community
#known_bird_population #prev_known_bird_population #seroprev_known_bird_population

CheckMCMC_2spec(InLoc = "Output_WNV/Cornacchie/MCMC/",
                FileName = "parametri_2spec_Cornacchie_", 
                anno = 2018,
                quale_cluster = 1,
                OnFile = T,
                OutLoc = "Output_WNV/Cornacchie/MCMC/Plots/", 
                OutFileName = "Check_Lik_2spec_Cornacchie",
                con_cosa_inizio = 1)

dev.off()
check_birth_pulse(parms = "Output_WNV/Cornacchie/MCMC/parametri_2spec_Cornacchie_M_2018_1.txt", #c('muB' = 0.07,'s' = 4,'phi' = 0.6),
                  tmin = 90,
                  tmax = 244,
                  quante_specie = 2)

