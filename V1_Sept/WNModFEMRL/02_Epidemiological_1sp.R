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


##installo pacchetto
#library(devtools)
#install_github("https://github.com/elisf02/WNModFEMRL.git", build_vignettes = T)

##set working directory del lavoro (cartella dove ho messo i file da richiamare e dove ho inserito cartelle output)
setwd('/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/WNModFEMRL/')

##richiamo il pacchetto
library(WNModFEMRL)
vignette("WNModFEMRL")

WN_MCMC_and_Simu(seed = 0,  #lascialo sempre così
                 max_iter_MCMC = 10000,  #numero di iterazioni della MonteCarlo
                 numero_simulazioni = 100, #numero di simulazioni
                 n_iter_preliminary_lik = 10000,  #numero di iterazioni della likely
                                                 #(prova ad aumentarlo nel caso)
                 numero_cluster = 1,  #un solo cluster che comprende tutta la Lombardia
                 anno_inizio = 2016,  
                 anno_fine = 2018,
                 con_cosa_inizio = 1,  #lascialo sempre così
                 
                 prior_pR = 0, #per ora lascialo così
                 prior_B0 = 0, #lascialo a 0 all'inizio. poi prova a vedere cosa succede cambiandolo
                               #numero iniziale di uccelli a inizio simulazioni
                 min_niB = 0.09,#1/11 #quanto tempo impiegano per passare da esposti a infetti
                                #calcolato come (1/tempo in giorni) da Komar et al. a 3 dpi sviluppano viremia
                 max_niB = 1,#1/1 #quanto tempo impiegano per passare da esposti a infetti
                                #calcolato come (1/tempo in giorni) da Komar et al. alcuni viremici il giorno stesso dell'inoculo
                 min_recB = 0.086, #0, #quanto tempo impiegano per passare da infetti a recovered
                               #calcolato come (1/tempo in giorni) da Komar et al.
                 max_recB = 0.33, #0.14, #1/7, #quanto tempo impiegano per passare da infetti a recovered
                                 #calcolato come (1/tempo in giorni) da Komar et al. 
                                 ####fai check!!
                 min_muB = 0,#0.002, #1/365, #quante uova producono all'anno (numero di uova/365)
                 max_muB = 0.022, #0.04, #16/365,
                 min_s = 5,  #sincronizzazione delle nascite (calcolato dal grafico della birth pulse)
                 max_s = 20, #sincronizzazione delle nascite (calcolato dal grafico della birth pulse)
                 min_phi = 0.33, #tempistica delle nascite (tra 0 e 1), calcolato da grafico birth pulse 
                 max_phi = 0.66, #tempistica delle nascite (tra 0 e 1), calcolato da grafico birth pulse 
                 
                 file_temperatura_name  = "per_mcmc/temperatura_media_", 
                 file_numero_pool_name = "per_mcmc/totale_pool_cluster_",
                 file_numero_pool_positivi_name = "per_mcmc/totale_pool_WNVpos_cluster_", 
                 file_pool_size_name = "per_mcmc/mean_pool_size_cluster_",
                 file_zanzare_name = "per_mcmc/zanzare/adulti_medi_", 
                 vettore_date_catture = "per_mcmc/giorni_cattura.txt",
                 
                 sigma_big = 4, 
                 sigma_small = 0.012,
                 sigma_medio = 1,
                 
                 LocOutMCMC = "Output_WNV/MCMC", 
                 LocOutSimu = "Output_WNV/Simulazioni",
                 NameOutParms = "parametri_", 
                 NameOutParmsPerSimu = "per_simulazione_",
                 NameOutDyn = "dynamics_")



WNV_ParPlotFunc(anno_inizio = 2016,
                anno_fine = 2018,
                numero_cluster = 1,
                con_cosa_inizio = 1,
                
                OutLoc = "Output_WNV/MCMC",
                FileAllParmsName = "parametri_", 
                FileParmsPerSimuName = "per_simulazione_")


WN_PlotModelFit(anno_inizio = 2016,
                anno_fine = 2018,
                numero_cluster = 1,
                con_cosa_inizio = 1, 
                max_iter_MCMC = 10000,
                tmax = 180,
                
                FolderSimu = "Output_WNV/Simulazioni/", 
                FileDynName = "dynamics_",
                FolderPlotOut = "Output_WNV/Simulazioni/Plots/", 
                PlotName = "ModelFit_",
                file_numero_pool_name = "per_mcmc/totale_pool_cluster_", 
                file_numero_pool_positivi_name = "per_mcmc/totale_pool_WNVpos_cluster_", 
                file_pool_size_name = "per_mcmc/mean_pool_size_cluster_", 
                vettore_date_catture = "per_mcmc/giorni_cattura.txt", 
                
                cex_main = 1.5,
                cex_axes = 1.2, 
                cex_lab_axes = 1.2,
                margins_plot = c(7, 7, 4, 2),
                ymax_plot = 17)


WN_PlotDynamics(anno_inizio = 2016,
                anno_fine = 2018,
                numero_cluster = 1,
                con_cosa_inizio = 1, 
                
                FolderSimu = "Output_WNV/Simulazioni/",
                FileDynName = "dynamics_", 
                FolderPlotOut = "Output_WNV/Simulazioni/Plots/",
                PlotName = 'Dynamics_BirdPop_', #cambia a seconda del plot che stai facendo 
                vettore_date_catture = "per_mcmc/giorni_cattura.txt", 
                
                cex_main = 2,
                cex_axes = 1.2,
                cex_lab_axes = 1.2,
                margins_plot = c(7, 7, 4, 2), 
                colore = "darkblue", 
                what_plot = "bird_population")
# "mosquito_prevalence" oppure "moquito_population" o  "bird_prevalence" o "bird_population") 

CheckMCMC(InLoc = "Output_WNV/MCMC/",
          FileName = "parametri_", 
          anno = 2018,
          quale_cluster = 1,
          OnFile = T,
          OutLoc = "Output_WNV/MCMC/Plots/", 
          OutFileName = "Check_Lik_",
          con_cosa_inizio = 1)

dev.off()
check_birth_pulse(parms = "Output_WNV/MCMC/parametri_M_2018_1.txt", #c('muB' = 0.07,'s' = 4,'phi' = 0.6), c('muB' = 0.05,'s' = 10,'phi' = 0.45)
                  tmin = 90,
                  tmax = 244,
                  quante_specie = 1)
