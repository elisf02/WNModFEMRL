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
setwd('/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/WNModFEMRL')

##richiamo il pacchetto
library(WNModFEMRL)
vignette("WNModFEMRL")

ENTO_ParmsEstimFunc(numero_cluster = 1,
                    anno_inizio = 2016,
                    anno_fine = 2018,
                    
                    solo_un_cluster = 1,
                    max_iter_MCMC = 10000,
                    sigma_big = 5,
                    sigma_small = 0.01,
                    seed = 0,
                    
                    file_temperatura="per_mcmc/temperatura_media_",
                    file_catture="per_mcmc/catture_medie_cluster_",
                    file_trappole="per_mcmc/trappole_attive_cluster_",
                    vettore_date_catture="per_mcmc/giorni_cattura.txt",
                    file_luce="per_mcmc/light_minutes.txt",
                    
                    OutLoc="Output/MCMC",
                    FileAllParmsName="parametri_")


ENTO_SaveParmsPerSim(numero_cluster = 1,
                     anno_inizio = 2016,
                     anno_fine = 2018,
                     
                     vettore_date_catture = "per_mcmc/giorni_cattura.txt", 
                     solo_un_cluster = 1,
                     
                     OutLoc = "Output/MCMC",
                     FileAllParmsName = "parametri_", 
                     FileParmsPerSimuName = "per_simulazione_")


ENTO_PlotFunc(anno_inizio = 2016,
              anno_fine = 2018,
              numero_cluster = 1,
              
              OutLoc = "Output/MCMC", 
              FileAllParmsName = "parametri_",
              FileParmsPerSimuName = "per_simulazione_")


ENTO_PlotCheckK(anno_inizio = 2016,
                anno_fine = 2018,
                numero_cluster = 1,
                
                OutLoc = "Output/MCMC", 
                FilePlotName = "confronto_carrying_capacity",
                FileAllParmsName = "parametri_")


ENTO_SimuFunc(anno_inizio = 2016,
              anno_fine = 2018,
              numero_cluster = 1,
              numero_simulazioni = 200, 
              
              OutLoc = "Output/Simulazioni",
              PlotLoc = "Output/Simulazioni/Plots", 
              
              FileParmsLoc = "Output/MCMC",
              Filename = "catture_simulate_", 
              FileParmsPerSimuName = "per_simulazione_",
              FileDynName = "dynamics_", 
              FileCaptName = "catture_",
              
              solo_un_cluster = 1,
              seed = 0, 
              
              file_temperatura = "per_mcmc/temperatura_media_",
              file_catture = "per_mcmc/catture_medie_cluster_", 
              file_trappole = "per_mcmc/trappole_attive_cluster_",
              vettore_date_catture = "per_mcmc/giorni_cattura.txt", 
              file_luce = "per_mcmc/light_minutes.txt",
              
              cex_points = 1.2, 
              cex_axes = 1.2,
              cex_lab_axes = 1.2,
              margins_plot = c(5, 5, 2, 2))


ENTO_DynFunc(anno_inizio = 2016,
             anno_fine = 2018,
             numero_cluster = 1,
             
             OutLoc = "Output/Simulazioni", 
             OutZanzMedieLoc = "per_mcmc/zanzare",
             
             FileDynName = "dynamics_", 
             FileZanzMedieName = "adulti_medi_",
             PlotDynName = "dinamica_adulti_", 
             
             cex_points = 1.2,
             cex_axes = 1.2,
             cex_lab_axes = 1.2,
             margins_plot = c(5, 5, 2, 2))
