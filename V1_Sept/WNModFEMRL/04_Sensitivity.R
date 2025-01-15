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

##richiamo il pacchetto
library(WNModFEMRL)
vignette("WNModFEMRL")

setwd('/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/WNModFEMRL/')

UnAnno = T #se F crea il file utilizzando le medie dei parametri sui tre anni (parametri_MediaAllYears.txt)
           #se T crea i file con i parametri medi divisi per ogni anno (check se cambia qualcosa effettivamente nei grafici)

# creiamo file medie parms per sensitivity ----
# con il comando source() faccio correre lo script che mi crea i file con le medie dei parametri
for (anno in 2016:2018) {
  source('mean_parms.R')
}

#check
#name_file_parametri_medi = paste0('Output_WNV/MCMC/mean_parms_',anno,'.txt')
#scan(name_file_parametri_medi)

# dinamica parametri medi ----
Simu4Sens(seed = 0,
          numero_cluster = 1,
          anno_inizio = 2016,
          anno_fine = 2018, 
          con_cosa_inizio = 1,
          to_mean_all_years = T,  # F non funziona 
          file_temperatura_name = "per_mcmc/temperatura_media_", 
          file_numero_pool_name = "per_mcmc/totale_pool_cluster_", 
          file_numero_pool_positivi_name = "per_mcmc/totale_pool_WNVpos_cluster_", 
          file_pool_size_name = "per_mcmc/mean_pool_size_cluster_", 
          file_zanzare_name = "per_mcmc/zanzare/adulti_medi_",
          vettore_date_catture = "per_mcmc/giorni_cattura.txt", 
          LocOutSimu = "Output_WNV/Simulazioni",
          LocOutMCMC = "Output_WNV/MCMC", 
          NameInParms = "parametri_MediaAllYears",
          NameOutDyn = "dynamics_ParmsMedi_MediaAllYears_")

# check dinamiche medie ----
anno = 2018
quale_cluster = 1
# settimane = scan(vettore_date_catture)
print(anno)
print(quale_cluster)
numero_classi = 7

# se vuoi plottare altro file cambi il nome qua sotto (es 
#paste0("Output_WNV/Simulazioni/dynamics_M_", 
#anno, "_", quale_cluster, 
#".txt"))
nome_file_output_dynamics = paste0("Output_WNV/Simulazioni/dynamics_ParmsMedi_MediaAllYears_", 
                                   anno, "_", quale_cluster, 
                                   ".txt")
scan(nome_file_output_dynamics)

output_dynamics = read.table(nome_file_output_dynamics)
    
sel_MS = seq(1, nrow(output_dynamics), numero_classi)
sel_ME = seq(2, nrow(output_dynamics), numero_classi)
sel_MI = seq(3, nrow(output_dynamics), numero_classi)
sel_BS = seq(4, nrow(output_dynamics), numero_classi)
sel_BE = seq(5, nrow(output_dynamics), numero_classi)
sel_BI = seq(6, nrow(output_dynamics), numero_classi)
sel_BR = seq(7, nrow(output_dynamics), numero_classi)

MS = output_dynamics[sel_MS, ]
ME = output_dynamics[sel_ME, ]
MI = output_dynamics[sel_MI, ]
BS = output_dynamics[sel_BS, ]
BE = output_dynamics[sel_BE, ]
BI = output_dynamics[sel_BI, ]
BR = output_dynamics[sel_BR, ]

mosquito_population = MS + ME + MI
mosquito_prevalence = MI/mosquito_population

bird_population = BS + BE + BI + BR
bird_prevalence = BI/bird_population
bird_seroprevalence = BR/bird_population

pop = bird_seroprevalence # questo se lo cambi cambi la pop che vai a plottare
#bird_population #bird_prevalence #bird_seroprevalence
#mosquito_population #mosquito_prevalence #MI

qmax_pop = c()
qmin_pop = c()
mean_pop = c()
for (j in 1:(ncol(pop))) {
  qmax_pop = c(qmax_pop, quantile(pop[, j], probs = 0.975, 
                                  na.rm = T))
  qmin_pop = c(qmin_pop, quantile(pop[, j], probs = 0.025, 
                                  na.rm = T))
  mean_pop = c(mean_pop, mean(pop[, j], na.rm = T))
}
qmax_pop[which(is.na(qmax_pop))] = 0
qmin_pop[which(is.na(qmin_pop))] = 0
mean_pop[which(is.na(mean_pop))] = 0

colore = 'darkgreen'
ymax = max(qmax_pop)
xmax = length(qmax_pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Bird seroprevalence", anno)) #, quale_cluster))
lines(mean_pop, lwd = 3, col = colore)
poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
                     y = c(qmin_pop, rev(qmax_pop)))
polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
            border = NA)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

# 

###################################
# sensitivity su MuB ----
Simu4Sens(seed = 0,
          numero_cluster = 1,
          anno_inizio = 2016,
          anno_fine = 2018, 
          con_cosa_inizio = 1,
          to_mean_all_years = T, # vuole dire che usiamo la media di tutti gli anni sennò se F uso la media di ogni anno
          
          file_temperatura_name = "per_mcmc/temperatura_media_", 
          file_numero_pool_name = "per_mcmc/totale_pool_cluster_", 
          file_numero_pool_positivi_name = "per_mcmc/totale_pool_WNVpos_cluster_", 
          file_pool_size_name = "per_mcmc/mean_pool_size_cluster_", 
          file_zanzare_name = "per_mcmc/zanzare/adulti_medi_",
          vettore_date_catture = "per_mcmc/giorni_cattura.txt", 
          LocOutSimu = "Output_WNV/Simulazioni",
          LocOutMCMC = "Output_WNV/MCMC", 
          NameInParms = "parametri_sens_MeadiaAllYears_muB",
          NameOutDyn = "dynamics_sens_MeadiaAllYears_muB_")

# check dinamiche ----
anno = 2018
# check per anni diversi length(which(pop1 == pop2))

quale_cluster = 1
# settimane = scan(vettore_date_catture)
print(anno)
print(quale_cluster)
numero_classi = 7

# se vuoi plottare altro file cambi il nome qua sotto (es 
#paste0("Output_WNV/Simulazioni/dynamics_M_", 
#anno, "_", quale_cluster, 
#".txt"))
changing_par = 'muB'
nome_file_output_dynamics = paste0("Output_WNV/Simulazioni/dynamics_sens_MeadiaAllYears_",
                                   changing_par, "_",
                                   anno, "_", quale_cluster, 
                                   ".txt")
scan(nome_file_output_dynamics)

output_dynamics = read.table(nome_file_output_dynamics)

sel_MS = seq(1, nrow(output_dynamics), numero_classi)
sel_ME = seq(2, nrow(output_dynamics), numero_classi)
sel_MI = seq(3, nrow(output_dynamics), numero_classi)
sel_BS = seq(4, nrow(output_dynamics), numero_classi)
sel_BE = seq(5, nrow(output_dynamics), numero_classi)
sel_BI = seq(6, nrow(output_dynamics), numero_classi)
sel_BR = seq(7, nrow(output_dynamics), numero_classi)

MS = output_dynamics[sel_MS, ]
ME = output_dynamics[sel_ME, ]
MI = output_dynamics[sel_MI, ]
BS = output_dynamics[sel_BS, ]
BE = output_dynamics[sel_BE, ]
BI = output_dynamics[sel_BI, ]
BR = output_dynamics[sel_BR, ]

mosquito_population = MS + ME + MI
mosquito_prevalence = MI/mosquito_population

bird_population = BS + BE + BI + BR
bird_prevalence = BI/bird_population
bird_seroprevalence = BR/bird_population

# plot ----
# bird population----
pop = bird_population # questo se lo cambi cambi la pop che vai a plottare

qmax_pop = c()
qmin_pop = c()
mean_pop = c()
for (j in 1:(ncol(pop))) {
  qmax_pop = c(qmax_pop, quantile(pop[, j], probs = 0.975, 
                                  na.rm = T))
  qmin_pop = c(qmin_pop, quantile(pop[, j], probs = 0.025, 
                                  na.rm = T))
  mean_pop = c(mean_pop, mean(pop[, j], na.rm = T))
}
qmax_pop[which(is.na(qmax_pop))] = 0
qmin_pop[which(is.na(qmin_pop))] = 0
mean_pop[which(is.na(mean_pop))] = 0

colore = 'darkblue'
ymax = max(qmax_pop)
xmax = length(qmax_pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Birds' tot pop", anno)) #, quale_cluster))
lines(mean_pop, lwd = 3, col = colore)
poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
                 y = c(qmin_pop, rev(qmax_pop)))
polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
        border = NA)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

ymax = max(pop)
# plot sensitivity----
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Bird population", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i])
}
min = 0/365
max = 8/365
var_par =  seq(min, max, length.out = 100)
#legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.8, 
#      lwd = 4, col = colori[seq(1,100, 9)])
legend('topleft', legend = round(var_par,4)[seq(1,100, 9)]*365 , cex = 0.8, 
       lwd = 4, col = colori[seq(1,100, 9)])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

# bird prevalence----
pop = bird_prevalence # questo se lo cambi cambi la pop che vai a plottare

qmax_pop = c()
qmin_pop = c()
mean_pop = c()
for (j in 1:(ncol(pop))) {
  qmax_pop = c(qmax_pop, quantile(pop[, j], probs = 0.975, 
                                  na.rm = T))
  qmin_pop = c(qmin_pop, quantile(pop[, j], probs = 0.025, 
                                  na.rm = T))
  mean_pop = c(mean_pop, mean(pop[, j], na.rm = T))
}
qmax_pop[which(is.na(qmax_pop))] = 0
qmin_pop[which(is.na(qmin_pop))] = 0
mean_pop[which(is.na(mean_pop))] = 0

colore = 'orange'
ymax = max(qmax_pop)
xmax = length(qmax_pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Birds' prev", anno)) #, quale_cluster))
lines(mean_pop, lwd = 3, col = colore)
poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
                 y = c(qmin_pop, rev(qmax_pop)))
polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
        border = NA)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

ymax = max(pop)
# plot sensitivity----
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Bird prevalence", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i])
}
min = 0/365
max = 8/365
var_par =  seq(min, max, length.out = 100)
#legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.8, 
#      lwd = 4, col = colori[seq(1,100, 9)])
legend('topleft', legend = round(var_par,4)[seq(1,100, 9)]*365 , cex = 0.8, 
       lwd = 4, col = colori[seq(1,100, 9)])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

# bird seroprevalence----
pop = bird_seroprevalence # questo se lo cambi cambi la pop che vai a plottare

qmax_pop = c()
qmin_pop = c()
mean_pop = c()
for (j in 1:(ncol(pop))) {
  qmax_pop = c(qmax_pop, quantile(pop[, j], probs = 0.975, 
                                  na.rm = T))
  qmin_pop = c(qmin_pop, quantile(pop[, j], probs = 0.025, 
                                  na.rm = T))
  mean_pop = c(mean_pop, mean(pop[, j], na.rm = T))
}
qmax_pop[which(is.na(qmax_pop))] = 0
qmin_pop[which(is.na(qmin_pop))] = 0
mean_pop[which(is.na(mean_pop))] = 0

colore = 'purple'
ymax = max(qmax_pop)
xmax = length(qmax_pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Birds' seroprev", anno)) #, quale_cluster))
lines(mean_pop, lwd = 3, col = colore)
poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
                 y = c(qmin_pop, rev(qmax_pop)))
polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
        border = NA)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

ymax = max(pop)
# plot sensitivity----
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Bird seroprevalence", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i])
}
min = 0/365
max = 8/365
var_par =  seq(min, max, length.out = 100)
#legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.8, 
#      lwd = 4, col = colori[seq(1,100, 9)])
legend('topleft', legend = round(var_par,4)[seq(1,100, 9)]*365 , cex = 0.8, 
       lwd = 4, col = colori[seq(1,100, 9)])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

# zanzare infette ----
pop = MI # questo se lo cambi cambi la pop che vai a plottare

qmax_pop = c()
qmin_pop = c()
mean_pop = c()
for (j in 1:(ncol(pop))) {
  qmax_pop = c(qmax_pop, quantile(pop[, j], probs = 0.975, 
                                  na.rm = T))
  qmin_pop = c(qmin_pop, quantile(pop[, j], probs = 0.025, 
                                  na.rm = T))
  mean_pop = c(mean_pop, mean(pop[, j], na.rm = T))
}
qmax_pop[which(is.na(qmax_pop))] = 0
qmin_pop[which(is.na(qmin_pop))] = 0
mean_pop[which(is.na(mean_pop))] = 0

colore = 'violet'
ymax = max(qmax_pop)
xmax = length(qmax_pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosq prev", anno)) #, quale_cluster))
lines(mean_pop, lwd = 3, col = colore)
poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
                 y = c(qmin_pop, rev(qmax_pop)))
polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
        border = NA)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

# 
# plot sensitivity ----
ymax = max(pop)

plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito Infected", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i])
}
min = 0/365
max = 8/365
var_par =  seq(min, max, length.out = 100)

legend('topleft', legend = round(var_par,4)[seq(1,100, 9)]*365 , cex = 0.8, 
       lwd = 4, col = colori[seq(1,100, 9)])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

# mosquito prevalence ----
pop = mosquito_prevalence # questo se lo cambi cambi la pop che vai a plottare

qmax_pop = c()
qmin_pop = c()
mean_pop = c()
for (j in 1:(ncol(pop))) {
  qmax_pop = c(qmax_pop, quantile(pop[, j], probs = 0.975, 
                                  na.rm = T))
  qmin_pop = c(qmin_pop, quantile(pop[, j], probs = 0.025, 
                                  na.rm = T))
  mean_pop = c(mean_pop, mean(pop[, j], na.rm = T))
}
qmax_pop[which(is.na(qmax_pop))] = 0
qmin_pop[which(is.na(qmin_pop))] = 0
mean_pop[which(is.na(mean_pop))] = 0

colore = 'violet'
ymax = max(qmax_pop)
xmax = length(qmax_pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosq prev", anno)) #, quale_cluster))
lines(mean_pop, lwd = 3, col = colore)
poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
                 y = c(qmin_pop, rev(qmax_pop)))
polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
        border = NA)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

# 
# plot sensitivity ----
ymax = max(pop)

plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosqito prevalence", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i])
}
min = 0/365
max = 8/365
var_par =  seq(min, max, length.out = 100)

legend('topleft', legend = round(var_par,4)[seq(1,100, 9)]*365 , cex = 0.8, 
       lwd = 4, col = colori[seq(1,100, 9)])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

###################################
# sensitivity su s ----
Simu4Sens(seed = 0,
          numero_cluster = 1,
          anno_inizio = 2016,
          anno_fine = 2018, 
          con_cosa_inizio = 1,
          to_mean_all_years = T, # vuole dire che usiamo la media di tutti gli anni sennò se F uso la media di ogni anno
          
          file_temperatura_name = "per_mcmc/temperatura_media_", 
          file_numero_pool_name = "per_mcmc/totale_pool_cluster_", 
          file_numero_pool_positivi_name = "per_mcmc/totale_pool_WNVpos_cluster_", 
          file_pool_size_name = "per_mcmc/mean_pool_size_cluster_", 
          file_zanzare_name = "per_mcmc/zanzare/adulti_medi_",
          vettore_date_catture = "per_mcmc/giorni_cattura.txt", 
          LocOutSimu = "Output_WNV/Simulazioni",
          LocOutMCMC = "Output_WNV/MCMC", 
          NameInParms = "parametri_sens_MeadiaAllYears_s",
          NameOutDyn = "dynamics_sens_MeadiaAllYears_s_")

# check dinamiche ----
anno = 2018
quale_cluster = 1
# settimane = scan(vettore_date_catture)
print(anno)
print(quale_cluster)
numero_classi = 7

# se vuoi plottare altro file cambi il nome qua sotto (es 
#paste0("Output_WNV/Simulazioni/dynamics_M_", 
#anno, "_", quale_cluster, 
#".txt"))
changing_par = 's'
nome_file_output_dynamics = paste0("Output_WNV/Simulazioni/dynamics_sens_MeadiaAllYears_",
                                   changing_par, "_",
                                   anno, "_", quale_cluster, 
                                   ".txt")
output_dynamics = read.table(nome_file_output_dynamics)

sel_MS = seq(1, nrow(output_dynamics), numero_classi)
sel_ME = seq(2, nrow(output_dynamics), numero_classi)
sel_MI = seq(3, nrow(output_dynamics), numero_classi)
sel_BS = seq(4, nrow(output_dynamics), numero_classi)
sel_BE = seq(5, nrow(output_dynamics), numero_classi)
sel_BI = seq(6, nrow(output_dynamics), numero_classi)
sel_BR = seq(7, nrow(output_dynamics), numero_classi)

MS = output_dynamics[sel_MS, ]
ME = output_dynamics[sel_ME, ]
MI = output_dynamics[sel_MI, ]
BS = output_dynamics[sel_BS, ]
BE = output_dynamics[sel_BE, ]
BI = output_dynamics[sel_BI, ]
BR = output_dynamics[sel_BR, ]

mosquito_population = MS + ME + MI
mosquito_prevalence = MI/mosquito_population

bird_population = BS + BE + BI + BR
bird_prevalence = BI/bird_population
bird_seroprevalence = BR/bird_population

# plot ----
#birds population ----
pop = bird_population # questo se lo cambi cambi la pop che vai a plottare

qmax_pop = c()
qmin_pop = c()
mean_pop = c()
for (j in 1:(ncol(pop))) {
  qmax_pop = c(qmax_pop, quantile(pop[, j], probs = 0.975, 
                                  na.rm = T))
  qmin_pop = c(qmin_pop, quantile(pop[, j], probs = 0.025, 
                                  na.rm = T))
  mean_pop = c(mean_pop, mean(pop[, j], na.rm = T))
}
qmax_pop[which(is.na(qmax_pop))] = 0
qmin_pop[which(is.na(qmin_pop))] = 0
mean_pop[which(is.na(mean_pop))] = 0

colore = 'darkgreen'
ymax = max(qmax_pop)
xmax = length(qmax_pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Birds' tot pop", anno)) #, quale_cluster))
lines(mean_pop, lwd = 3, col = colore)
poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
                 y = c(qmin_pop, rev(qmax_pop)))
polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
        border = NA)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

ymax = max(pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Bird population", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i])
}
min = 5
max = 20
var_par =  seq(min, max, length.out = 100)
#legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.8, 
#      lwd = 4, col = colori[seq(1,100, 9)])
legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.7, 
       lwd = 4, col = colori[seq(1,100, 9)])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

#birds prevalence ----
pop = bird_prevalence # questo se lo cambi cambi la pop che vai a plottare

qmax_pop = c()
qmin_pop = c()
mean_pop = c()
for (j in 1:(ncol(pop))) {
  qmax_pop = c(qmax_pop, quantile(pop[, j], probs = 0.975, 
                                  na.rm = T))
  qmin_pop = c(qmin_pop, quantile(pop[, j], probs = 0.025, 
                                  na.rm = T))
  mean_pop = c(mean_pop, mean(pop[, j], na.rm = T))
}
qmax_pop[which(is.na(qmax_pop))] = 0
qmin_pop[which(is.na(qmin_pop))] = 0
mean_pop[which(is.na(mean_pop))] = 0

colore = 'orange'
ymax = max(qmax_pop)
xmax = length(qmax_pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Birds' tot pop", anno)) #, quale_cluster))
lines(mean_pop, lwd = 3, col = colore)
poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
                 y = c(qmin_pop, rev(qmax_pop)))
polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
        border = NA)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

ymax = max(pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Bird prevalence", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i])
}
min = 5
max = 20
var_par =  seq(min, max, length.out = 100)
#legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.8, 
#      lwd = 4, col = colori[seq(1,100, 9)])
legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.7, 
       lwd = 4, col = colori[seq(1,100, 9)])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)


#birds seroprevalence ----
pop = bird_seroprevalence # questo se lo cambi cambi la pop che vai a plottare

qmax_pop = c()
qmin_pop = c()
mean_pop = c()
for (j in 1:(ncol(pop))) {
  qmax_pop = c(qmax_pop, quantile(pop[, j], probs = 0.975, 
                                  na.rm = T))
  qmin_pop = c(qmin_pop, quantile(pop[, j], probs = 0.025, 
                                  na.rm = T))
  mean_pop = c(mean_pop, mean(pop[, j], na.rm = T))
}
qmax_pop[which(is.na(qmax_pop))] = 0
qmin_pop[which(is.na(qmin_pop))] = 0
mean_pop[which(is.na(mean_pop))] = 0

colore = 'purple'
ymax = max(qmax_pop)
xmax = length(qmax_pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Birds' tot pop", anno)) #, quale_cluster))
lines(mean_pop, lwd = 3, col = colore)
poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
                 y = c(qmin_pop, rev(qmax_pop)))
polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
        border = NA)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

ymax = max(pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Bird seroprevalence", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i])
}
min = 5
max = 20
var_par =  seq(min, max, length.out = 100)
#legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.8, 
#      lwd = 4, col = colori[seq(1,100, 9)])
legend('bottomright', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.7, 
       lwd = 4, col = colori[seq(1,100, 9)])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)


# zanzare infette ----
pop = MI # questo se lo cambi cambi la pop che vai a plottare

qmax_pop = c()
qmin_pop = c()
mean_pop = c()
for (j in 1:(ncol(pop))) {
  qmax_pop = c(qmax_pop, quantile(pop[, j], probs = 0.975, 
                                  na.rm = T))
  qmin_pop = c(qmin_pop, quantile(pop[, j], probs = 0.025, 
                                  na.rm = T))
  mean_pop = c(mean_pop, mean(pop[, j], na.rm = T))
}
qmax_pop[which(is.na(qmax_pop))] = 0
qmin_pop[which(is.na(qmin_pop))] = 0
mean_pop[which(is.na(mean_pop))] = 0

colore = 'violet'
ymax = max(qmax_pop)
xmax = length(qmax_pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosq prev", anno)) #, quale_cluster))
lines(mean_pop, lwd = 3, col = colore)
poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
                 y = c(qmin_pop, rev(qmax_pop)))
polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
        border = NA)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

# 
# plot sensitivity ----
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito infected", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i])
}
legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.8, 
       lwd = 4, col = colori[seq(1,100, 9)])
legend('topleft', legend = round(var_par,4)[seq(1,100, 9)], cex = 0.8, 
       lwd = 4, col = colori[seq(1,100, 9)])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

# zanzare prevalence ----
pop = mosquito_prevalence # questo se lo cambi cambi la pop che vai a plottare

qmax_pop = c()
qmin_pop = c()
mean_pop = c()
for (j in 1:(ncol(pop))) {
  qmax_pop = c(qmax_pop, quantile(pop[, j], probs = 0.975, 
                                  na.rm = T))
  qmin_pop = c(qmin_pop, quantile(pop[, j], probs = 0.025, 
                                  na.rm = T))
  mean_pop = c(mean_pop, mean(pop[, j], na.rm = T))
}
qmax_pop[which(is.na(qmax_pop))] = 0
qmin_pop[which(is.na(qmin_pop))] = 0
mean_pop[which(is.na(mean_pop))] = 0

colore = 'violet'
ymax = max(qmax_pop)
xmax = length(qmax_pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosq prev", anno)) #, quale_cluster))
lines(mean_pop, lwd = 3, col = colore)
poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
                 y = c(qmin_pop, rev(qmax_pop)))
polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
        border = NA)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

# 
# plot sensitivity ----
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito prevalence", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i])
}
legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.8, 
       lwd = 4, col = colori[seq(1,100, 9)])
legend('topleft', legend = round(var_par,4)[seq(1,100, 9)], cex = 0.8, 
       lwd = 4, col = colori[seq(1,100, 9)])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)


###################################
# sensitivity su phi ----
Simu4Sens(seed = 0,
          numero_cluster = 1,
          anno_inizio = 2016,
          anno_fine = 2018, 
          con_cosa_inizio = 1,
          to_mean_all_years = T, # vuole dire che usiamo la media di tutti gli anni sennò se F uso la media di ogni anno
          
          file_temperatura_name = "per_mcmc/temperatura_media_", 
          file_numero_pool_name = "per_mcmc/totale_pool_cluster_", 
          file_numero_pool_positivi_name = "per_mcmc/totale_pool_WNVpos_cluster_", 
          file_pool_size_name = "per_mcmc/mean_pool_size_cluster_", 
          file_zanzare_name = "per_mcmc/zanzare/adulti_medi_",
          vettore_date_catture = "per_mcmc/giorni_cattura.txt", 
          LocOutSimu = "Output_WNV/Simulazioni",
          LocOutMCMC = "Output_WNV/MCMC", 
          NameInParms = "parametri_sens_MeadiaAllYears_phi",
          NameOutDyn = "dynamics_sens_MeadiaAllYears_phi_")

# check dinamiche ----
anno = 2018
quale_cluster = 1
# settimane = scan(vettore_date_catture)
print(anno)
print(quale_cluster)
numero_classi = 7

# se vuoi plottare altro file cambi il nome qua sotto (es 
#paste0("Output_WNV/Simulazioni/dynamics_M_", 
#anno, "_", quale_cluster, 
#".txt"))
changing_par = 'phi'
nome_file_output_dynamics = paste0("Output_WNV/Simulazioni/dynamics_sens_MeadiaAllYears_",
                                   changing_par, "_",
                                   anno, "_", quale_cluster, 
                                   ".txt")
output_dynamics = read.table(nome_file_output_dynamics)

sel_MS = seq(1, nrow(output_dynamics), numero_classi)
sel_ME = seq(2, nrow(output_dynamics), numero_classi)
sel_MI = seq(3, nrow(output_dynamics), numero_classi)
sel_BS = seq(4, nrow(output_dynamics), numero_classi)
sel_BE = seq(5, nrow(output_dynamics), numero_classi)
sel_BI = seq(6, nrow(output_dynamics), numero_classi)
sel_BR = seq(7, nrow(output_dynamics), numero_classi)

MS = output_dynamics[sel_MS, ]
ME = output_dynamics[sel_ME, ]
MI = output_dynamics[sel_MI, ]
BS = output_dynamics[sel_BS, ]
BE = output_dynamics[sel_BE, ]
BI = output_dynamics[sel_BI, ]
BR = output_dynamics[sel_BR, ]

mosquito_population = MS + ME + MI
mosquito_prevalence = MI/mosquito_population

bird_population = BS + BE + BI + BR
bird_prevalence = BI/bird_population
bird_seroprevalence = BR/bird_population

# plot ----
#birds population ----
pop = bird_population # questo se lo cambi cambi la pop che vai a plottare

qmax_pop = c()
qmin_pop = c()
mean_pop = c()
for (j in 1:(ncol(pop))) {
  qmax_pop = c(qmax_pop, quantile(pop[, j], probs = 0.975, 
                                  na.rm = T))
  qmin_pop = c(qmin_pop, quantile(pop[, j], probs = 0.025, 
                                  na.rm = T))
  mean_pop = c(mean_pop, mean(pop[, j], na.rm = T))
}
qmax_pop[which(is.na(qmax_pop))] = 0
qmin_pop[which(is.na(qmin_pop))] = 0
mean_pop[which(is.na(mean_pop))] = 0

colore = 'darkgreen'
ymax = max(qmax_pop)
xmax = length(qmax_pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Birds' tot pop", anno)) #, quale_cluster))
lines(mean_pop, lwd = 3, col = colore)
poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
                 y = c(qmin_pop, rev(qmax_pop)))
polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
        border = NA)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

ymax = max(pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Bird population", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i])
}
min = 0.33
max = 0.66
var_par =  seq(min, max, length.out = 100) # importarlo dal file di simulazioni
#legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.8, 
#      lwd = 4, col = colori[seq(1,100, 9)])
legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.7, 
       lwd = 4, col = colori[seq(1,100, 9)])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

#birds prevalence ----
pop = bird_prevalence # questo se lo cambi cambi la pop che vai a plottare

qmax_pop = c()
qmin_pop = c()
mean_pop = c()
for (j in 1:(ncol(pop))) {
  qmax_pop = c(qmax_pop, quantile(pop[, j], probs = 0.975, 
                                  na.rm = T))
  qmin_pop = c(qmin_pop, quantile(pop[, j], probs = 0.025, 
                                  na.rm = T))
  mean_pop = c(mean_pop, mean(pop[, j], na.rm = T))
}
qmax_pop[which(is.na(qmax_pop))] = 0
qmin_pop[which(is.na(qmin_pop))] = 0
mean_pop[which(is.na(mean_pop))] = 0

colore = 'orange'
ymax = max(qmax_pop)
xmax = length(qmax_pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Birds' prev", anno)) #, quale_cluster))
lines(mean_pop, lwd = 3, col = colore)
poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
                 y = c(qmin_pop, rev(qmax_pop)))
polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
        border = NA)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

ymax = max(pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Bird prevalence", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i])
}
min = 0.33
max = 0.66
var_par =  seq(min, max, length.out = 100) # importarlo dal file di simulazioni
#legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.8, 
#      lwd = 4, col = colori[seq(1,100, 9)])
legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.7, 
       lwd = 4, col = colori[seq(1,100, 9)])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

#birds seroprevalence ----
pop = bird_seroprevalence # questo se lo cambi cambi la pop che vai a plottare

qmax_pop = c()
qmin_pop = c()
mean_pop = c()
for (j in 1:(ncol(pop))) {
  qmax_pop = c(qmax_pop, quantile(pop[, j], probs = 0.975, 
                                  na.rm = T))
  qmin_pop = c(qmin_pop, quantile(pop[, j], probs = 0.025, 
                                  na.rm = T))
  mean_pop = c(mean_pop, mean(pop[, j], na.rm = T))
}
qmax_pop[which(is.na(qmax_pop))] = 0
qmin_pop[which(is.na(qmin_pop))] = 0
mean_pop[which(is.na(mean_pop))] = 0

colore = 'purple'
ymax = max(qmax_pop)
xmax = length(qmax_pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Bird seroprevalence", anno)) #, quale_cluster))
lines(mean_pop, lwd = 3, col = colore)
poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
                 y = c(qmin_pop, rev(qmax_pop)))
polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
        border = NA)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

ymax = max(pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("bird seroprevalence", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i])
}
min = 0.33
max = 0.66
var_par =  seq(min, max, length.out = 100) # importarlo dal file di simulazioni
#legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.8, 
#      lwd = 4, col = colori[seq(1,100, 9)])
legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.7, 
       lwd = 4, col = colori[seq(1,100, 9)])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)



# zanzare infette ----
pop = MI # questo se lo cambi cambi la pop che vai a plottare

qmax_pop = c()
qmin_pop = c()
mean_pop = c()
for (j in 1:(ncol(pop))) {
  qmax_pop = c(qmax_pop, quantile(pop[, j], probs = 0.975, 
                                  na.rm = T))
  qmin_pop = c(qmin_pop, quantile(pop[, j], probs = 0.025, 
                                  na.rm = T))
  mean_pop = c(mean_pop, mean(pop[, j], na.rm = T))
}
qmax_pop[which(is.na(qmax_pop))] = 0
qmin_pop[which(is.na(qmin_pop))] = 0
mean_pop[which(is.na(mean_pop))] = 0

colore = 'violet'
ymax = max(qmax_pop)
xmax = length(qmax_pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("MI", anno)) #, quale_cluster))
lines(mean_pop, lwd = 3, col = colore)
poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
                 y = c(qmin_pop, rev(qmax_pop)))
polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
        border = NA)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

# 
# plot sensitivity ----
ymax = max(pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito infected", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i])
}
legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.8, 
       lwd = 4, col = colori[seq(1,100, 9)])
legend('topleft', legend = round(var_par,4)[seq(1,100, 9)], cex = 0.8, 
       lwd = 4, col = colori[seq(1,100, 9)])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

# plot extra phi 1
pop_tmp = pop[1:10,]
ymax = max(pop_tmp)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito infected 1-10", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop_tmp)) {
  lines(as.numeric(pop_tmp[i,]), col = colori[i])
}
legend('topleft', legend = round(var_par,4)[1:10] , cex = 0.8, 
       lwd = 4, col = colori[1:10])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

# plot extra phi 2
pop_tmp = pop[11:20,]
ymax = max(pop_tmp)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito infected 11-20", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop_tmp)) {
  lines(as.numeric(pop_tmp[i,]), col = colori[i+10])
}
legend('topright', legend = round(var_par,4)[11:20] , cex = 0.8, 
       lwd = 4, col = colori[11:20])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

# plot extra phi 

# plot extra phi 3
pop_tmp = pop[21:30,]
ymax = max(pop_tmp)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito infected 21-30", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop_tmp)) {
  lines(as.numeric(pop_tmp[i,]), col = colori[i+20])
}
legend('topright', legend = round(var_par,4)[21:30] , cex = 0.8, 
       lwd = 4, col = colori[21:30])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

# plot extra phi 1
pop_tmp = pop[41:50,]
ymax = max(pop_tmp)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito infected 41-50", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop_tmp)) {
  lines(as.numeric(pop_tmp[i,]), col = colori[i+40])
}
legend('topright', legend = round(var_par,4)[41:50] , cex = 0.8, 
       lwd = 4, col = colori[41:50])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)


# plot extra phi 1
pop_tmp = pop[81:90,]
ymax = max(pop_tmp)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito infected 81-90", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop_tmp)) {
  lines(as.numeric(pop_tmp[i,]), col = colori[i+80])
}
legend('topleft', legend = round(var_par,4)[81:90] , cex = 0.8, 
       lwd = 4, col = colori[81:90])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)
# plot extra phi 1
pop_tmp = pop[91:100,]
ymax = max(pop_tmp)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito infected 91-100", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop_tmp)) {
  lines(as.numeric(pop_tmp[i,]), col = colori[i+90])
}
legend('topleft', legend = round(var_par,4)[91:100] , cex = 0.8, 
       lwd = 4, col = colori[91:100])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)


#zanzare prevalence ----
pop = mosquito_prevalence # questo se lo cambi cambi la pop che vai a plottare

qmax_pop = c()
qmin_pop = c()
mean_pop = c()
for (j in 1:(ncol(pop))) {
  qmax_pop = c(qmax_pop, quantile(pop[, j], probs = 0.975, 
                                  na.rm = T))
  qmin_pop = c(qmin_pop, quantile(pop[, j], probs = 0.025, 
                                  na.rm = T))
  mean_pop = c(mean_pop, mean(pop[, j], na.rm = T))
}
qmax_pop[which(is.na(qmax_pop))] = 0
qmin_pop[which(is.na(qmin_pop))] = 0
mean_pop[which(is.na(mean_pop))] = 0

colore = 'violet'
ymax = max(qmax_pop)
xmax = length(qmax_pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito prevalence", anno)) #, quale_cluster))
lines(mean_pop, lwd = 3, col = colore)
poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
                 y = c(qmin_pop, rev(qmax_pop)))
polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
        border = NA)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)

ymax = max(pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito prevalence", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i])
}
min = 0.33
max = 0.66
var_par =  seq(min, max, length.out = 100) # importarlo dal file di simulazioni
#legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.8, 
#      lwd = 4, col = colori[seq(1,100, 9)])
legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.7, 
       lwd = 4, col = colori[seq(1,100, 9)])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = T), cex.axis = 1)


