rm(list=ls())

# choose ----
par = 'muB' 
par = 'phi'  
par = 's' 

anno = 2018

#
# prep ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(WNModFEMRL)
library(RColorBrewer)
#display.brewer.all()
#display.brewer.pal(11,"Spectral") # Spectral sono 11 colori

tmax = 180
file_numero_pool_name = "/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/WNModFEMRL/per_mcmc/totale_pool_cluster_"
file_numero_pool_positivi_name = "/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/WNModFEMRL/per_mcmc/totale_pool_WNVpos_cluster_"
file_pool_size_name = "/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/WNModFEMRL/per_mcmc/mean_pool_size_cluster_"
vettore_date_catture = "/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/WNModFEMRL/per_mcmc/giorni_cattura.txt"
cex_main = 2
cex_axes = 1.2
cex_lab_axes = 1.2
margins_plot = c(7, 7, 4, 2)
ymax_plot = 10

settimane = scan(vettore_date_catture)
  
colori = rainbow(n = 1)
  
quanti_dentro95 = 0
  
quante_osservazioni = 0

numero_pool = read.table(paste0(file_numero_pool_name, anno))
numero_pool_positivi = read.table(paste0(file_numero_pool_positivi_name, anno))
pool_size = read.table(paste0(file_pool_size_name, anno))
numero_classi = 7

quando_ho_pool = which(pool_size[1, ]>0)
pool_positivi_simulati = c()
con_cosa_inizio = 1
nome_file_output_dynamics = paste0('/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/WNModFEMRL/Output_WNV/Simulazioni/dynamics_sens_MeadiaAllYears_',
                                   par,'_', anno, '_1.txt')
output_dynamics = read.table(nome_file_output_dynamics)

# seleziono solo i parametri significativi per i plot da mostrare
nome_file_parametri_sens = paste0('/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/WNModFEMRL/Output_WNV/MCMC/parametri_sens_MeadiaAllYears_',
                                   par,'.txt')
parametri_sens = read.table(nome_file_parametri_sens)
colnames(parametri_sens) = c("p", "B0", "pR", "b1", "muB", "s", "phi", 
                   "niB", "recB")
# decido quali mu sono significativi
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

mosquito_prevalence = MI/(MS + ME + MI)

if(par == 'muB') {
  par_vec = parametri_sens[,'muB']
  selezione_par = c(#which(round(par_vec*365,1) == 0.5)[1], 
    which(round(par_vec*365,1) == 1)[1], 
    #which(round(par_vec*365,1) == 1.5)[1], 
    which(round(par_vec*365,1) == 2)[1], 
    #which(round(par_vec*365,1) == 2.5)[1], 
    which(round(par_vec*365,1) == 3)[1], 
    #which(round(par_vec*365,1) == 3.5)[1], 
    which(round(par_vec*365,1) == 4)[1], 
    #which(round(par_vec*365,1) == 4.5)[1], 
    which(round(par_vec*365,1) == 5)[1],
    #which(round(par_vec*365,1) == 5.5)[1],
    which(round(par_vec*365,1) == 6)[1],
    #which(round(par_vec*365,1) == 6.5)[1],
    which(round(par_vec*365,1) == 7)[1],
    #which(round(par_vec*365,1) == 7.5)[1],
    which(round(par_vec*365,1) == 8)[1])
  par_short = par_vec[selezione_par]
  colori = brewer.pal(length(selezione_par), "Spectral") # length massima 11
  
}
if(par == 'phi') {
  par_vec = parametri_sens[,'phi']
  
  birth_pulses = c()
  maxes = c()
  for (phi_tmp in par_vec) {
    print(phi_tmp)
    
    birth_pulse = with(as.list(parametri_sens[1,]), 
                       muB * exp(-s * sin(pi * (seq(0, 365)/365 - phi_tmp))^2)/besselI(s/2, 0, T))
    birth_pulses = cbind(birth_pulses,
                         birth_pulse)
    maxes = c(maxes, which(birth_pulse == max(birth_pulse)))
  }

  selezione_par = c(which(round(maxes) == 121), # 1 May
                    which(round(maxes) == 136), # 15 May
                    which(round(maxes) == 152), # 1 Jun
                    which(round(maxes) == 166), # 15 Jun
                    which(round(maxes) == 182), # 1 Jul
                    which(round(maxes) == 196), # 15 jul
                    which(round(maxes) == 213), # 1 aug
                    which(round(maxes) == 227) # 15 aug
  )
  par_short = par_vec[selezione_par]
  colori = brewer.pal(length(selezione_par), "Spectral") # length massima 11
}
if(par == 's') {
  par_vec = parametri_sens[,'s']
  
  selezione_par = seq(1,100,10)
  par_short = par_vec[selezione_par]
  colori = brewer.pal(length(selezione_par), "Spectral") # length massima 11
}

# birth pulse per i parametri selezionati 
if(par == 'muB') {
  birth_pulses = c()
  for (tmp in par_short) {
    print(tmp)
    birth_pulse = with(as.list(parametri_sens[1,]), 
                       tmp * exp(-s * sin(pi * (seq(0, 365)/365 - phi))^2)/besselI(s/2, 0, T))
    
    
    birth_pulses = cbind(birth_pulses,
                         birth_pulse)
  }
}
if(par == 'phi') {
  birth_pulses = c()
  for (tmp in par_short) {
    print(tmp)
    birth_pulse = with(as.list(parametri_sens[1,]), 
                       muB * exp(-s * sin(pi * (seq(0, 365)/365 - tmp))^2)/besselI(s/2, 0, T))
    
    
    birth_pulses = cbind(birth_pulses,
                         birth_pulse)
  }
}
if(par == 's') {
  birth_pulses = c()
  for (tmp in par_short) {
    print(tmp)
    birth_pulse = with(as.list(parametri_sens[1,]), 
                       muB * exp(-tmp* sin(pi * (seq(0, 365)/365 - phi))^2)/besselI(tmp/2, 0, T))
    
    
    birth_pulses = cbind(birth_pulses,
                         birth_pulse)
  }
}

MS_short = MS[selezione_par,]
ME_short = ME[selezione_par,]
MI_short = MI[selezione_par,]

BS_short = BS[selezione_par,]
BE_short = BE[selezione_par,]
BI_short = BI[selezione_par,]
BR_short = BR[selezione_par,]

mosquito_prevalence_selected_parms = MI_short/(MS_short + ME_short + MI_short)

pool_pos_pivot = c()
pool_pos_list = list()

#mosquito_prevalence_selected_parms
capture_days = settimane[quando_ho_pool]-120
capture_days = capture_days[which(capture_days<tmax)]

mosquito_prevalence_short = mosquito_prevalence_selected_parms[,capture_days]

for (j in 1:nrow(mosquito_prevalence_selected_parms)) {
  pool_pos_simu = c()
  for (capture_day_index in 1:length(capture_days)) {
    prob_mosquito_neg = 1-mosquito_prevalence_short[j,capture_day_index]
    prob_pool_neg = (prob_mosquito_neg)^as.numeric(pool_size[1,capture_day_index])
    prob_pool_positivo =1-prob_pool_neg
    
    pool_analizzati = numero_pool[1, capture_day_index]
    pool_positivi = numero_pool_positivi[1, capture_day_index]
    pool_pos_simu = cbind(pool_pos_simu, rbinom(n = 100, size = pool_analizzati, prob = prob_pool_positivo))
  }
  #colnames(pool_pos_simu) = seq(1:18)
  pool_pos_list[[j]] = pool_pos_simu
  pool_pos_pivot = rbind(pool_pos_pivot, pivot_longer(as.data.frame(pool_pos_simu), cols = 1:17) %>% 
                           mutate(par = paste0('mu', j)))
}

################ plot ----
# plot birth pulse con i parametri scelti ----
par(mfrow = c(1, 1), mar = c(5,4,4,3))
ymax = round(max(birth_pulses), 3)
xmax = nrow(birth_pulses)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Birth pulse")) #, quale_cluster))
for (i in 1:ncol(birth_pulses)) {
  lines(as.numeric(birth_pulses[,i]), col = colori[i], lwd = 2)
}
if(par == 'muB')
  leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
if(par == 'phi')
  leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul', '01 Aug', '15 Aug')
if(par == 's')
  leg = paste('s=', round(par_vec[selezione_par],1))
axis(1, at = c(1, 
               1 + 31, 
               1 + 31 + 30, 
               1 + 31 + 30 + 31, 
               1 + 31 + 30 + 31 + 31, 
               1 + 31 + 30 + 31 + 31 + 30, 
               1 + 31 + 30 + 31 + 31 + 30 + 31,
               1 + 31 + 30 + 31 + 31 + 30 + 31 + 31,
               1 + 31 + 30 + 31 + 31 + 30 + 31 + 31 + 30,
               1 + 31 + 30 + 31 + 31 + 30 + 31  + 31 + 30 + 31,
               1 + 31 + 30 + 31 + 31 + 30 + 31  + 31 + 30 + 31 + 30,
               1 + 31 + 30 + 31 + 31 + 30 + 31  + 31 + 30 + 31 + 30 + 31), 
     labels = c('Jan', 'Feb', 'Mar', "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", 'Nov', 'Dec'), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
     labels = format(seq(0, ymax, length.out = 10), 
                     digits = 2, scientific = F), cex.axis = 1)
legend('topright', legend = leg , cex = 0.8,  lwd = 2, col = colori, bty = 'n')

# bird pop ----
par(mfrow = c(1, 1), mar = c(5,4,4,3))
pop = BS_short + BI_short + BE_short + BR_short
ymax = round(max(pop))+1
xmax = ncol(pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste('different' ,par, "N of birds")) #, quale_cluster))
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i], lwd = 2)
}
if(par == 'muB')
  leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
if(par == 'phi')
  leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul', '01 Aug', '15 Aug')
if(par == 's')
  leg = paste('s=', round(par_vec[selezione_par],1))
axis(1, at = c(1, 
               1 + 31, 
               1 + 31 + 30, 
               1 + 31 + 30 + 31, 
               1 + 31 + 30 + 31 + 31, 
               1 + 31 + 30 + 31 + 31 + 30, 
               1 + 31 + 30 + 31 + 31 + 30 + 31), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
     labels = format(seq(0, ymax, length.out = 10), 
                     digits = 2, scientific = F), cex.axis = 1)
legend('topleft', legend = leg , cex = 0.8,  lwd = 2, col = colori, bty = 'n')

par(mfrow = c(1, 1), mar = c(5,4,4,3))
pop = BS_short + BI_short + BE_short + BR_short
ymax = round(max(pop[,0:(3*30)]))
xmax = ncol(pop)
plot(0, col = "white", xlim = c(0, 3*30), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste('different' ,par, "N of birds")) #, quale_cluster))
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i], lwd = 2)
}
if(par == 'muB')
  leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
if(par == 'phi')
  leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul', '01 Aug', '15 Aug')
if(par == 's')
  leg = paste('s=', round(par_vec[selezione_par],1))
axis(1, at = c(1, 
               1 + 31, 
               1 + 31 + 30, 
               1 + 31 + 30 + 31, 
               1 + 31 + 30 + 31 + 31, 
               1 + 31 + 30 + 31 + 31 + 30, 
               1 + 31 + 30 + 31 + 31 + 30 + 31), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
     labels = format(seq(0, ymax, length.out = 10), 
                     digits = 2, scientific = F), cex.axis = 1)

par(mfrow = c(1, 1), mar = c(5,4,4,3))
pop = BS_short + BI_short + BE_short + BR_short
ymax = round(max(pop[,62:95]))
xmax = ncol(pop)
plot(0, col = "white", xlim = c(62, 95), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste('different' ,par, "N of birds")) #, quale_cluster))
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i], lwd = 2)
}
if(par == 'muB')
  leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
if(par == 'phi')
  leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul', '01 Aug', '15 Aug')
if(par == 's')
  leg = paste('s=', round(par_vec[selezione_par],1))
axis(1, at = seq(62, 102, 7), 
     labels = c('01 Jun', "7 Jun", "14 Jun", "21 Jun", "28 Jun","05 Jul"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
     labels = format(seq(0, ymax, length.out = 10), 
                     digits = 2, scientific = F), cex.axis = 1)


# BS----
par(mfrow = c(1, 1), mar = c(5,4,4,3))
pop = BS_short
ymax = round(max(pop))+1
xmax = ncol(pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Bird susceptible", anno)) #, quale_cluster))
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i], lwd = 2)
}
if(par == 'muB')
  leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
if(par == 'phi')
  leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul', '01 Aug', '15 Aug')
if(par == 's')
  leg = paste('s=', round(par_vec[selezione_par],1))
axis(1, at = c(1, 
               1 + 31, 
               1 + 31 + 30, 
               1 + 31 + 30 + 31, 
               1 + 31 + 30 + 31 + 31, 
               1 + 31 + 30 + 31 + 31 + 30, 
               1 + 31 + 30 + 31 + 31 + 30 + 31), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
     labels = format(seq(0, ymax, length.out = 10), 
                     digits = 2, scientific = F), cex.axis = 1)
if(anno == 2018)
  legend('topleft', legend = leg , cex = 0.8,  lwd = 2, col = colori, bty = 'n')

if(par == 'phi') {
  pop2 = pop[1:6,]
  ymax = round(max(pop2),3)
  xmax = ncol(pop2)
  
  plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
       xlab = "", ylab = "", axes = F, main = paste("Bird susceptible", anno)) #, quale_cluster))
  for (i in 1:nrow(pop2)) {
    lines(as.numeric(pop2[i,]), col = colori[i], lwd = 2)
  }
  if(par == 'muB')
    leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
  if(par == 'phi')
    leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul')
  axis(1, at = c(1, 
                 1 + 31, 
                 1 + 31 + 30, 
                 1 + 31 + 30 + 31, 
                 1 + 31 + 30 + 31 + 31, 
                 1 + 31 + 30 + 31 + 31 + 30, 
                 1 + 31 + 30 + 31 + 31 + 30 + 31), 
       labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
       cex.axis = 1, las = 2)
  axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
       labels = format(seq(0, ymax, length.out = 10), 
                       digits = 2, scientific = F), cex.axis = 1)
  if(anno == 2018)
    legend('topleft', legend = leg , cex = 0.8,  lwd = 2, col = colori[1:6], bty = 'n')
  
}


# BE----
par(mfrow = c(1, 1), mar = c(5,4,4,3))
pop = BE_short
ymax = round(max(pop))+1
xmax = ncol(pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Bird exposed", anno)) #, quale_cluster))
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i], lwd = 2)
}
if(par == 'muB')
  leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
if(par == 'phi')
  leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul', '01 Aug', '15 Aug')
if(par == 's')
  leg = paste('s=', round(par_vec[selezione_par],1))
axis(1, at = c(1, 
               1 + 31, 
               1 + 31 + 30, 
               1 + 31 + 30 + 31, 
               1 + 31 + 30 + 31 + 31, 
               1 + 31 + 30 + 31 + 31 + 30, 
               1 + 31 + 30 + 31 + 31 + 30 + 31), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
     labels = format(seq(0, ymax, length.out = 10), 
                     digits = 2, scientific = F), cex.axis = 1)
if(anno == 2018)
  legend('topright', legend = leg , cex = 0.8,  lwd = 2, col = colori, bty = 'n')

if(par == 'phi') {
  pop2 = pop[1:6,]
  ymax = round(max(pop2),3)
  xmax = ncol(pop2)
  
  plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
       xlab = "", ylab = "", axes = F, main = paste("Bird exposed", anno)) #, quale_cluster))
  for (i in 1:nrow(pop2)) {
    lines(as.numeric(pop2[i,]), col = colori[i], lwd = 2)
  }
  if(par == 'muB')
    leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
  if(par == 'phi')
    leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul')
  axis(1, at = c(1, 
                 1 + 31, 
                 1 + 31 + 30, 
                 1 + 31 + 30 + 31, 
                 1 + 31 + 30 + 31 + 31, 
                 1 + 31 + 30 + 31 + 31 + 30, 
                 1 + 31 + 30 + 31 + 31 + 30 + 31), 
       labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
       cex.axis = 1, las = 2)
  axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
       labels = format(seq(0, ymax, length.out = 10), 
                       digits = 2, scientific = F), cex.axis = 1)
  if(anno == 2018)
    legend('topright', legend = leg , cex = 0.8,  lwd = 2, col = colori[1:6], bty = 'n')
  
}

# BI----
par(mfrow = c(1, 1), mar = c(5,4,4,3))
pop = BI_short
ymax = round(max(pop))+1
xmax = ncol(pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Bird infected", anno)) #, quale_cluster))
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i], lwd = 2)
}
if(par == 'muB')
  leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
if(par == 'phi')
  leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul', '01 Aug', '15 Aug')
if(par == 's')
  leg = paste('s=', round(par_vec[selezione_par],1))
axis(1, at = c(1, 
               1 + 31, 
               1 + 31 + 30, 
               1 + 31 + 30 + 31, 
               1 + 31 + 30 + 31 + 31, 
               1 + 31 + 30 + 31 + 31 + 30, 
               1 + 31 + 30 + 31 + 31 + 30 + 31), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
     labels = format(seq(0, ymax, length.out = 10), 
                     digits = 2, scientific = F), cex.axis = 1)
if(anno == 2018)
  legend('topright', legend = leg , cex = 0.8,  lwd = 2, col = colori, bty = 'n')

if(par == 'phi') {
  pop2 = pop[1:6,]
  ymax = round(max(pop2),3)
  xmax = ncol(pop2)
  
  plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
       xlab = "", ylab = "", axes = F, main = paste("Bird infected", anno)) #, quale_cluster))
  for (i in 1:nrow(pop2)) {
    lines(as.numeric(pop2[i,]), col = colori[i], lwd = 2)
  }
  if(par == 'muB')
    leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
  if(par == 'phi')
    leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul')
  axis(1, at = c(1, 
                 1 + 31, 
                 1 + 31 + 30, 
                 1 + 31 + 30 + 31, 
                 1 + 31 + 30 + 31 + 31, 
                 1 + 31 + 30 + 31 + 31 + 30, 
                 1 + 31 + 30 + 31 + 31 + 30 + 31), 
       labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
       cex.axis = 1, las = 2)
  axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
       labels = format(seq(0, ymax, length.out = 10), 
                       digits = 2, scientific = F), cex.axis = 1)
  if(anno == 2018)
    legend('topright', legend = leg , cex = 0.8,  lwd = 2, col = colori[1:6], bty = 'n')
  
}


# BR ----
par(mfrow = c(1, 1), mar = c(5,4,4,3))
pop = BR_short
ymax = round(max(pop))+9
xmax = ncol(pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Bird recovered", anno)) #, quale_cluster))
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i], lwd = 2)
}
if(par == 'muB')
  leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
if(par == 'phi')
  leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul', '01 Aug', '15 Aug')
if(par == 's')
  leg = paste('s=', round(par_vec[selezione_par],1))
axis(1, at = c(1, 
               1 + 31, 
               1 + 31 + 30, 
               1 + 31 + 30 + 31, 
               1 + 31 + 30 + 31 + 31, 
               1 + 31 + 30 + 31 + 31 + 30, 
               1 + 31 + 30 + 31 + 31 + 30 + 31), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
     labels = format(seq(0, ymax, length.out = 10), 
                     digits = 2, scientific = F), cex.axis = 1)
if(anno == 2018)
  legend('topleft', legend = leg , cex = 0.8,  lwd = 2, col = colori, bty = 'n')

if(par == 'phi') {
  pop2 = pop[1:6,]
  ymax = round(max(pop2),3)
  xmax = ncol(pop2)
  
  plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
       xlab = "", ylab = "", axes = F, main = paste("Bird recovered", anno)) #, quale_cluster))
  for (i in 1:nrow(pop2)) {
    lines(as.numeric(pop2[i,]), col = colori[i], lwd = 2)
  }
  if(par == 'muB')
    leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
  if(par == 'phi')
    leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul')
  axis(1, at = c(1, 
                 1 + 31, 
                 1 + 31 + 30, 
                 1 + 31 + 30 + 31, 
                 1 + 31 + 30 + 31 + 31, 
                 1 + 31 + 30 + 31 + 31 + 30, 
                 1 + 31 + 30 + 31 + 31 + 30 + 31), 
       labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
       cex.axis = 1, las = 2)
  axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
       labels = format(seq(0, ymax, length.out = 10), 
                       digits = 2, scientific = F), cex.axis = 1)
  if(anno == 2018)
    legend('topleft', legend = leg , cex = 0.8,  lwd = 2, col = colori[1:6], bty = 'n')
  
}


# bird prevalence----
par(mfrow = c(1, 1), mar = c(5,4,4,3))
pop = (BI_short/(BS_short+BE_short+BI_short+BR_short))
ymax = (max(pop))
xmax = ncol(pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Bird prevalence", anno)) #, quale_cluster))
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i], lwd = 2)
}
if(par == 'muB')
  leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
if(par == 'phi')
  leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul', '01 Aug', '15 Aug')
if(par == 's')
  leg = paste('s=', round(par_vec[selezione_par],1))
axis(1, at = c(1, 
               1 + 31, 
               1 + 31 + 30, 
               1 + 31 + 30 + 31, 
               1 + 31 + 30 + 31 + 31, 
               1 + 31 + 30 + 31 + 31 + 30, 
               1 + 31 + 30 + 31 + 31 + 30 + 31), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
     labels = format(seq(0, ymax, length.out = 10), 
                     digits = 2, scientific = F), cex.axis = 1)
if(anno == 2018)
  legend('topright', legend = leg , cex = 0.8,  lwd = 2, col = colori, bty = 'n')

if(par == 'phi') {
  pop2 = pop[1:6,]
  ymax = round(max(pop2),3)
  xmax = ncol(pop2)
  
  plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
       xlab = "", ylab = "", axes = F, main = paste("Bird prevalence", anno)) #, quale_cluster))
  for (i in 1:nrow(pop2)) {
    lines(as.numeric(pop2[i,]), col = colori[i], lwd = 2)
  }
  if(par == 'muB')
    leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
  if(par == 'phi')
    leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul')
  axis(1, at = c(1, 
                 1 + 31, 
                 1 + 31 + 30, 
                 1 + 31 + 30 + 31, 
                 1 + 31 + 30 + 31 + 31, 
                 1 + 31 + 30 + 31 + 31 + 30, 
                 1 + 31 + 30 + 31 + 31 + 30 + 31), 
       labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
       cex.axis = 1, las = 2)
  axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
       labels = format(seq(0, ymax, length.out = 10), 
                       digits = 2, scientific = F), cex.axis = 1)
  if(anno == 2018)
    legend('topleft', legend = leg , cex = 0.8,  lwd = 2, col = colori[1:6], bty = 'n')
  
}


# bird seroprevalence----
par(mfrow = c(1, 1), mar = c(5,4,4,3))
pop = (BR_short/(BS_short+BE_short+BI_short+BR_short))
ymax = (max(pop))+0.15
xmax = ncol(pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Bird seroprevalence", anno)) #, quale_cluster))
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i], lwd = 2)
}
if(par == 'muB')
  leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
if(par == 'phi')
  leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul', '01 Aug', '15 Aug')
if(par == 's')
  leg = paste('s=', round(par_vec[selezione_par],1))
axis(1, at = c(1, 
               1 + 31, 
               1 + 31 + 30, 
               1 + 31 + 30 + 31, 
               1 + 31 + 30 + 31 + 31, 
               1 + 31 + 30 + 31 + 31 + 30, 
               1 + 31 + 30 + 31 + 31 + 30 + 31), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
     labels = format(seq(0, ymax, length.out = 10), 
                     digits = 2, scientific = F), cex.axis = 1)
if(anno == 2018)
  legend('topleft', legend = leg , cex = 0.8,  lwd = 2, col = colori, bty = 'n')

if(par == 'phi') {
  pop2 = pop[1:6,]
  ymax = round(max(pop2),3)
  xmax = ncol(pop2)
  
  plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
       xlab = "", ylab = "", axes = F, main = paste("Bird seroprevalence", anno)) #, quale_cluster))
  for (i in 1:nrow(pop2)) {
    lines(as.numeric(pop2[i,]), col = colori[i], lwd = 2)
  }
  if(par == 'muB')
    leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
  if(par == 'phi')
    leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul')
  axis(1, at = c(1, 
                 1 + 31, 
                 1 + 31 + 30, 
                 1 + 31 + 30 + 31, 
                 1 + 31 + 30 + 31 + 31, 
                 1 + 31 + 30 + 31 + 31 + 30, 
                 1 + 31 + 30 + 31 + 31 + 30 + 31), 
       labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
       cex.axis = 1, las = 2)
  axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
       labels = format(seq(0, ymax, length.out = 10), 
                       digits = 2, scientific = F), cex.axis = 1)
  if(anno == 2018)
    legend('topleft', legend = leg , cex = 0.8,  lwd = 2, col = colori[1:6], bty = 'n')
  
}


# vector/host ----
par(mfrow = c(1, 1), mar = c(5,4,4,3))
pop = (MS_short + MI_short + ME_short)/(BS_short + BI_short + BE_short + BR_short)
ymax = round(max(pop))+1
xmax = ncol(pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste(par, "- V/H", anno)) #, quale_cluster))
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i], lwd = 2)
}
if(par == 'muB')
  leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
if(par == 'phi')
  leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul', '01 Aug', '15 Aug')
if(par == 's')
  leg = paste('s=', round(par_vec[selezione_par],1))
axis(1, at = c(1, 
               1 + 31, 
               1 + 31 + 30, 
               1 + 31 + 30 + 31, 
               1 + 31 + 30 + 31 + 31, 
               1 + 31 + 30 + 31 + 31 + 30, 
               1 + 31 + 30 + 31 + 31 + 30 + 31), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
     labels = format(seq(0, ymax, length.out = 10), 
                     digits = 2, scientific = F), cex.axis = 1)
if(anno == 2018)
  legend('topright', legend = leg , cex = 0.8,  lwd = 2, col = colori, bty = 'n')

par(mfrow = c(1, 1), mar = c(5,4,4,3))
pop = (MS_short + MI_short + ME_short)/(BS_short + BI_short + BE_short + BR_short)
ymax = round(max(pop[,62:95]))
xmax = ncol(pop)
plot(0, col = "white", xlim = c(62, 95), ylim = c(0, 580), 
     xlab = "", ylab = "", axes = F, main = paste(par, "- V/H", anno)) #, quale_cluster))
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i], lwd = 2)
}
if(par == 'muB')
  leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
if(par == 'phi')
  leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul', '01 Aug', '15 Aug')
if(par == 's')
  leg = paste('s=', round(par_vec[selezione_par],1))
axis(1, at = seq(62, 102, 7), 
     labels = c('01 Jun', "7 Jun", "14 Jun", "21 Jun", "28 Jun", "05 Jul"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, 580, length.out = 10), las = 2, 
     labels = format(seq(0, 580, length.out = 10), 
                     digits = 2, scientific = F), cex.axis = 1)

# MI----
par(mfrow = c(1, 1), mar = c(5,4,4,3))
pop = MI_short
ymax = round(max(pop))+1
xmax = ncol(pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito infected", anno)) #, quale_cluster))
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i], lwd = 2)
}
if(par == 'muB')
  leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
if(par == 'phi')
  leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul', '01 Aug', '15 Aug')
if(par == 's')
  leg = paste('s=', round(par_vec[selezione_par],1))
axis(1, at = c(1, 
               1 + 31, 
               1 + 31 + 30, 
               1 + 31 + 30 + 31, 
               1 + 31 + 30 + 31 + 31, 
               1 + 31 + 30 + 31 + 31 + 30, 
               1 + 31 + 30 + 31 + 31 + 30 + 31), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
     labels = format(seq(0, ymax, length.out = 10), 
                     digits = 2, scientific = F), cex.axis = 1)
if(anno == 2018)
  legend('topleft', legend = leg , cex = 0.8,  lwd = 2, col = colori, bty = 'n')

if(par == 'phi') {
  pop2 = pop[1:6,]
  ymax = round(max(pop2),3)
  xmax = ncol(pop2)
  
  plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
       xlab = "", ylab = "", axes = F, main = paste("Mosquito infected", anno)) #, quale_cluster))
  for (i in 1:nrow(pop2)) {
    lines(as.numeric(pop2[i,]), col = colori[i], lwd = 2)
  }
  if(par == 'muB')
    leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
  if(par == 'phi')
    leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul')
  axis(1, at = c(1, 
                 1 + 31, 
                 1 + 31 + 30, 
                 1 + 31 + 30 + 31, 
                 1 + 31 + 30 + 31 + 31, 
                 1 + 31 + 30 + 31 + 31 + 30, 
                 1 + 31 + 30 + 31 + 31 + 30 + 31), 
       labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
       cex.axis = 1, las = 2)
  axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
       labels = format(seq(0, ymax, length.out = 10), 
                       digits = 2, scientific = F), cex.axis = 1)
  if(anno == 2017)
    legend('topright', legend = leg , cex = 0.8,  lwd = 2, col = colori[1:6], bty = 'n')
  
}

# ME----
par(mfrow = c(1, 1), mar = c(5,4,4,3))
pop = ME_short
ymax = round(max(pop))+1
xmax = ncol(pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito exposed", anno)) #, quale_cluster))
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i], lwd = 2)
}
if(par == 'muB')
  leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
if(par == 'phi')
  leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul', '01 Aug', '15 Aug')
if(par == 's')
  leg = paste('s=', round(par_vec[selezione_par],1))
axis(1, at = c(1, 
               1 + 31, 
               1 + 31 + 30, 
               1 + 31 + 30 + 31, 
               1 + 31 + 30 + 31 + 31, 
               1 + 31 + 30 + 31 + 31 + 30, 
               1 + 31 + 30 + 31 + 31 + 30 + 31), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
     labels = format(seq(0, ymax, length.out = 10), 
                     digits = 2, scientific = F), cex.axis = 1)
if(anno == 2018)
  legend('topright', legend = leg , cex = 0.8,  lwd = 2, col = colori, bty = 'n')

if(par == 'phi') {
  pop2 = pop[1:6,]
  ymax = round(max(pop2),3)
  xmax = ncol(pop2)
  
  plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
       xlab = "", ylab = "", axes = F, main = paste("Mosquito exposed", anno)) #, quale_cluster))
  for (i in 1:nrow(pop2)) {
    lines(as.numeric(pop2[i,]), col = colori[i], lwd = 2)
  }
  if(par == 'muB')
    leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
  if(par == 'phi')
    leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul')
  axis(1, at = c(1, 
                 1 + 31, 
                 1 + 31 + 30, 
                 1 + 31 + 30 + 31, 
                 1 + 31 + 30 + 31 + 31, 
                 1 + 31 + 30 + 31 + 31 + 30, 
                 1 + 31 + 30 + 31 + 31 + 30 + 31), 
       labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
       cex.axis = 1, las = 2)
  axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
       labels = format(seq(0, ymax, length.out = 10), 
                       digits = 2, scientific = F), cex.axis = 1)
  if(anno == 2018)
    legend('topright', legend = leg , cex = 0.8,  lwd = 2, col = colori[1:6], bty = 'n')
  
}


# mosquito prevalence----
par(mfrow = c(1, 1), mar = c(5,4,4,3))
pop = (MI_short/(MS_short+ME_short+MI_short))
ymax = (max(pop))+0.0002
xmax = ncol(pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito prevalence", anno)) #, quale_cluster))
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i], lwd = 2)
}
if(par == 'muB')
  leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
if(par == 'phi')
  leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul', '01 Aug', '15 Aug')
if(par == 's')
  leg = paste('s=', round(par_vec[selezione_par],1))
axis(1, at = c(1, 
               1 + 31, 
               1 + 31 + 30, 
               1 + 31 + 30 + 31, 
               1 + 31 + 30 + 31 + 31, 
               1 + 31 + 30 + 31 + 31 + 30, 
               1 + 31 + 30 + 31 + 31 + 30 + 31), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
     labels = format(seq(0, ymax, length.out = 10), 
                     digits = 2, scientific = F), cex.axis = 1)

if(anno == 2018)
  legend("topleft", legend = leg , cex = 0.8,  lwd = 2, col = colori, bty = 'n')

if(par == 'phi') {
  pop2 = pop[1:6,]
  ymax = round(max(pop2),3)
  xmax = ncol(pop2)
  
  plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 0.0013), 
       xlab = "", ylab = "", axes = F, main = paste("Mosquito prevalence", anno)) #, quale_cluster))
  for (i in 1:nrow(pop2)) {
    lines(as.numeric(pop2[i,]), col = colori[i], lwd = 2)
  }
  if(par == 'muB')
    leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
  if(par == 'phi')
    leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul')
  axis(1, at = c(1, 
                 1 + 31, 
                 1 + 31 + 30, 
                 1 + 31 + 30 + 31, 
                 1 + 31 + 30 + 31 + 31, 
                 1 + 31 + 30 + 31 + 31 + 30, 
                 1 + 31 + 30 + 31 + 31 + 30 + 31), 
       labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
       cex.axis = 1, las = 2)
  axis(2, at = seq(0, 0.0013, length.out = 10), las = 2, 
       labels = format(seq(0, 0.0013, length.out = 10), 
                       digits = 2, scientific = F), cex.axis = 1)
  if(anno == 2018)
    legend(5,0.0013, legend = leg , cex = 0.8,  lwd = 2, col = colori[1:6], bty = 'n')
  
}


# plot 1 check del file che sia quello che intendo ----
par(mfrow = c(1, 1), mar = c(5,4,4,3))
pop = MI_short
ymax = round(max(pop))+1
xmax = ncol(pop)
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito infected", anno)) #, quale_cluster))
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i], lwd = 2)
}
if(par == 'muB')
  leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
if(par == 'phi')
  leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul', '01 Aug', '15 Aug')
if(par == 's')
  leg = paste('s=', round(par_vec[selezione_par],1))
axis(1, at = c(1, 
               1 + 31, 
               1 + 31 + 30, 
               1 + 31 + 30 + 31, 
               1 + 31 + 30 + 31 + 31, 
               1 + 31 + 30 + 31 + 31 + 30, 
               1 + 31 + 30 + 31 + 31 + 30 + 31), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
     labels = format(seq(0, ymax, length.out = 10), 
                     digits = 2, scientific = F), cex.axis = 1)
if(anno == 2017)
  legend('topright', legend = leg , cex = 0.8,  lwd = 2, col = colori, bty = 'n')

if(par == 'phi') {
  pop2 = pop[1:6,]
  ymax = round(max(pop2),3)
  xmax = ncol(pop2)
  
  plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
       xlab = "", ylab = "", axes = F, main = paste("Mosquito infected", anno)) #, quale_cluster))
  for (i in 1:nrow(pop2)) {
    lines(as.numeric(pop2[i,]), col = colori[i], lwd = 2)
  }
  if(par == 'muB')
    leg = paste(round(par_vec[selezione_par]*365,1), 'eggs/year')
  if(par == 'phi')
    leg = c('01 May', '15 May', '01 Jun', '15 Jun', '01 Jul', '15 Jul')
  axis(1, at = c(1, 
                 1 + 31, 
                 1 + 31 + 30, 
                 1 + 31 + 30 + 31, 
                 1 + 31 + 30 + 31 + 31, 
                 1 + 31 + 30 + 31 + 31 + 30, 
                 1 + 31 + 30 + 31 + 31 + 30 + 31), 
       labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
       cex.axis = 1, las = 2)
  axis(2, at = seq(0, ymax, length.out = 10), las = 2, 
       labels = format(seq(0, ymax, length.out = 10), 
                       digits = 2, scientific = F), cex.axis = 1)
  if(anno == 2017)
    legend('topright', legend = leg , cex = 0.8,  lwd = 2, col = colori[1:6], bty = 'n')
  
}

par(mfrow = c(2, 1), mar = c(5,4,4,3))
ymax = ifelse(anno == 2016, 26, 16)
xmax = ncol(pool_pos_simu)+0.5
plot(0, col = "white", xlim = c(1, xmax), ylim = c(0,ymax), xlab = "", ylab = "", axes = F, 
     main = paste('Expected PosPools', anno), cex.main = 1.2)
scarti = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
for(j in 1:length(pool_pos_list)) {
  pool_pos_simu = pool_pos_list[[j]]
  scarto = scarti[j]
  for (i in 1:(ncol(pool_pos_simu))) 
    plotbox(w = (i+scarto), 
            X = pool_pos_simu[,i], fun = median, 
            wd = 0.05, col = colori[j])
  
}
points((1:xmax)+0.5, 
       numero_pool_positivi[1, quando_ho_pool][1:xmax], 
       pch = 15, col = "darkorange", cex = 1)
date = format(as.Date(as.numeric(settimane[quando_ho_pool[1:xmax]]), 
                      origin = paste("01-01-", anno, sep = ""), 
                      format = "%d-%m-%Y"), "%d-%m")
axis(1, at = c(1:xmax), labels = date, las = 2)
axis(2, las = 2, at = seq(0, ymax, 2))
mtext("WNV positive pools", side = 2, line = 2.2, 
      adj = 0.5, cex = 1)

# giorno per giorno ----
par(mfrow = c(3,2), mar = c(4,3.5,3.2,2)) #, cex.main = cex_main, 
    
for (i in 1:length(date)) {
  ymax = ifelse(anno == 2018, 26, 16)
  xmax = length(pool_pos_list)
  plot(0, col = "white", xlim = c(0, xmax), ylim = c(0,ymax), xlab = "", ylab = "", axes = F, 
       main = paste('expected pos pools', date[i]), cex.main  = 1.2)
  axis(1, at = c(1:length(pool_pos_list)), 
       labels = paste0(par,seq(1:length(pool_pos_list))), 
       las = 2, cex = 0.15)
  axis(2, las = 2, at = seq(0, ymax, 2), cex = 0.3)
  mtext("WNV positive pools", side = 2, line = 2, 
        adj = 0.5, cex = 0.8)
  for(j in 1:length(pool_pos_list)) {
    pool_pos_simu = pool_pos_list[[j]]
    
    plotbox(w = j,  
            X = pool_pos_simu[,i], fun = median, 
            wd = 0.2, col = colori[j])
    abline(h = numero_pool_positivi[1,quando_ho_pool[i]][1:xmax], col = "darkorange", lwd = 2)
        
  }
}

# birth pulse ----
birth_pulse = apply(parametri_sens[seq(1,100, 10),], MARGIN = 1, function(a) {
  with(as.list(a), muB * exp(-s * sin(pi * (seq(0,365)/365 - phi))^2)/besselI(s/2, 0, T))
})
ymax = max(birth_pulse)

plot(0, col = "white", xlim = c(0, 365), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("birth pulse")) #, quale_cluster))
colori = rainbow(length(parametri_sens[seq(1,100, 10),]))
for (i in 1:ncol(birth_pulse)) {
  lines(as.numeric(birth_pulse[,i]), col = colori[i])
}
axis(1, at = c(1+31, 1+31+28, 1+31+27+31, 1+31+28+31+30, 1+31+28+31+30+31,
               1+31+28+31+30+31+30, 1+31+28+31+30+31+30+30, 1+31+28+31+30+31+30+30+31,
               1+31+28+31+30+31+30+30+31+30, 1+31+28+31+30+31+30+30+31+30+31,
               1+31+28+31+30+31+30+30+31+30+31+30, 1+31+28+31+30+31+30+30+31+30+31+30+31),
     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept",
                "Oct", "Nov", "Dec"), 
     cex.axis = 1, las = 2)

axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = F), cex.axis = 1)



