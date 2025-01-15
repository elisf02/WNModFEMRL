##################################### 02_Epidemiological_1sp ########################################
#### model fit plot ####
settimane = scan(vettore_date_catture)
colori = rainbow(n = numero_cluster)
quanti_dentro95 = 0
quante_osservazioni = 0
for (anno in anno_inizio:anno_fine) {
  print(anno)
  nome_file_plot = paste0(FolderPlotOut, PlotName, anno, 
                          ".pdf")
  #pdf(nome_file_plot, height = 1 * 1000, width = 1 * 1400)
  par(mfrow = c(1, 1), mar = margins_plot, cex.main = cex_main, 
      cex.axis = cex_axes)
  message(paste("plot ModelFit", anno))
  file_numero_pool = paste0(file_numero_pool_name, anno)
  file_numero_pool_positivi = paste0(file_numero_pool_positivi_name, 
                                     anno)
  file_pool_size = paste0(file_pool_size_name, anno)
  numero_pool = read.table(file_numero_pool)
  numero_pool_positivi = read.table(file_numero_pool_positivi)
  pool_size = read.table(file_pool_size)
  numero_classi = 7
  for (quale_cluster in 1:numero_cluster) {
    quando_ho_pool = which(pool_size[quale_cluster, ] > 
                             0)
    pool_positivi_simulati = c()
    con_cosa_inizio = 1
    nome_file_output_dynamics = paste0(FolderSimu, FileDynName, 
                                       "M_", anno, "_", quale_cluster, ".txt")
    output_dynamics = read.table(nome_file_output_dynamics)
    mean_MS_allcluster = c()
    mean_ME_allcluster = c()
    mean_MI_allcluster = c()
    sel_MS = seq(1, nrow(output_dynamics), numero_classi)
    sel_ME = seq(2, nrow(output_dynamics), numero_classi)
    sel_MI = seq(3, nrow(output_dynamics), numero_classi)
    MS = output_dynamics[sel_MS, ]
    ME = output_dynamics[sel_ME, ]
    MI = output_dynamics[sel_MI, ]
    mosquito_prevalence = MI/(MS + ME + MI)
    qmax_mosquito_prevalence = c()
    qmin_mosquito_prevalence = c()
    mean_mosquito_prevalence = c()
    for (j in 1:(ncol(mosquito_prevalence))) {
      qmax_mosquito_prevalence = c(qmax_mosquito_prevalence, 
                                   quantile(mosquito_prevalence[, j], probs = 0.975, 
                                            na.rm = T))
      qmin_mosquito_prevalence = c(qmin_mosquito_prevalence, 
                                   quantile(mosquito_prevalence[, j], probs = 0.025, 
                                            na.rm = T))
      mean_mosquito_prevalence = c(mean_mosquito_prevalence, 
                                   mean(mosquito_prevalence[, j], na.rm = T))
    }
    qmax_mosquito_prevalence[which(is.na(qmax_mosquito_prevalence))] = 0
    qmin_mosquito_prevalence[which(is.na(qmin_mosquito_prevalence))] = 0
    mean_mosquito_prevalence[which(is.na(mean_mosquito_prevalence))] = 0
    for (index_giorno_pool in quando_ho_pool) {
      giorno_pool = settimane[index_giorno_pool] - 
        120
      if (giorno_pool < tmax) {
        prob_pool_positivo = 1 - (1 - mosquito_prevalence[, 
                                                          giorno_pool])^as.numeric(pool_size[quale_cluster, 
                                                                                             index_giorno_pool])
        pool_analizzati = numero_pool[quale_cluster, 
                                      index_giorno_pool]
        pool_positivi = numero_pool_positivi[quale_cluster, 
                                             index_giorno_pool]
        tmp = c()
        tmp_sel = sample(1:length(prob_pool_positivo), 
                         size = 100)
        for (quale in tmp_sel) tmp = c(tmp, rbinom(n = 1, 
                                                   size = pool_analizzati, prob = prob_pool_positivo[quale]))
        pool_positivi_simulati = cbind(pool_positivi_simulati, 
                                       tmp)
      }
    }
    ymax = ymax_plot
    xmax = ncol(pool_positivi_simulati)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = anno)
    for (i in 1:(ncol(pool_positivi_simulati))) plotbox(w = i, 
                                                        X = pool_positivi_simulati[, i], fun = median, 
                                                        wd = 0.2, col = "blue")#colori[quale_cluster])
    points(1:xmax, numero_pool_positivi[quale_cluster, 
                                        quando_ho_pool][1:xmax], pch = 19, col = "orange", 
           cex = 2)
    date = format(as.Date(as.numeric(settimane[quando_ho_pool[1:xmax]]), 
                          origin = paste("01-01-", anno, sep = ""), format = "%d-%m-%Y"), 
                  "%d-%m")
    axis(1, at = c(1:xmax), labels = date, las = 2)
    axis(2, las = 2, at = seq(0, ymax, 5))
    mtext("WNV positive pools", side = 2, line = 3, adj = 0.5, cex = 1.5)
    legend( x = "topright",
            legend = c("Observations","Predictions"),
            col = c("orange","blue"), lwd = 2, lty = c(0,0),
            pch = c(19,15),
            bty = "n",
            cex = 1.1,
            y.intersp = 0.7)
    quante_osservazioni = quante_osservazioni + length(quando_ho_pool)
    for (i in 1:(ncol(pool_positivi_simulati))) {
      qmin = quantile(pool_positivi_simulati[, i], 
                      probs = 0.025)
      qmax = quantile(pool_positivi_simulati[, i], 
                      probs = 0.975)
      if (numero_pool_positivi[quale_cluster, quando_ho_pool][i] >= 
          qmin & numero_pool_positivi[quale_cluster, 
                                      quando_ho_pool][i] <= qmax) 
        quanti_dentro95 = quanti_dentro95 + 1
    }
  }
  #dev.off()
}
#### mosquito/bird plot ####
anno_inizio = 2016
anno_fine = 2018
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Simulazioni/"
FileDynName = "dynamics_"
FolderPlotOut = "Output_WNV/Simulazioni/Plots/"
PlotName = 'Dynamics_BirdPop_' #cambia a seconda del plot che stai facendo 
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

cex_main = 2
cex_axes = 1.2
cex_lab_axes = 1.2
margins_plot = c(7, 7, 4, 2) 
what_plot = "mosquito_bird"
colore = "darkblue"
settimane = scan(vettore_date_catture)

for (anno in anno_inizio:anno_fine) {
  print(anno)
  nome_file_plot = paste0(FolderPlotOut, PlotName, anno, 
                          ".pdf")
  #jpeg(nome_file_plot, height = 1 * 1000, width = 1 * 1400, res = 200)
  par(mfrow = c(1, 1), mar = margins_plot, cex.main = cex_main, 
      cex.axis = cex_axes)
  numero_classi = 7
  for (quale_cluster in 1:numero_cluster) {
    if (con_cosa_inizio == 0) 
      nome_file_output_dynamics = paste0(FolderSimu, 
                                         FileDynName, "B_", anno, "_", quale_cluster, 
                                         ".txt")
    if (con_cosa_inizio == 1) 
      nome_file_output_dynamics = paste0(FolderSimu, 
                                         FileDynName, "M_", anno, "_", quale_cluster, 
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
    mosquito_bird = mosquito_population/bird_population
    if (what_plot == "mosquito_prevalence") 
      pop = mosquito_prevalence
    if (what_plot == "moquito_population") 
      pop = mosquito_population
    if (what_plot == "bird_prevalence") 
      pop = bird_prevalence
    if (what_plot == "bird_population") 
      pop = bird_population
    if (what_plot == "mosquito_bird") 
      pop = mosquito_bird
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
    ymax = max(qmax_pop)
    xmax = length(qmax_pop)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       600), xlab = "", ylab = "", axes = F, main = anno)
    lines(mean_pop, lwd = 3, col = colore)
   # poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
                    # y = c(qmin_pop, rev(qmax_pop)))
    #polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
           # border = NA)
   
    axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 
                     30, 1 + 30 + 31 + 30 + 31, 1 + 30 + 31 + 30 + 
                     31 + 31, 1 + 30 + 31 + 30 + 
                     31 + 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                          "Aug", "Sept", "Oct"), cex.axis = 1.3, las = 2)
    #axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
         #labels = format(seq(0, ymax, length.out = 4), 
                       #  digits = 2, scientific = F), cex.axis = 1.3)
    axis(2, at = seq(0, 600, 100), las = 2, 
         labels = format(seq(0, 600, 100), 
                         digits = 2, scientific = F), cex.axis = 1.3)
    if (what_plot == "mosquito_prevalence") 
      text = "Prev in mosquitoes"
    if (what_plot == "moquito_population") 
      text = "Mosquito pop"
    if (what_plot == "bird_prevalence") 
      text = "Prev in birds"
    if (what_plot == "bird_population") 
      text = "Bird pop"
    if (what_plot == "mosquito_bird") 
      text = "mosquitoes to birds rate"
    mtext(text, side = 2, line = 5, adj = 0.5, cex = 1.7)
  }
#dev.off()
}

#### birth pulse plot ####
parms = "Output_WNV/MCMC/parametri_M_2018_1.txt"
main = 2018
tmin = 91
tmax = 274
quante_specie = 1

cex_main = 2
cex_axes = 1.2
cex_lab_axes = 1.2
margins_plot = c(5, 5, 2, 2)
par(mfrow = c(1, 1), mar = margins_plot, cex.main = cex_main, 
    cex.axis = cex_axes)

if (is.numeric(parms)) {
  with(as.list(parms), plot(muB * exp(-s * sin(pi * (seq(0, 
                                                         365)/365 - phi[1]))^2)/besselI(s/2, 0, T), type = "l", 
                            lwd = 3, col = "black"), xlab = "time", ylab = "BIRTH_PULSE")
  abline(v = c(tmin, tmax), lwd = 2, col = "red")
}
if (is.character(parms)) {
  if (quante_specie == 1) 
    nomi_parametri = c("p", "B0", "pR", "b1", "muB", 
                       "s", "phi", "niB", "recB")
  if (quante_specie == 2) 
    nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", 
                       "pR_ca", "pR_sn", "b1_ca", "muB_ca", "s_ca", 
                       "phi_ca", "niB_ca", "recB_ca")
  parametri_stimati = vector("list", length(nomi_parametri))
  output_mcmc = read.table(parms)
  colnames(output_mcmc) = c(nomi_parametri, "lik")
  burnin = nrow(output_mcmc) * 0.1
  output_mcmc = output_mcmc[-c(1:burnin), ]
  if (quante_specie == 1) {
    birth_pulse = apply(output_mcmc, MARGIN = 1, function(a) {
      with(as.list(a), muB * exp(-s * sin(pi * (seq(0, 
                                                    365)/365 - phi))^2)/besselI(s/2, 0, T))
    })
  }
  if (quante_specie == 2) {
    birth_pulse = apply(output_mcmc, MARGIN = 1, function(a) {
      with(as.list(a), muB_ca * exp(-s_ca * sin(pi * 
                                                  (seq(0, 365)/365 - phi_ca))^2)/besselI(s_ca/2, 
                                                                                         0, T))
    })
  }
  mean = apply(birth_pulse, MARGIN = 1, function(a) {
    mean(a, na.rm = T)
  })
  qmin = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.025, na.rm = T)
  })
  qmax = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.975, na.rm = T)
  })
  ymax = max(qmax, qmin, mean)
  plot(0, col = "white", xlim = c(0, 365), ylim = c(0, 
                                                    0.15), xlab = "", ylab = "", axes = F, main = main, bty="l")

  lines(mean_pop, lwd = 3, col = colore)
  abline(v = c(tmin, tmax), lwd = 2, col = "red")
  lines(mean, lwd = 3, col = "black")
  poligono = cbind(x = c(1:length(mean), length(mean):1), 
                   y = c(qmin, rev(qmax)))
  polygon(poligono, col = adjustcolor("grey", alpha = 0.2), 
          border = NA)
  axis(1, at = c(1, 1 + 31, 1 + 31 + 28, 1 + 31 + 28 + 
                   31, 1 + 31 + 28 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 , 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + 31), labels = c("Gen", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                                             "Aug", "Sept", "Oct", "Nov", "Dec", "Gen"), cex.axis = 1.3, las = 2)

  axis(2, at = seq(0, 0.15, 0.05), las = 2, 
       labels = format(seq(0, 0.15, 0.05), 
                       digits = 2, scientific = F), cex.axis = 1.3)
  mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)  
}
#dev.off()
#### boxplot plot NON SONO RIUSCITA #### 
anno_inizio = 2016
anno_fine = 2018
numero_cluster = 1
con_cosa_inizio = 1

OutLoc = "Output_WNV/MCMC"
FileAllParmsName = "parametri_"
FileParmsPerSimuName = "per_simulazione_"

nomi_parametri = c("p", "B0", "pR", "b1", "muB", "s", "phi", 
                   "niB", "recB")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
  for (quale_cluster in 1:numero_cluster) {
    if (con_cosa_inizio == 0) 
      nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                   "B_", anno, "_", quale_cluster, ".txt")
    if (con_cosa_inizio == 1) 
      nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                   "M_", anno, "_", quale_cluster, ".txt")
    output_mcmc = read.table(nome_file_parametri)
    burnin = nrow(output_mcmc) * 0.1
    if (ncol(output_mcmc) <= 1) 
      output_mcmc = matrix(0, ncol = length(nomi_parametri) + 
                             1, nrow = max_iter_MCMC)
    for (j in 1:(ncol(output_mcmc) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                        output_mcmc[-c(1:burnin), j])
  }
  if (con_cosa_inizio == 0) 
    nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                       "B_", anno, "_", quale_cluster, ".pdf")
  if (con_cosa_inizio == 1) 
    nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                       "M_", anno, "_", quale_cluster, ".pdf")
  #jpeg(nome_file, width = length(nomi_parametri) * 1200, 
      # height = 1000, res = 200)
  par(mfrow = c(1, length(nomi_parametri)), cex.axis = 1.5, 
      #mar = c(7, 7, 4, 2), cex.lab = 1.5, cex.main = 1.5)
      mar = c(4, 6, 2, 2), cex.lab = 1.5, cex.main = 1.5)
  for (i in 1:length(parametri_stimati)) {
    tab = parametri_stimati[[i]]
    ymax = 0
    for (j in 1:ncol(tab)) if (quantile(tab[, j], probs = 0.975) > 
                               ymax) 
      ymax = quantile(tab[, j], probs = 0.975)
    ymin = ymax
    for (j in 1:ncol(tab)) if (quantile(tab[, j], probs = 0.025) < 
                               ymin) 
      ymin = quantile(tab[, j], probs = 0.025)
    plot(0, col = "white", xlim = c(1, ncol(tab)), ylim = c(ymin, 
                                                            ymax), main = nomi_parametri[i], axes = F, xlab = "", 
         ylab = "")
    for (j in 1:ncol(tab)) plotbox(w = j, X = tab[, j], 
                                   fun = median)
    axis(2, at = seq(ymin, ymax, length.out = 4), las = 2, 
         labels = format(seq(ymin, ymax, length.out = 4), 
                         scientific = T, digits = 2))
    axis(1, at = c(1:ncol(tab)))
  }
  #dev.off()
}


##################################### 04_Sensitivity ########################################
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

mosquito_bird = mosquito_population/bird_population

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
  lines(as.numeric(pop[i,]), col = adjustcolor(colori[i], alpha = 0.25))
}
for (i in 1:nrow(pop)) {
    lines(as.numeric(pop[i,]), col = colori[i])
}
min = 0/365
max = 8/365
var_par =  seq(min, max, length.out = 100)

legend('topleft', legend = round(var_par, 4)[seq(1,100, 25)]*365 , cex = 0.8, 
       lwd = 4, col = colori[seq(1,100, 25)])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct"), 
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
ymax = 60 #max(pop)

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
                     digits = 2, scientific = F), cex.axis = 1)

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

#mosquito bird
pop = mosquito_bird # questo se lo cambi cambi la pop che vai a plottare

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
     xlab = "", ylab = "", axes = F, main = paste("Mosq to Bird rate", anno)) #, quale_cluster))
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
# mosquito to bird rate ----
pop = mosquito_bird # questo se lo cambi cambi la pop che vai a plottare

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
     xlab = "", ylab = "", axes = F, main = paste("Mosq to bird", anno)) #, quale_cluster))
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
ymax = 750#max(pop)

plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosqito to Bird rate", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i])
}
min = 0/365
max = 8/365
var_par =  seq(min, max, length.out = 100)

legend('topright', legend = round(var_par,4)[seq(1,100, 9)]*365 , cex = 0.8, 
       lwd = 4, col = colori[seq(1,100, 9)])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = F), cex.axis = 1)

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

mosquito_bird = mosquito_population/bird_population

# plot ----
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
ymax = 65
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
                     digits = 2, scientific = F), cex.axis = 1)

# mosquito to bird rate ----
pop = mosquito_bird # questo se lo cambi cambi la pop che vai a plottare

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
     xlab = "", ylab = "", axes = F, main = paste("Mosq to bird", anno)) #, quale_cluster))
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
ymax = 570#max(pop)

plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosqito to Bird rate", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i])
}
min = 0/365
max = 8/365
var_par =  seq(min, max, length.out = 100)

legend('topright', legend = round(var_par,4)[seq(1,100, 9)]*365 , cex = 0.8, 
       lwd = 4, col = colori[seq(1,100, 9)])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = F), cex.axis = 1)


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

mosquito_bird = mosquito_population/bird_population

# plot ----
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
ymax = 120 #max(pop)
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
                     digits = 2, scientific = F), cex.axis = 1)

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


# mosquito to bird rate ----
pop = mosquito_bird # questo se lo cambi cambi la pop che vai a plottare

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
     xlab = "", ylab = "", axes = F, main = paste("Mosq to bird", anno)) #, quale_cluster))
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
ymax = 620#max(pop)

plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosqito to Bird rate", anno)) #, quale_cluster))
colori = rainbow(100)
for (i in 1:nrow(pop)) {
  lines(as.numeric(pop[i,]), col = colori[i])
}
min = 0/365
max = 8/365
var_par =  seq(min, max, length.out = 100)

legend('topright', legend = round(var_par,4)[seq(1,100, 9)]*365 , cex = 0.8, 
       lwd = 4, col = colori[seq(1,100, 9)])
#apply(pop, MARGIN = 1, lines)
axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                 31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                 31 + 30), 
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
     cex.axis = 1, las = 2)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = F), cex.axis = 1)



####prova plot BS####
changing_par = 'muB'
anno = 2016
quale_cluster = 1
numero_classi = 7
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

susceptible_birds = BS
infected_birds = BI
exposed_birds = BE
recovered_birds = BR

# plot ----
# bird susceptible----
pop = susceptible_birds #susceptible_birds #bird_population # questo se lo cambi cambi la pop che vai a plottare

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

##################################### 05_Epidemiological_2sp ########################################
#### model fit plot MERLI ####
anno_inizio = 2016
anno_fine = 2018
numero_cluster = 1 
con_cosa_inizio = 1
max_iter_MCMC = 10000
tmax = 180

quale_specie = "Blackbirds"

FolderSimu = "Output_WNV/Merli/Simulazioni/"
FileDynName = "dynamics_2spec_Merli_" #CAMBIA NOME A SECONDA DELLA SPECIE
FolderPlotOut = "Output_WNV/Merli/Simulazioni/Plots/"
PlotName = "ModelFit_2spec_Merli_" #CAMBIA NOME A SECONDA DELLA SPECIE
file_numero_pool_name = "per_mcmc/totale_pool_cluster_"
file_numero_pool_positivi_name = "per_mcmc/totale_pool_WNVpos_cluster_" 
file_pool_size_name = "per_mcmc/mean_pool_size_cluster_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

cex_main = 2
cex_axes = 1.5
cex_lab_axes = 1.5
margins_plot = c(5, 5, 2, 2)
ymax_plot = 17

settimane = scan(vettore_date_catture)
colori = rainbow(n = numero_cluster)
quanti_dentro95 = 0
quante_osservazioni = 0

for (anno in anno_inizio:anno_fine) {
  print(anno)
  nome_file_plot = paste0(FolderPlotOut, PlotName, anno, 
                          ".pdf")
  #jpeg(nome_file_plot, height = 1 * 1000, width = 1 * 1400, 
   #    res = 200)
  par(mfrow = c(1, 1), mar = margins_plot, cex.main = cex_main, 
      cex.axis = cex_axes)
  message(paste("plot ModelFit", anno))
  file_numero_pool = paste0(file_numero_pool_name, anno)
  file_numero_pool_positivi = paste0(file_numero_pool_positivi_name, 
                                     anno)
  file_pool_size = paste0(file_pool_size_name, anno)
  numero_pool = read.table(file_numero_pool)
  numero_pool_positivi = read.table(file_numero_pool_positivi)
  pool_size = read.table(file_pool_size)
  numero_classi = 11
  for (quale_cluster in 1:numero_cluster) {
    quando_ho_pool = which(pool_size[quale_cluster, ] > 
                             0)
    pool_positivi_simulati = c()
    con_cosa_inizio = 1
    nome_file_output_dynamics = paste0(FolderSimu, FileDynName, 
                                       "M_", anno, "_", quale_cluster, ".txt")
    output_dynamics = read.table(nome_file_output_dynamics)
    mean_MS_allcluster = c()
    mean_ME_allcluster = c()
    mean_MI_allcluster = c()
    sel_MS = seq(1, nrow(output_dynamics), numero_classi)
    sel_ME = seq(2, nrow(output_dynamics), numero_classi)
    sel_MI = seq(3, nrow(output_dynamics), numero_classi)
    MS = output_dynamics[sel_MS, ]
    ME = output_dynamics[sel_ME, ]
    MI = output_dynamics[sel_MI, ]
    mosquito_prevalence = MI/(MS + ME + MI)
    qmax_mosquito_prevalence = c()
    qmin_mosquito_prevalence = c()
    mean_mosquito_prevalence = c()
    for (j in 1:(ncol(mosquito_prevalence))) {
      qmax_mosquito_prevalence = c(qmax_mosquito_prevalence, 
                                   quantile(mosquito_prevalence[, j], probs = 0.975, 
                                            na.rm = T))
      qmin_mosquito_prevalence = c(qmin_mosquito_prevalence, 
                                   quantile(mosquito_prevalence[, j], probs = 0.025, 
                                            na.rm = T))
      mean_mosquito_prevalence = c(mean_mosquito_prevalence, 
                                   mean(mosquito_prevalence[, j], na.rm = T))
    }
    qmax_mosquito_prevalence[which(is.na(qmax_mosquito_prevalence))] = 0
    qmin_mosquito_prevalence[which(is.na(qmin_mosquito_prevalence))] = 0
    mean_mosquito_prevalence[which(is.na(mean_mosquito_prevalence))] = 0
    for (index_giorno_pool in quando_ho_pool) {
      giorno_pool = settimane[index_giorno_pool] - 
        120
      if (giorno_pool < tmax) {
        prob_pool_positivo = 1 - (1 - mosquito_prevalence[, 
                                                          giorno_pool])^as.numeric(pool_size[quale_cluster, 
                                                                                             index_giorno_pool])
        pool_analizzati = numero_pool[quale_cluster, 
                                      index_giorno_pool]
        pool_positivi = numero_pool_positivi[quale_cluster, 
                                             index_giorno_pool]
        tmp = c()
        tmp_sel = sample(1:length(prob_pool_positivo), 
                         size = 100)
        for (quale in tmp_sel) tmp = c(tmp, rbinom(n = 1, 
                                                   size = pool_analizzati, prob = prob_pool_positivo[quale]))
        pool_positivi_simulati = cbind(pool_positivi_simulati, 
                                       tmp)
      }
    }
    ymax = ymax_plot
    xmax = ncol(pool_positivi_simulati)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = paste(quale_specie, anno))
    for (i in 1:(ncol(pool_positivi_simulati))) plotbox(w = i, 
                                                        X = pool_positivi_simulati[, i], fun = median, 
                                                        wd = 0.2, col = "blue") #colori[quale_cluster])
    points(1:xmax, numero_pool_positivi[quale_cluster, 
                                        quando_ho_pool][1:xmax], pch = 19, col = "darkorange", 
           cex = 2)
    date = format(as.Date(as.numeric(settimane[quando_ho_pool[1:xmax]]), 
                          origin = paste("01-01-", anno, sep = ""), format = "%d-%m-%Y"), 
                  "%d-%m")
    axis(1, at = c(1:xmax), labels = date, las = 2)
    axis(2, las = 2, at = seq(0, ymax, 5))
    mtext("WNV positive pools", side = 2, line = 3, 
          adj = 0.5, cex = 1.5)
    legend( x = "topright",
            legend = c("Observations","Predictions"),
            col = c("orange","blue"), lwd = 2, lty = c(0,0),
            pch = c(19,15),
            bty = "n",
            cex = 1.5,
            y.intersp = 0.7)
    quante_osservazioni = quante_osservazioni + length(quando_ho_pool)
    for (i in 1:(ncol(pool_positivi_simulati))) {
      qmin = quantile(pool_positivi_simulati[, i], 
                      probs = 0.025)
      qmax = quantile(pool_positivi_simulati[, i], 
                      probs = 0.975)
      if (numero_pool_positivi[quale_cluster, quando_ho_pool][i] >= 
          qmin & numero_pool_positivi[quale_cluster, 
                                      quando_ho_pool][i] <= qmax) 
        quanti_dentro95 = quanti_dentro95 + 1
    }
  }
  #dev.off()
}
#### model fit plot GAZZE ####
anno_inizio = 2016
anno_fine = 2018
numero_cluster = 1 
con_cosa_inizio = 1
max_iter_MCMC = 10000
tmax = 180

quale_specie = "Magpies"

FolderSimu = "Output_WNV/Gazze/Simulazioni/"
FileDynName = "dynamics_2spec_Gazze_" #CAMBIA NOME A SECONDA DELLA SPECIE
FolderPlotOut = "Output_WNV/Gazze/Simulazioni/Plots/"
PlotName = "ModelFit_2spec_Gazze_" #CAMBIA NOME A SECONDA DELLA SPECIE
file_numero_pool_name = "per_mcmc/totale_pool_cluster_"
file_numero_pool_positivi_name = "per_mcmc/totale_pool_WNVpos_cluster_" 
file_pool_size_name = "per_mcmc/mean_pool_size_cluster_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

cex_main = 2
cex_axes = 1.5
cex_lab_axes = 1.5
margins_plot = c(5, 5, 2, 2)
ymax_plot = 17

settimane = scan(vettore_date_catture)
colori = rainbow(n = numero_cluster)
quanti_dentro95 = 0
quante_osservazioni = 0

for (anno in anno_inizio:anno_fine) {
  print(anno)
  nome_file_plot = paste0(FolderPlotOut, PlotName, anno, 
                          ".pdf")
  #jpeg(nome_file_plot, height = 1 * 1000, width = 1 * 1400, 
  #    res = 200)
  par(mfrow = c(1, 1), mar = margins_plot, cex.main = cex_main, 
      cex.axis = cex_axes)
  message(paste("plot ModelFit", anno))
  file_numero_pool = paste0(file_numero_pool_name, anno)
  file_numero_pool_positivi = paste0(file_numero_pool_positivi_name, 
                                     anno)
  file_pool_size = paste0(file_pool_size_name, anno)
  numero_pool = read.table(file_numero_pool)
  numero_pool_positivi = read.table(file_numero_pool_positivi)
  pool_size = read.table(file_pool_size)
  numero_classi = 11
  for (quale_cluster in 1:numero_cluster) {
    quando_ho_pool = which(pool_size[quale_cluster, ] > 
                             0)
    pool_positivi_simulati = c()
    con_cosa_inizio = 1
    nome_file_output_dynamics = paste0(FolderSimu, FileDynName, 
                                       "M_", anno, "_", quale_cluster, ".txt")
    output_dynamics = read.table(nome_file_output_dynamics)
    mean_MS_allcluster = c()
    mean_ME_allcluster = c()
    mean_MI_allcluster = c()
    sel_MS = seq(1, nrow(output_dynamics), numero_classi)
    sel_ME = seq(2, nrow(output_dynamics), numero_classi)
    sel_MI = seq(3, nrow(output_dynamics), numero_classi)
    MS = output_dynamics[sel_MS, ]
    ME = output_dynamics[sel_ME, ]
    MI = output_dynamics[sel_MI, ]
    mosquito_prevalence = MI/(MS + ME + MI)
    qmax_mosquito_prevalence = c()
    qmin_mosquito_prevalence = c()
    mean_mosquito_prevalence = c()
    for (j in 1:(ncol(mosquito_prevalence))) {
      qmax_mosquito_prevalence = c(qmax_mosquito_prevalence, 
                                   quantile(mosquito_prevalence[, j], probs = 0.975, 
                                            na.rm = T))
      qmin_mosquito_prevalence = c(qmin_mosquito_prevalence, 
                                   quantile(mosquito_prevalence[, j], probs = 0.025, 
                                            na.rm = T))
      mean_mosquito_prevalence = c(mean_mosquito_prevalence, 
                                   mean(mosquito_prevalence[, j], na.rm = T))
    }
    qmax_mosquito_prevalence[which(is.na(qmax_mosquito_prevalence))] = 0
    qmin_mosquito_prevalence[which(is.na(qmin_mosquito_prevalence))] = 0
    mean_mosquito_prevalence[which(is.na(mean_mosquito_prevalence))] = 0
    for (index_giorno_pool in quando_ho_pool) {
      giorno_pool = settimane[index_giorno_pool] - 
        120
      if (giorno_pool < tmax) {
        prob_pool_positivo = 1 - (1 - mosquito_prevalence[, 
                                                          giorno_pool])^as.numeric(pool_size[quale_cluster, 
                                                                                             index_giorno_pool])
        pool_analizzati = numero_pool[quale_cluster, 
                                      index_giorno_pool]
        pool_positivi = numero_pool_positivi[quale_cluster, 
                                             index_giorno_pool]
        tmp = c()
        tmp_sel = sample(1:length(prob_pool_positivo), 
                         size = 100)
        for (quale in tmp_sel) tmp = c(tmp, rbinom(n = 1, 
                                                   size = pool_analizzati, prob = prob_pool_positivo[quale]))
        pool_positivi_simulati = cbind(pool_positivi_simulati, 
                                       tmp)
      }
    }
    ymax = ymax_plot
    xmax = ncol(pool_positivi_simulati)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = paste(quale_specie, anno))
    for (i in 1:(ncol(pool_positivi_simulati))) plotbox(w = i, 
                                                        X = pool_positivi_simulati[, i], fun = median, 
                                                        wd = 0.2, col = "blue") #colori[quale_cluster])
    points(1:xmax, numero_pool_positivi[quale_cluster, 
                                        quando_ho_pool][1:xmax], pch = 19, col = "darkorange", 
           cex = 2)
    date = format(as.Date(as.numeric(settimane[quando_ho_pool[1:xmax]]), 
                          origin = paste("01-01-", anno, sep = ""), format = "%d-%m-%Y"), 
                  "%d-%m")
    axis(1, at = c(1:xmax), labels = date, las = 2)
    axis(2, las = 2, at = seq(0, ymax, 5))
    mtext("WNV positive pools", side = 2, line = 3, 
          adj = 0.5, cex = 1.5)
    legend( x = "topright",
            legend = c("Observations","Predictions"),
            col = c("orange","blue"), lwd = 2, lty = c(0,0),
            pch = c(19,15),
            bty = "n",
            cex = 1.5,
            y.intersp = 0.7)
    quante_osservazioni = quante_osservazioni + length(quando_ho_pool)
    for (i in 1:(ncol(pool_positivi_simulati))) {
      qmin = quantile(pool_positivi_simulati[, i], 
                      probs = 0.025)
      qmax = quantile(pool_positivi_simulati[, i], 
                      probs = 0.975)
      if (numero_pool_positivi[quale_cluster, quando_ho_pool][i] >= 
          qmin & numero_pool_positivi[quale_cluster, 
                                      quando_ho_pool][i] <= qmax) 
        quanti_dentro95 = quanti_dentro95 + 1
    }
  }
  #dev.off()
}
#### model fit plot GERMANI ####
anno_inizio = 2016
anno_fine = 2018
numero_cluster = 1 
con_cosa_inizio = 1
max_iter_MCMC = 10000
tmax = 180

quale_specie = "Mallards"

FolderSimu = "Output_WNV/Germani/Simulazioni/"
FileDynName = "dynamics_2spec_Germani_" #CAMBIA NOME A SECONDA DELLA SPECIE
FolderPlotOut = "Output_WNV/Germani/Simulazioni/Plots/"
PlotName = "ModelFit_2spec_Germani_" #CAMBIA NOME A SECONDA DELLA SPECIE
file_numero_pool_name = "per_mcmc/totale_pool_cluster_"
file_numero_pool_positivi_name = "per_mcmc/totale_pool_WNVpos_cluster_" 
file_pool_size_name = "per_mcmc/mean_pool_size_cluster_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

cex_main = 2
cex_axes = 1.5
cex_lab_axes = 1.5
margins_plot = c(5, 5, 2, 2)
ymax_plot = 17

settimane = scan(vettore_date_catture)
colori = rainbow(n = numero_cluster)
quanti_dentro95 = 0
quante_osservazioni = 0

for (anno in anno_inizio:anno_fine) {
  print(anno)
  nome_file_plot = paste0(FolderPlotOut, PlotName, anno, 
                          ".pdf")
  #jpeg(nome_file_plot, height = 1 * 1000, width = 1 * 1400, 
  #    res = 200)
  par(mfrow = c(1, 1), mar = margins_plot, cex.main = cex_main, 
      cex.axis = cex_axes)
  message(paste("plot ModelFit", anno))
  file_numero_pool = paste0(file_numero_pool_name, anno)
  file_numero_pool_positivi = paste0(file_numero_pool_positivi_name, 
                                     anno)
  file_pool_size = paste0(file_pool_size_name, anno)
  numero_pool = read.table(file_numero_pool)
  numero_pool_positivi = read.table(file_numero_pool_positivi)
  pool_size = read.table(file_pool_size)
  numero_classi = 11
  for (quale_cluster in 1:numero_cluster) {
    quando_ho_pool = which(pool_size[quale_cluster, ] > 
                             0)
    pool_positivi_simulati = c()
    con_cosa_inizio = 1
    nome_file_output_dynamics = paste0(FolderSimu, FileDynName, 
                                       "M_", anno, "_", quale_cluster, ".txt")
    output_dynamics = read.table(nome_file_output_dynamics)
    mean_MS_allcluster = c()
    mean_ME_allcluster = c()
    mean_MI_allcluster = c()
    sel_MS = seq(1, nrow(output_dynamics), numero_classi)
    sel_ME = seq(2, nrow(output_dynamics), numero_classi)
    sel_MI = seq(3, nrow(output_dynamics), numero_classi)
    MS = output_dynamics[sel_MS, ]
    ME = output_dynamics[sel_ME, ]
    MI = output_dynamics[sel_MI, ]
    mosquito_prevalence = MI/(MS + ME + MI)
    qmax_mosquito_prevalence = c()
    qmin_mosquito_prevalence = c()
    mean_mosquito_prevalence = c()
    for (j in 1:(ncol(mosquito_prevalence))) {
      qmax_mosquito_prevalence = c(qmax_mosquito_prevalence, 
                                   quantile(mosquito_prevalence[, j], probs = 0.975, 
                                            na.rm = T))
      qmin_mosquito_prevalence = c(qmin_mosquito_prevalence, 
                                   quantile(mosquito_prevalence[, j], probs = 0.025, 
                                            na.rm = T))
      mean_mosquito_prevalence = c(mean_mosquito_prevalence, 
                                   mean(mosquito_prevalence[, j], na.rm = T))
    }
    qmax_mosquito_prevalence[which(is.na(qmax_mosquito_prevalence))] = 0
    qmin_mosquito_prevalence[which(is.na(qmin_mosquito_prevalence))] = 0
    mean_mosquito_prevalence[which(is.na(mean_mosquito_prevalence))] = 0
    for (index_giorno_pool in quando_ho_pool) {
      giorno_pool = settimane[index_giorno_pool] - 
        120
      if (giorno_pool < tmax) {
        prob_pool_positivo = 1 - (1 - mosquito_prevalence[, 
                                                          giorno_pool])^as.numeric(pool_size[quale_cluster, 
                                                                                             index_giorno_pool])
        pool_analizzati = numero_pool[quale_cluster, 
                                      index_giorno_pool]
        pool_positivi = numero_pool_positivi[quale_cluster, 
                                             index_giorno_pool]
        tmp = c()
        tmp_sel = sample(1:length(prob_pool_positivo), 
                         size = 100)
        for (quale in tmp_sel) tmp = c(tmp, rbinom(n = 1, 
                                                   size = pool_analizzati, prob = prob_pool_positivo[quale]))
        pool_positivi_simulati = cbind(pool_positivi_simulati, 
                                       tmp)
      }
    }
    ymax = ymax_plot
    xmax = ncol(pool_positivi_simulati)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = paste(quale_specie, anno))
    for (i in 1:(ncol(pool_positivi_simulati))) plotbox(w = i, 
                                                        X = pool_positivi_simulati[, i], fun = median, 
                                                        wd = 0.2, col = "blue") #colori[quale_cluster])
    points(1:xmax, numero_pool_positivi[quale_cluster, 
                                        quando_ho_pool][1:xmax], pch = 19, col = "darkorange", 
           cex = 2)
    date = format(as.Date(as.numeric(settimane[quando_ho_pool[1:xmax]]), 
                          origin = paste("01-01-", anno, sep = ""), format = "%d-%m-%Y"), 
                  "%d-%m")
    axis(1, at = c(1:xmax), labels = date, las = 2)
    axis(2, las = 2, at = seq(0, ymax, 5))
    mtext("WNV positive pools", side = 2, line = 3, 
          adj = 0.5, cex = 1.5)
    legend( x = "topright",
            legend = c("Observations","Predictions"),
            col = c("orange","blue"), lwd = 2, lty = c(0,0),
            pch = c(19,15),
            bty = "n",
            cex = 1.5,
            y.intersp = 0.7)
    quante_osservazioni = quante_osservazioni + length(quando_ho_pool)
    for (i in 1:(ncol(pool_positivi_simulati))) {
      qmin = quantile(pool_positivi_simulati[, i], 
                      probs = 0.025)
      qmax = quantile(pool_positivi_simulati[, i], 
                      probs = 0.975)
      if (numero_pool_positivi[quale_cluster, quando_ho_pool][i] >= 
          qmin & numero_pool_positivi[quale_cluster, 
                                      quando_ho_pool][i] <= qmax) 
        quanti_dentro95 = quanti_dentro95 + 1
    }
  }
  #dev.off()
}


#### birth pulse plot 2sp_Merli ####
#birth pulse della comunità aviare----
parms = "Output_WNV/Merli/MCMC/parametri_2spec_Merli_M_2018_1.txt" 
tmin = 91
tmax = 274
quante_specie = 2
anno = 2018
par(mar = c(5, 7, 5, 0))

if (is.character(parms)) {
  if (quante_specie == 1) 
    nomi_parametri = c("p", "B0", "pR", "b1", "muB", 
                       "s", "phi", "niB", "recB")
  if (quante_specie == 2) 
    nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", 
                       "pR_ca", "pR_sn", "b1_ca", "muB_ca", "s_ca", 
                       "phi_ca", "niB_ca", "recB_ca")
  parametri_stimati = vector("list", length(nomi_parametri))
  output_mcmc = read.table(parms)
  colnames(output_mcmc) = c(nomi_parametri, "lik")
  burnin = nrow(output_mcmc) * 0.1
  output_mcmc = output_mcmc[-c(1:burnin), ]
  if (quante_specie == 1) {
    birth_pulse = apply(output_mcmc, MARGIN = 1, function(a) {
      with(as.list(a), muB * exp(-s * sin(pi * (seq(0, 
                                                    365)/365 - phi))^2)/besselI(s/2, 0, T))
    })
  }
  if (quante_specie == 2) {
    birth_pulse = apply(output_mcmc, MARGIN = 1, function(a) {
      with(as.list(a), muB_ca * exp(-s_ca * sin(pi * 
                                                  (seq(0, 365)/365 - phi_ca))^2)/besselI(s_ca/2, 
                                                                                         0, T))
    })
  }
  mean = apply(birth_pulse, MARGIN = 1, function(a) {
    mean(a, na.rm = T)
  })
  qmin = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.025, na.rm = T)
  })
  qmax = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.975, na.rm = T)
  })
  ymax = 0.1 #max(qmax, qmin, mean)
  plot(0, col = "white", xlim = c(0, 366), ylim = c(0, ymax), 
       xlab = "", ylab = "", main = anno, cex.main = 2.5, axes = F)
  abline(v = c(tmin, tmax), lwd = 3, col = "darkorange")
  lines(mean, lwd = 3.5, col = "black")
  axis(1, at = c(1, 1 + 31, 1 + 31 + 28, 1 + 31 + 28 + 
                   31, 1 + 31 + 28 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 , 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + 31), labels = c("Gen", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                                                                           "Aug", "Sept", "Oct", "Nov", "Dec", "Gen"), cex.axis = 2, las = 2)
  
  axis(2, at = seq(0, ymax, 0.02), las = 2, 
       labels = format(seq(0, ymax, 0.02), 
                       digits = 2, scientific = F), cex.axis = 2)
  mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)
  legend(275,0.1, legend = "AC-blackbirds", lty = 1, lwd = 3.5, col = "black", 
         cex = 1.2, box.lwd = F)
  
  #poligono = cbind(x = c(1:length(mean), length(mean):1), 
                  # y = c(qmin, rev(qmax)))
#  polygon(poligono, col = adjustcolor("grey", alpha = 0.2), 
 #         border = NA)
}
#birth pulse merli ----
parms = "Output_WNV/Merli/MCMC/parametri_Merli.txt" 
tmin = 91
tmax = 274
quante_specie = 3

if (is.character(parms)) {
  if (quante_specie == 1) 
    nomi_parametri = c("p", "B0", "pR", "b1", "muB", 
                       "s", "phi", "niB", "recB")
  if (quante_specie == 2) 
    nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", 
                       "pR_ca", "pR_sn", "b1_ca", "muB_ca", "s_ca", 
                       "phi_ca", "niB_ca", "recB_ca")
  if (quante_specie == 3) 
    nomi_parametri = c("muB", "s", "phi")
  
  parametri_stimati = vector("list", length(nomi_parametri))
  output_mcmc = read.table(parms)
  colnames(output_mcmc) = c(nomi_parametri, "lik")
  #burnin = nrow(output_mcmc) * 0.1
  #output_mcmc = output_mcmc[-c(1:burnin), ]
  
  if (quante_specie == 1) {
    birth_pulse = apply(output_mcmc, MARGIN = 1, function(a) {
      with(as.list(a), muB * exp(-s * sin(pi * (seq(0, 
                                                    365)/365 - phi))^2)/besselI(s/2, 0, T))
    })
  }
  if (quante_specie == 2) {
    birth_pulse = apply(output_mcmc, MARGIN = 1, function(a) {
      with(as.list(a), muB_ca * exp(-s_ca * sin(pi * 
                                                  (seq(0, 365)/365 - phi_ca))^2)/besselI(s_ca/2,                                                                                      0, T))
    })
  }
  if (quante_specie == 3) {
    birth_pulse = apply(output_mcmc, MARGIN = 1, function(a) {
      with(as.list(a), muB * exp(-s * sin(pi * 
                                                  (seq(0, 365)/365 - phi))^2)/besselI(s/2,                                                                                     0, T))
    })
  }
  mean = apply(birth_pulse, MARGIN = 1, function(a) {
    mean(a, na.rm = T)
  })
  qmin = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.025, na.rm = T)
  })
  qmax = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.975, na.rm = T)
  })
  ymax = 0.1 #max(qmax, qmin, mean)
  plot(0, col = "white", xlim = c(0, 366), ylim = c(0, 
                                                    ymax), xlab = "", ylab = "", axes = F)
  abline(v = c(tmin, tmax), lwd = 2, col = "darkorange")
  lines(mean, lwd = 3, col = "black")
  axis(1, at = c(1, 1 + 31, 1 + 31 + 28, 1 + 31 + 28 + 
                   31, 1 + 31 + 28 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 , 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + 31), labels = c("Gen", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                                                                           "Aug", "Sept", "Oct", "Nov", "Dec", "Gen"), cex.axis = 2, las = 2)
  
  axis(2, at = seq(0, ymax, 0.02), las = 2, 
       labels = format(seq(0, ymax, 0.02), 
                       digits = 2, scientific = F), cex.axis = 2)
  mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)
  
  legend(275,0.1, legend = "Blackbirds", lty = 1, lwd = 3, col = "black", 
         cex = 1.4, box.lwd = F)
}




#### birth pulse plot 2sp_Gazze ####
#birth pulse della comunità aviare----
parms = "Output_WNV/Gazze/MCMC/parametri_2spec_Gazze_M_2018_1.txt" 
tmin = 91
tmax = 274
quante_specie = 2
anno = 2018

if (is.character(parms)) {
  if (quante_specie == 1) 
    nomi_parametri = c("p", "B0", "pR", "b1", "muB", 
                       "s", "phi", "niB", "recB")
  if (quante_specie == 2) 
    nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", 
                       "pR_ca", "pR_sn", "b1_ca", "muB_ca", "s_ca", 
                       "phi_ca", "niB_ca", "recB_ca")
  parametri_stimati = vector("list", length(nomi_parametri))
  output_mcmc = read.table(parms)
  colnames(output_mcmc) = c(nomi_parametri, "lik")
  burnin = nrow(output_mcmc) * 0.1
  output_mcmc = output_mcmc[-c(1:burnin), ]
  if (quante_specie == 1) {
    birth_pulse = apply(output_mcmc, MARGIN = 1, function(a) {
      with(as.list(a), muB * exp(-s * sin(pi * (seq(0, 
                                                    365)/365 - phi))^2)/besselI(s/2, 0, T))
    })
  }
  if (quante_specie == 2) {
    birth_pulse = apply(output_mcmc, MARGIN = 1, function(a) {
      with(as.list(a), muB_ca * exp(-s_ca * sin(pi * 
                                                  (seq(0, 365)/365 - phi_ca))^2)/besselI(s_ca/2, 
                                                                                         0, T))
    })
  }
  mean = apply(birth_pulse, MARGIN = 1, function(a) {
    mean(a, na.rm = T)
  })
  qmin = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.025, na.rm = T)
  })
  qmax = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.975, na.rm = T)
  })
  ymax = 0.1 #max(qmax, qmin, mean)
  plot(0, col = "white", xlim = c(0, 366), ylim = c(0, 
                                                    ymax), xlab = "", ylab = "", main = anno, cex.main = 3, axes = F)
  abline(v = c(tmin, tmax), lwd = 2, col = "darkorange")
  lines(mean, lwd = 3, col = "purple")
  axis(1, at = c(1, 1 + 31, 1 + 31 + 28, 1 + 31 + 28 + 
                   31, 1 + 31 + 28 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 , 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + 31), labels = c("Gen", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                                                                           "Aug", "Sept", "Oct", "Nov", "Dec", "Gen"), cex.axis = 2, las = 2)
  
  axis(2, at = seq(0, ymax, 0.02), las = 2, 
       labels = format(seq(0, ymax, 0.02), 
                       digits = 2, scientific = F), cex.axis = 2)
  mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)
  legend(275,0.08, legend = "AC-magpies", lty = 1, lwd = 2.5, col = "purple",
         cex = 1.4, box.lwd = F)
  
  #poligono = cbind(x = c(1:length(mean), length(mean):1), 
  # y = c(qmin, rev(qmax)))
  #  polygon(poligono, col = adjustcolor("grey", alpha = 0.2), 
  #         border = NA)
}
#birth pulse gazze ----
parms = "Output_WNV/Gazze/MCMC/parametri_Gazze.txt" 
tmin = 91
tmax = 274
quante_specie = 3

if (is.character(parms)) {
  if (quante_specie == 1) 
    nomi_parametri = c("p", "B0", "pR", "b1", "muB", 
                       "s", "phi", "niB", "recB")
  if (quante_specie == 2) 
    nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", 
                       "pR_ca", "pR_sn", "b1_ca", "muB_ca", "s_ca", 
                       "phi_ca", "niB_ca", "recB_ca")
  if (quante_specie == 3) 
    nomi_parametri = c("muB", "s", "phi")
  
  parametri_stimati = vector("list", length(nomi_parametri))
  output_mcmc = read.table(parms)
  colnames(output_mcmc) = c(nomi_parametri, "lik")
  #burnin = nrow(output_mcmc) * 0.1
  #output_mcmc = output_mcmc[-c(1:burnin), ]
  
  if (quante_specie == 1) {
    birth_pulse = apply(output_mcmc, MARGIN = 1, function(a) {
      with(as.list(a), muB * exp(-s * sin(pi * (seq(0, 
                                                    365)/365 - phi))^2)/besselI(s/2, 0, T))
    })
  }
  if (quante_specie == 2) {
    birth_pulse = apply(output_mcmc, MARGIN = 1, function(a) {
      with(as.list(a), muB_ca * exp(-s_ca * sin(pi * 
                                                  (seq(0, 365)/365 - phi_ca))^2)/besselI(s_ca/2,                                                                                      0, T))
    })
  }
  if (quante_specie == 3) {
    birth_pulse = apply(output_mcmc, MARGIN = 1, function(a) {
      with(as.list(a), muB * exp(-s * sin(pi * 
                                            (seq(0, 365)/365 - phi))^2)/besselI(s/2,                                                                                     0, T))
    })
  }
  mean = apply(birth_pulse, MARGIN = 1, function(a) {
    mean(a, na.rm = T)
  })
  qmin = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.025, na.rm = T)
  })
  qmax = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.975, na.rm = T)
  })
  ymax = 0.1 #max(qmax, qmin, mean)
  plot(0, col = "white", xlim = c(0, 366), ylim = c(0, 
                                                    ymax), xlab = "", ylab = "", axes = F)
  abline(v = c(tmin, tmax), lwd = 2, col = "darkorange")
  lines(mean, lwd = 3, col = "purple")
  axis(1, at = c(1, 1 + 31, 1 + 31 + 28, 1 + 31 + 28 + 
                   31, 1 + 31 + 28 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 , 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + 31), labels = c("Gen", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                                                                           "Aug", "Sept", "Oct", "Nov", "Dec", "Gen"), cex.axis = 2, las = 2)
  
  axis(2, at = seq(0, ymax, 0.02), las = 2, 
       labels = format(seq(0, ymax, 0.02), 
                       digits = 2, scientific = F), cex.axis = 2)
  mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)
  legend(275,0.08, legend = "Magpies", lty = 1, lwd = 2.5, col = "purple",
         cex = 1.5, box.lwd = F)
  
}





#### birth pulse plot 2sp_Germani ####
#birth pulse della comunità aviare----
parms = "Output_WNV/Germani/MCMC/parametri_2spec_Germani_M_2018_1.txt" 
tmin = 91
tmax = 274
quante_specie = 2
anno = 2018

if (is.character(parms)) {
  if (quante_specie == 1) 
    nomi_parametri = c("p", "B0", "pR", "b1", "muB", 
                       "s", "phi", "niB", "recB")
  if (quante_specie == 2) 
    nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", 
                       "pR_ca", "pR_sn", "b1_ca", "muB_ca", "s_ca", 
                       "phi_ca", "niB_ca", "recB_ca")
  parametri_stimati = vector("list", length(nomi_parametri))
  output_mcmc = read.table(parms)
  colnames(output_mcmc) = c(nomi_parametri, "lik")
  burnin = nrow(output_mcmc) * 0.1
  output_mcmc = output_mcmc[-c(1:burnin), ]
  if (quante_specie == 1) {
    birth_pulse = apply(output_mcmc, MARGIN = 1, function(a) {
      with(as.list(a), muB * exp(-s * sin(pi * (seq(0, 
                                                    365)/365 - phi))^2)/besselI(s/2, 0, T))
    })
  }
  if (quante_specie == 2) {
    birth_pulse = apply(output_mcmc, MARGIN = 1, function(a) {
      with(as.list(a), muB_ca * exp(-s_ca * sin(pi * 
                                                  (seq(0, 365)/365 - phi_ca))^2)/besselI(s_ca/2, 
                                                                                         0, T))
    })
  }
  mean = apply(birth_pulse, MARGIN = 1, function(a) {
    mean(a, na.rm = T)
  })
  qmin = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.025, na.rm = T)
  })
  qmax = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.975, na.rm = T)
  })
  ymax = 0.1 #max(qmax, qmin, mean)
  plot(0, col = "white", xlim = c(0, 366), ylim = c(0, 
                                                    ymax), xlab = "", ylab = "", main = anno, cex.main = 3, axes = F)
  abline(v = c(tmin, tmax), lwd = 2, col = "darkorange")
  lines(mean, lwd = 3, col = "green")
  axis(1, at = c(1, 1 + 31, 1 + 31 + 28, 1 + 31 + 28 + 
                   31, 1 + 31 + 28 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 , 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + 31), labels = c("Gen", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                                                                           "Aug", "Sept", "Oct", "Nov", "Dec", "Gen"), cex.axis = 2, las = 2)
  
  axis(2, at = seq(0, ymax, 0.02), las = 2, 
       labels = format(seq(0, ymax, 0.02), 
                       digits = 2, scientific = F), cex.axis = 2)
  mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)
  legend(275,0.06, legend = "AC-mallards", lty = 1, lwd = 2.5, col = "green",
         cex = 1.5, box.lwd = F)
  
  #poligono = cbind(x = c(1:length(mean), length(mean):1), 
  # y = c(qmin, rev(qmax)))
  #  polygon(poligono, col = adjustcolor("grey", alpha = 0.2), 
  #         border = NA)
}
#birth pulse germani ----
parms = "Output_WNV/Germani/MCMC/parametri_Germani.txt" 
tmin = 91
tmax = 274
quante_specie = 3

if (is.character(parms)) {
  if (quante_specie == 1) 
    nomi_parametri = c("p", "B0", "pR", "b1", "muB", 
                       "s", "phi", "niB", "recB")
  if (quante_specie == 2) 
    nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", 
                       "pR_ca", "pR_sn", "b1_ca", "muB_ca", "s_ca", 
                       "phi_ca", "niB_ca", "recB_ca")
  if (quante_specie == 3) 
    nomi_parametri = c("muB", "s", "phi")
  
  parametri_stimati = vector("list", length(nomi_parametri))
  output_mcmc = read.table(parms)
  colnames(output_mcmc) = c(nomi_parametri, "lik")
  #burnin = nrow(output_mcmc) * 0.1
  #output_mcmc = output_mcmc[-c(1:burnin), ]
  
  if (quante_specie == 1) {
    birth_pulse = apply(output_mcmc, MARGIN = 1, function(a) {
      with(as.list(a), muB * exp(-s * sin(pi * (seq(0, 
                                                    365)/365 - phi))^2)/besselI(s/2, 0, T))
    })
  }
  if (quante_specie == 2) {
    birth_pulse = apply(output_mcmc, MARGIN = 1, function(a) {
      with(as.list(a), muB_ca * exp(-s_ca * sin(pi * 
                                                  (seq(0, 365)/365 - phi_ca))^2)/besselI(s_ca/2,                                                                                      0, T))
    })
  }
  if (quante_specie == 3) {
    birth_pulse = apply(output_mcmc, MARGIN = 1, function(a) {
      with(as.list(a), muB * exp(-s * sin(pi * 
                                            (seq(0, 365)/365 - phi))^2)/besselI(s/2,                                                                                     0, T))
    })
  }
  mean = apply(birth_pulse, MARGIN = 1, function(a) {
    mean(a, na.rm = T)
  })
  qmin = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.025, na.rm = T)
  })
  qmax = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.975, na.rm = T)
  })
  ymax = 0.1 #max(qmax, qmin, mean)
  plot(0, col = "white", xlim = c(0, 366), ylim = c(0, 
                                                    ymax), xlab = "", ylab = "", axes = F)
  abline(v = c(tmin, tmax), lwd = 2, col = "darkorange")
  lines(mean, lwd = 3, col = "green")
  axis(1, at = c(1, 1 + 31, 1 + 31 + 28, 1 + 31 + 28 + 
                   31, 1 + 31 + 28 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31, 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 , 1 + 31 + 28 + 31 + 
                   30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + 31), labels = c("Gen", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                                                                           "Aug", "Sept", "Oct", "Nov", "Dec", "Gen"), cex.axis = 2, las = 2)
  
  axis(2, at = seq(0, ymax, 0.02), las = 2, 
       labels = format(seq(0, ymax, 0.02), 
                       digits = 2, scientific = F), cex.axis = 2)
  mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)
  legend(275,0.06, legend = "Mallards", lty = 1, lwd = 3, col = "green",
         cex = 1.5, box.lwd = F)
  
}




#### contribution plot merli ####
FolderSimu = "Output_WNV/Merli/Simulazioni/"
FileDynName = "dynamics_2spec_Merli_"
anno = 2018  #cambia l'anno di volta in volta
quale_cluster = 1
numero_classi = 11

quale_specie = "Blackbirds"

nome_file_output_dynamics = paste0(FolderSimu, 
                                   FileDynName, "M_", anno, "_", quale_cluster, 
                                   ".txt")

nome_file_parametri = paste0("Output_WNV/Merli/MCMC/parametri_2spec_Merli_M_", anno,
                             "_1.txt")

output_mcmc = read.table(nome_file_parametri)
colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                          "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca",
                          "niB_ca", "recB_ca")

output_dynamics = read.table(nome_file_output_dynamics)
sel_MS = seq(1, nrow(output_dynamics), numero_classi)
sel_ME = seq(2, nrow(output_dynamics), numero_classi)
sel_MI = seq(3, nrow(output_dynamics), numero_classi)
sel_BS = seq(4, nrow(output_dynamics), numero_classi)
sel_BE = seq(5, nrow(output_dynamics), numero_classi)
sel_BI = seq(6, nrow(output_dynamics), numero_classi)
sel_BR = seq(7, nrow(output_dynamics), numero_classi)
sel_BS2 = seq(8, nrow(output_dynamics), numero_classi)
sel_BE2 = seq(9, nrow(output_dynamics), numero_classi)
sel_BI2 = seq(10, nrow(output_dynamics), numero_classi)
sel_BR2 = seq(11, nrow(output_dynamics), numero_classi)
MS = output_dynamics[sel_MS, ]
ME = output_dynamics[sel_ME, ]
MI = output_dynamics[sel_MI, ]
BS = output_dynamics[sel_BS, ]
BE = output_dynamics[sel_BE, ]
BI = output_dynamics[sel_BI, ]
BR = output_dynamics[sel_BR, ]
BS2 = output_dynamics[sel_BS2, ]
BE2 = output_dynamics[sel_BE2, ]
BI2 = output_dynamics[sel_BI2, ]
BR2 = output_dynamics[sel_BR2, ]

b11 = 0.35 #usa biting rate specie nota
b1 = mean(output_mcmc[, 'b1_ca']) #usa bititng rate calcolato da MCMC (per avian community)

pop_tot = apply((BS + BE + BI + BR), MARGIN = 2, mean)
pop_tot2 = apply((BS2 + BE2 + BI2 + BR2), MARGIN = 2, mean)

num = b11*(apply(BI, MARGIN = 2, mean)/pop_tot)
den = b11*(apply(BI, MARGIN = 2, mean)/pop_tot) + b1*(apply(BI2, MARGIN = 2, mean)/pop_tot2)
contr_known_bird = (num/den)*100

par(mfrow = c(1,1))

plot(0, col = "white", xlim = c(0, 180), ylim = c(0, 100),
     xlab = "", ylab = "", axes = F, main = paste(quale_specie, anno), type = 'l')

lines(contr_known_bird, lwd = 3, col = "black")


axis(1, at = c(1, 1+30, 1+30 + 31, 1+30 + 31 + 30, 1+30 + 31 + 30 + 
                 31, 1+30 + 31 + 30 + 31 + 31, 1+30 + 31 + 30 + 31 + 
                 31 + 30),
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept","Oct"), 
     cex.axis = 1.5, lwd = 2.5)
axis(2, at = NULL, las =2, lwd = 2.5) 
mtext("Contribution %", side = 2, line = 3, adj = 0.5, cex = 1.5)

#plot(contr_known_bird, col='red', main = "Blackbirds contribution 2016", type = "l")

#### contributo AC ####
FolderSimu = "Output_WNV/Merli/Simulazioni/"
FileDynName = "dynamics_2spec_Merli_"
anno = 2018  #cambia l'anno di volta in volta
quale_cluster = 1
numero_classi = 11

quale_specie = "Avian community"

nome_file_output_dynamics = paste0(FolderSimu, 
                                   FileDynName, "M_", anno, "_", quale_cluster, 
                                   ".txt")

nome_file_parametri = paste0("Output_WNV/Merli/MCMC/parametri_2spec_Merli_M_", anno,
                             "_1.txt")

output_mcmc = read.table(nome_file_parametri)
colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                          "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca",
                          "niB_ca", "recB_ca")

output_dynamics = read.table(nome_file_output_dynamics)
sel_MS = seq(1, nrow(output_dynamics), numero_classi)
sel_ME = seq(2, nrow(output_dynamics), numero_classi)
sel_MI = seq(3, nrow(output_dynamics), numero_classi)
sel_BS = seq(4, nrow(output_dynamics), numero_classi)
sel_BE = seq(5, nrow(output_dynamics), numero_classi)
sel_BI = seq(6, nrow(output_dynamics), numero_classi)
sel_BR = seq(7, nrow(output_dynamics), numero_classi)
sel_BS2 = seq(8, nrow(output_dynamics), numero_classi)
sel_BE2 = seq(9, nrow(output_dynamics), numero_classi)
sel_BI2 = seq(10, nrow(output_dynamics), numero_classi)
sel_BR2 = seq(11, nrow(output_dynamics), numero_classi)
MS = output_dynamics[sel_MS, ]
ME = output_dynamics[sel_ME, ]
MI = output_dynamics[sel_MI, ]
BS = output_dynamics[sel_BS, ]
BE = output_dynamics[sel_BE, ]
BI = output_dynamics[sel_BI, ]
BR = output_dynamics[sel_BR, ]
BS2 = output_dynamics[sel_BS2, ]
BE2 = output_dynamics[sel_BE2, ]
BI2 = output_dynamics[sel_BI2, ]
BR2 = output_dynamics[sel_BR2, ]

b11 = 0.35 #usa biting rate specie nota
b1 = mean(output_mcmc[, 'b1_ca']) #usa bititng rate calcolato da MCMC (per avian community)

pop_tot = apply((BS + BE + BI + BR), MARGIN = 2, mean)
pop_tot2 = apply((BS2 + BE2 + BI2 + BR2), MARGIN = 2, mean)

num = b11*(apply(BI, MARGIN = 2, mean)/pop_tot)
num2 = b1*(apply(BI2, MARGIN = 2, mean)/pop_tot2)
den = b11*(apply(BI, MARGIN = 2, mean)/pop_tot) + b1*(apply(BI2, MARGIN = 2, mean)/pop_tot2)

contr_known_bird = (num/den)*100
contr_ac = (num2/den)*100

par(mfrow = c(1,1))

plot(0, col = "white", xlim = c(0, 180), ylim = c(0, 100),
     xlab = "", ylab = "", axes = F, main = paste(quale_specie, anno), type = 'l')

lines(contr_ac, lwd = 3, col = "blue")


axis(1, at = c(1, 1+30, 1+30 + 31, 1+30 + 31 + 30, 1+30 + 31 + 30 + 
                 31, 1+30 + 31 + 30 + 31 + 31, 1+30 + 31 + 30 + 31 + 
                 31 + 30),
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept","Oct"), 
     cex.axis = 1.5, lwd = 2.5)
axis(2, at = NULL, las =2, lwd = 2.5) 
mtext("Contribution %", side = 2, line = 3, adj = 0.5, cex = 1.5)



#Gazze ----
FolderSimu = "Output_WNV/Gazze/Simulazioni/"
FileDynName = "dynamics_2spec_Gazze_"
anno = 2018
quale_cluster = 1
numero_classi = 11

quale_specie = "Magpies"

nome_file_output_dynamics = paste0(FolderSimu, 
                                   FileDynName, "M_", anno, "_", quale_cluster, 
                                   ".txt")

nome_file_parametri = paste0("Output_WNV/Gazze/MCMC/parametri_2spec_Gazze_M_", anno,
                             "_1.txt")

output_mcmc = read.table(nome_file_parametri)
colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                          "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca",
                          "niB_ca", "recB_ca")

output_dynamics = read.table(nome_file_output_dynamics)
sel_MS = seq(1, nrow(output_dynamics), numero_classi)
sel_ME = seq(2, nrow(output_dynamics), numero_classi)
sel_MI = seq(3, nrow(output_dynamics), numero_classi)
sel_BS = seq(4, nrow(output_dynamics), numero_classi)
sel_BE = seq(5, nrow(output_dynamics), numero_classi)
sel_BI = seq(6, nrow(output_dynamics), numero_classi)
sel_BR = seq(7, nrow(output_dynamics), numero_classi)
sel_BS2 = seq(8, nrow(output_dynamics), numero_classi)
sel_BE2 = seq(9, nrow(output_dynamics), numero_classi)
sel_BI2 = seq(10, nrow(output_dynamics), numero_classi)
sel_BR2 = seq(11, nrow(output_dynamics), numero_classi)
MS = output_dynamics[sel_MS, ]
ME = output_dynamics[sel_ME, ]
MI = output_dynamics[sel_MI, ]
BS = output_dynamics[sel_BS, ]
BE = output_dynamics[sel_BE, ]
BI = output_dynamics[sel_BI, ]
BR = output_dynamics[sel_BR, ]
BS2 = output_dynamics[sel_BS2, ]
BE2 = output_dynamics[sel_BE2, ]
BI2 = output_dynamics[sel_BI2, ]
BR2 = output_dynamics[sel_BR2, ]

b11 = 0.1 #usa biting rate specie nota (questo è dei germani perchè è l'ultima specie che hai simu)
b1 = mean(output_mcmc[, 'b1_ca']) #usa bititng rate calcolato da MCMC (per avian community)

pop_tot = apply((BS + BE + BI + BR), MARGIN = 2, mean)
pop_tot2 = apply((BS2 + BE2 + BI2 + BR2), MARGIN = 2, mean)

num = b11*(apply(BI, MARGIN = 2, mean)/pop_tot)
den = b11*(apply(BI, MARGIN = 2, mean)/pop_tot) + b1*(apply(BI2, MARGIN = 2, mean)/pop_tot2)
contr_known_bird = (num/den)*100

par(mfrow = c(1,1))

plot(0, col = "white", xlim = c(0, 180), ylim = c(0, 40),
     xlab = "", ylab = "", axes = F, main = paste(quale_specie, anno), type = 'l')

lines(contr_known_bird, lwd = 3, col = "black")


axis(1, at = c(1, 1+30, 1+30 + 31, 1+30 + 31 + 30, 1+30 + 31 + 30 + 
                 31, 1+30 + 31 + 30 + 31 + 31, 1+30 + 31 + 30 + 31 + 
                 31 + 30),
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept","Oct"), 
     cex.axis = 1.5, lwd = 2.5)
axis(2, at = NULL, las =2, lwd = 2.5) 
mtext("Contribution %", side = 2, line = 3, adj = 0.5, cex = 1.5)

#plot(contr_known_bird, col='red', main = "contrib Gazze 2018 (%)", type = "l") #sistema

#### contributo AC ####
FolderSimu = "Output_WNV/Gazze/Simulazioni/"
FileDynName = "dynamics_2spec_Gazze_"
anno = 2018  #cambia l'anno di volta in volta
quale_cluster = 1
numero_classi = 11

quale_specie = "Avian community"

nome_file_output_dynamics = paste0(FolderSimu, 
                                   FileDynName, "M_", anno, "_", quale_cluster, 
                                   ".txt")

nome_file_parametri = paste0("Output_WNV/Gazze/MCMC/parametri_2spec_Gazze_M_", anno,
                             "_1.txt")

output_mcmc = read.table(nome_file_parametri)
colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                          "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca",
                          "niB_ca", "recB_ca")

output_dynamics = read.table(nome_file_output_dynamics)
sel_MS = seq(1, nrow(output_dynamics), numero_classi)
sel_ME = seq(2, nrow(output_dynamics), numero_classi)
sel_MI = seq(3, nrow(output_dynamics), numero_classi)
sel_BS = seq(4, nrow(output_dynamics), numero_classi)
sel_BE = seq(5, nrow(output_dynamics), numero_classi)
sel_BI = seq(6, nrow(output_dynamics), numero_classi)
sel_BR = seq(7, nrow(output_dynamics), numero_classi)
sel_BS2 = seq(8, nrow(output_dynamics), numero_classi)
sel_BE2 = seq(9, nrow(output_dynamics), numero_classi)
sel_BI2 = seq(10, nrow(output_dynamics), numero_classi)
sel_BR2 = seq(11, nrow(output_dynamics), numero_classi)
MS = output_dynamics[sel_MS, ]
ME = output_dynamics[sel_ME, ]
MI = output_dynamics[sel_MI, ]
BS = output_dynamics[sel_BS, ]
BE = output_dynamics[sel_BE, ]
BI = output_dynamics[sel_BI, ]
BR = output_dynamics[sel_BR, ]
BS2 = output_dynamics[sel_BS2, ]
BE2 = output_dynamics[sel_BE2, ]
BI2 = output_dynamics[sel_BI2, ]
BR2 = output_dynamics[sel_BR2, ]

b11 = 0.1 #usa biting rate specie nota
b1 = mean(output_mcmc[, 'b1_ca']) #usa bititng rate calcolato da MCMC (per avian community)

pop_tot = apply((BS + BE + BI + BR), MARGIN = 2, mean)
pop_tot2 = apply((BS2 + BE2 + BI2 + BR2), MARGIN = 2, mean)

num = b11*(apply(BI, MARGIN = 2, mean)/pop_tot)
num2 = b1*(apply(BI2, MARGIN = 2, mean)/pop_tot2)
den = b11*(apply(BI, MARGIN = 2, mean)/pop_tot) + b1*(apply(BI2, MARGIN = 2, mean)/pop_tot2)

contr_known_bird = (num/den)*100
contr_ac = (num2/den)*100

par(mfrow = c(1,1))

plot(0, col = "white", xlim = c(0, 180), ylim = c(0, 100),
     xlab = "", ylab = "", axes = F, main = paste(quale_specie, anno), type = 'l')

lines(contr_ac, lwd = 3, col = "blue")


axis(1, at = c(1, 1+30, 1+30 + 31, 1+30 + 31 + 30, 1+30 + 31 + 30 + 
                 31, 1+30 + 31 + 30 + 31 + 31, 1+30 + 31 + 30 + 31 + 
                 31 + 30),
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept","Oct"), 
     cex.axis = 1.5, lwd = 2.5)
axis(2, at = NULL, las =2, lwd = 2.5) 
mtext("Contribution %", side = 2, line = 3, adj = 0.5, cex = 1.5)
#Germani ----
FolderSimu = "Output_WNV/Germani/Simulazioni/"
FileDynName = "dynamics_2spec_Germani_"
anno = 2018
quale_cluster = 1
numero_classi = 11

quale_specie = "Mallards"
nome_file_output_dynamics = paste0(FolderSimu, 
                                   FileDynName, "M_", anno, "_", quale_cluster, 
                                   ".txt")

nome_file_parametri = paste0("Output_WNV/Germani/MCMC/parametri_2spec_Germani_M_", anno,
                             "_1.txt")

output_mcmc = read.table(nome_file_parametri)
colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                          "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca",
                          "niB_ca", "recB_ca")

output_dynamics = read.table(nome_file_output_dynamics)
sel_MS = seq(1, nrow(output_dynamics), numero_classi)
sel_ME = seq(2, nrow(output_dynamics), numero_classi)
sel_MI = seq(3, nrow(output_dynamics), numero_classi)
sel_BS = seq(4, nrow(output_dynamics), numero_classi)
sel_BE = seq(5, nrow(output_dynamics), numero_classi)
sel_BI = seq(6, nrow(output_dynamics), numero_classi)
sel_BR = seq(7, nrow(output_dynamics), numero_classi)
sel_BS2 = seq(8, nrow(output_dynamics), numero_classi)
sel_BE2 = seq(9, nrow(output_dynamics), numero_classi)
sel_BI2 = seq(10, nrow(output_dynamics), numero_classi)
sel_BR2 = seq(11, nrow(output_dynamics), numero_classi)
MS = output_dynamics[sel_MS, ]
ME = output_dynamics[sel_ME, ]
MI = output_dynamics[sel_MI, ]
BS = output_dynamics[sel_BS, ]
BE = output_dynamics[sel_BE, ]
BI = output_dynamics[sel_BI, ]
BR = output_dynamics[sel_BR, ]
BS2 = output_dynamics[sel_BS2, ]
BE2 = output_dynamics[sel_BE2, ]
BI2 = output_dynamics[sel_BI2, ]
BR2 = output_dynamics[sel_BR2, ]

b11 = 0.01 #usa biting rate specie nota (questo è dei germani perchè è l'ultima specie che hai simu)
b1 = mean(output_mcmc[, 'b1_ca']) #usa bititng rate calcolato da MCMC (per avian community)

pop_tot = apply((BS + BE + BI + BR), MARGIN = 2, mean)
pop_tot2 = apply((BS2 + BE2 + BI2 + BR2), MARGIN = 2, mean)

num = b11*(apply(BI, MARGIN = 2, mean)/pop_tot)
den = b11*(apply(BI, MARGIN = 2, mean)/pop_tot) + b1*(apply(BI2, MARGIN = 2, mean)/pop_tot2)
contr_known_bird = (num/den)*100

par(mfrow = c(1,1))

plot(0, col = "white", xlim = c(0, 180), ylim = c(0, 0.5),
     xlab = "", ylab = "", axes = F, main = paste(quale_specie, anno), type = 'l')

lines(contr_known_bird, lwd = 3, col = "black")


axis(1, at = c(1, 1+30, 1+30 + 31, 1+30 + 31 + 30, 1+30 + 31 + 30 + 
                 31, 1+30 + 31 + 30 + 31 + 31, 1+30 + 31 + 30 + 31 + 
                 31 + 30),
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept","Oct"), 
     cex.axis = 1.5, lwd = 2.5)
axis(2, at = NULL, las =2, lwd = 2.5) 
mtext("Contribution %", side = 2, line = 3, adj = 0.5, cex = 1.5)

#plot(contr_known_bird, col='red', main = "contrib Germani 2018 (%)", type = "l") #sistema


#### contributo AC ####
FolderSimu = "Output_WNV/Germani/Simulazioni/"
FileDynName = "dynamics_2spec_Germani_"
anno = 2018  #cambia l'anno di volta in volta
quale_cluster = 1
numero_classi = 11

quale_specie = "Avian community"

nome_file_output_dynamics = paste0(FolderSimu, 
                                   FileDynName, "M_", anno, "_", quale_cluster, 
                                   ".txt")

nome_file_parametri = paste0("Output_WNV/Germani/MCMC/parametri_2spec_Germani_M_", anno,
                             "_1.txt")

output_mcmc = read.table(nome_file_parametri)
colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                          "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca",
                          "niB_ca", "recB_ca")

output_dynamics = read.table(nome_file_output_dynamics)
sel_MS = seq(1, nrow(output_dynamics), numero_classi)
sel_ME = seq(2, nrow(output_dynamics), numero_classi)
sel_MI = seq(3, nrow(output_dynamics), numero_classi)
sel_BS = seq(4, nrow(output_dynamics), numero_classi)
sel_BE = seq(5, nrow(output_dynamics), numero_classi)
sel_BI = seq(6, nrow(output_dynamics), numero_classi)
sel_BR = seq(7, nrow(output_dynamics), numero_classi)
sel_BS2 = seq(8, nrow(output_dynamics), numero_classi)
sel_BE2 = seq(9, nrow(output_dynamics), numero_classi)
sel_BI2 = seq(10, nrow(output_dynamics), numero_classi)
sel_BR2 = seq(11, nrow(output_dynamics), numero_classi)
MS = output_dynamics[sel_MS, ]
ME = output_dynamics[sel_ME, ]
MI = output_dynamics[sel_MI, ]
BS = output_dynamics[sel_BS, ]
BE = output_dynamics[sel_BE, ]
BI = output_dynamics[sel_BI, ]
BR = output_dynamics[sel_BR, ]
BS2 = output_dynamics[sel_BS2, ]
BE2 = output_dynamics[sel_BE2, ]
BI2 = output_dynamics[sel_BI2, ]
BR2 = output_dynamics[sel_BR2, ]

b11 = 0.01 #usa biting rate specie nota
b1 = mean(output_mcmc[, 'b1_ca']) #usa bititng rate calcolato da MCMC (per avian community)

pop_tot = apply((BS + BE + BI + BR), MARGIN = 2, mean)
pop_tot2 = apply((BS2 + BE2 + BI2 + BR2), MARGIN = 2, mean)

num = b11*(apply(BI, MARGIN = 2, mean)/pop_tot)
num2 = b1*(apply(BI2, MARGIN = 2, mean)/pop_tot2)
den = b11*(apply(BI, MARGIN = 2, mean)/pop_tot) + b1*(apply(BI2, MARGIN = 2, mean)/pop_tot2)

contr_known_bird = (num/den)*100
contr_ac = (num2/den)*100

par(mfrow = c(1,1))

plot(0, col = "white", xlim = c(0, 180), ylim = c(0, 100),
     xlab = "", ylab = "", axes = F, main = paste(quale_specie, anno), type = 'l')

lines(contr_ac, lwd = 3, col = "blue")


axis(1, at = c(1, 1+30, 1+30 + 31, 1+30 + 31 + 30, 1+30 + 31 + 30 + 
                 31, 1+30 + 31 + 30 + 31 + 31, 1+30 + 31 + 30 + 31 + 
                 31 + 30),
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept","Oct"), 
     cex.axis = 1.5, lwd = 2.5)
axis(2, at = NULL, las =2, lwd = 2.5) 
mtext("Contribution %", side = 2, line = 3, adj = 0.5, cex = 1.5)



####dinamiche di popolazione MERLI####
#quanti sono i merli rispetto alla popolazione totale di uccelli stimata??
anno_inizio = 2016
anno_fine = 2018
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Merli/Simulazioni/"
FileDynName = "dynamics_2spec_Merli_"
FolderPlotOut = "Output_WNV/Merli/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Merli_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "blackbirds"
quale_specie_2 = "Avian community"
MS = "Susceptible mosquitoes"
cex_main = 2
cex_axes = 2
cex_lab_axes = 2
margins_plot = c(7, 7, 4, 2)
what_plot = "BI" #known_bird_pop_rate #avian_community
                                  #known_bird_population #MS #BI
to_plot_on_file = F
settimane = scan(vettore_date_catture)


for (anno in anno_inizio:anno_fine) {
  print(anno)
  nome_file_plot = paste0(FolderPlotOut, PlotName, what_plot, 
                          anno, ".jpg")
  if (to_plot_on_file) 
    jpeg(nome_file_plot, height = 1 * 1000, width = 1 * 
           1400, res = 200)
  par(mfrow = c(1, 1), mar = margins_plot, cex.main = cex_main, 
      cex.axis = cex_axes)
  numero_classi = 11
  for (quale_cluster in 1:numero_cluster) {
    if (con_cosa_inizio == 0) 
      nome_file_output_dynamics = paste0(FolderSimu, 
                                         FileDynName, "B_", anno, "_", quale_cluster, 
                                         ".txt")
    if (con_cosa_inizio == 1) 
      nome_file_output_dynamics = paste0(FolderSimu, 
                                         FileDynName, "M_", anno, "_", quale_cluster, 
                                         ".txt")
    output_dynamics = read.table(nome_file_output_dynamics)
    sel_MS = seq(1, nrow(output_dynamics), numero_classi)
    sel_ME = seq(2, nrow(output_dynamics), numero_classi)
    sel_MI = seq(3, nrow(output_dynamics), numero_classi)
    sel_BS = seq(4, nrow(output_dynamics), numero_classi)
    sel_BE = seq(5, nrow(output_dynamics), numero_classi)
    sel_BI = seq(6, nrow(output_dynamics), numero_classi)
    sel_BR = seq(7, nrow(output_dynamics), numero_classi)
    sel_BS2 = seq(8, nrow(output_dynamics), numero_classi)
    sel_BE2 = seq(9, nrow(output_dynamics), numero_classi)
    sel_BI2 = seq(10, nrow(output_dynamics), numero_classi)
    sel_BR2 = seq(11, nrow(output_dynamics), numero_classi)
    MS = output_dynamics[sel_MS, ]
    ME = output_dynamics[sel_ME, ]
    MI = output_dynamics[sel_MI, ]
    BS = output_dynamics[sel_BS, ]
    BE = output_dynamics[sel_BE, ]
    BI = output_dynamics[sel_BI, ]
    BR = output_dynamics[sel_BR, ]
    BS2 = output_dynamics[sel_BS2, ]
    BE2 = output_dynamics[sel_BE2, ]
    BI2 = output_dynamics[sel_BI2, ]
    BR2 = output_dynamics[sel_BR2, ]
    mosquito_population = MS + ME + MI
    mosquito_prevalence = (MI/mosquito_population) *100
    all_bird_population = BS + BE + BI + BR + BS2 + BE2 + BI2 + BR2
    all_bird_prevalence = ((BI + BI2)/(BS + BE + BI + 
                                         BR + BS2 + BE2 + BI2 + BR2)) * 100
    all_bird_seroprevalence = ((BR + BR2)/(BS + BE + 
                                             BI + BR + BS2 + BE2 + BI2 + BR2)) * 100
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
   
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of birds"
      colore = "purple"
      main = paste(quale_specie_2,anno)
    }
    if (what_plot == "BI2") {
      pop = BI2
      ytext = "Number of I birds AC"
      colore = "purple"
      main = paste(quale_specie_2,anno)
    }
   
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of blackbirds"
      colore = "darkgreen"
      main = paste(quale_specie, anno)
    }
    if (what_plot == "BI") {
      pop = BI
      ytext = "Number of I blackbirds"
      colore = "darkgreen"
      main = paste(quale_specie, anno)
    }
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", 
                    quale_specie)
      colore = "black"
      main = anno
    }
    
    if (what_plot == "susceptible_mosquitoes") {
      pop = MS
      ytext = "Number of S-moquitoes"
      colore = "red"
    }
    
    qmax_pop = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop = apply(pop, MARGIN = 2, mean)
    qmax_pop[which(is.na(qmax_pop))] = 0
    qmin_pop[which(is.na(qmin_pop))] = 0
    mean_pop[which(is.na(mean_pop))] = 0
    ymax = max(mean_pop) #max(qmax_pop)
    xmax = length(qmax_pop)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop, lwd = 3, col = colore)
    #poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
     #                y = c(qmin_pop, rev(qmax_pop)))
    #polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
    #        border = NA)
    axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                     31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                     31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                          "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
    scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                            T, F)
    axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
         labels = format(seq(0, ymax, length.out = 4), 
                         digits = 2, scientific = scientific_num), cex.axis = cex_axes)
    mtext(ytext, side = 2, line = 5, cex = cex_axes)
  }
 # dev.off()
}

####mosquito prevalence
anno_inizio = 2016
anno_fine = 2018
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Merli/Simulazioni/"
FileDynName = "dynamics_2spec_Merli_"
FolderPlotOut = "Output_WNV/Merli/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Merli_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "blackbirds"
quale_specie_2 = "Avian community"
M = "Mosquito prevalence"
cex_main = 2
cex_axes = 1.2
cex_lab_axes = 1.2
margins_plot = c(7, 7, 4, 2)
what_plot = "infectious_mosquitoes" #known_bird_pop_rate #avian_community
#known_bird_population #MS
to_plot_on_file = F
settimane = scan(vettore_date_catture)



for (anno in anno_inizio:anno_fine) {
  print(anno)
  nome_file_plot = paste0(FolderPlotOut, PlotName, what_plot, 
                          anno, ".jpg")
  if (to_plot_on_file) 
    jpeg(nome_file_plot, height = 1 * 1000, width = 1 * 
           1400, res = 200)
  par(mfrow = c(1, 1), mar = margins_plot, cex.main = cex_main, 
      cex.axis = cex_axes)
  numero_classi = 11
  for (quale_cluster in 1:numero_cluster) {
    if (con_cosa_inizio == 0) 
      nome_file_output_dynamics = paste0(FolderSimu, 
                                         FileDynName, "B_", anno, "_", quale_cluster, 
                                         ".txt")
    if (con_cosa_inizio == 1) 
      nome_file_output_dynamics = paste0(FolderSimu, 
                                         FileDynName, "M_", anno, "_", quale_cluster, 
                                         ".txt")
    output_dynamics = read.table(nome_file_output_dynamics)
    sel_MS = seq(1, nrow(output_dynamics), numero_classi)
    sel_ME = seq(2, nrow(output_dynamics), numero_classi)
    sel_MI = seq(3, nrow(output_dynamics), numero_classi)
    sel_BS = seq(4, nrow(output_dynamics), numero_classi)
    sel_BE = seq(5, nrow(output_dynamics), numero_classi)
    sel_BI = seq(6, nrow(output_dynamics), numero_classi)
    sel_BR = seq(7, nrow(output_dynamics), numero_classi)
    sel_BS2 = seq(8, nrow(output_dynamics), numero_classi)
    sel_BE2 = seq(9, nrow(output_dynamics), numero_classi)
    sel_BI2 = seq(10, nrow(output_dynamics), numero_classi)
    sel_BR2 = seq(11, nrow(output_dynamics), numero_classi)
    MS = output_dynamics[sel_MS, ]
    ME = output_dynamics[sel_ME, ]
    MI = output_dynamics[sel_MI, ]
    BS = output_dynamics[sel_BS, ]
    BE = output_dynamics[sel_BE, ]
    BI = output_dynamics[sel_BI, ]
    BR = output_dynamics[sel_BR, ]
    BS2 = output_dynamics[sel_BS2, ]
    BE2 = output_dynamics[sel_BE2, ]
    BI2 = output_dynamics[sel_BI2, ]
    BR2 = output_dynamics[sel_BR2, ]
    mosquito_population = MS + ME + MI
    mosquito_prevalence = (MI/mosquito_population) * 
      100
    all_bird_population = BS + BE + BI + BR + BS2 + BE2 + 
      BI2 + BR2
    all_bird_prevalence = ((BI + BI2)/(BS + BE + BI + 
                                         BR + BS2 + BE2 + BI2 + BR2)) * 100
    all_bird_seroprevalence = ((BR + BR2)/(BS + BE + 
                                             BI + BR + BS2 + BE2 + BI2 + BR2)) * 100
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) * 
      100
    seroprev_known_bird_population = (BR/known_bird_population) * 
      100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) * 
      100
    seroprevalence_avian_community = (BR2/avian_community) * 
      100
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "prevalence (%)"
      colore = "violet"
    }
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of moquitoes"
      colore = "purple"
    }
    if (what_plot == "infectious_mosquitoes") {
      pop = MI
      ytext = "Number of I-moquitoes"
      colore = "violet"
    }
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "prevalence (%)"
      colore = "darkred"
    }
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "Number of birds"
      colore = "darkblue"
    }
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "prevalence (%)"
      colore = "darkgreen"
    }
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of birds"
      colore = "blue"
    }
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "prevalence (%)"
      colore = "red"
    }
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "prevalence (%)"
      colore = "green"
    }
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of birds"
      colore = "blue"
    }
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "prevalence (%)"
      colore = "red"
    }
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "prevalence (%)"
      colore = "green"
    }
    qmax_pop = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop = apply(pop, MARGIN = 2, mean)
    qmax_pop[which(is.na(qmax_pop))] = 0
    qmin_pop[which(is.na(qmin_pop))] = 0
    mean_pop[which(is.na(mean_pop))] = 0
    ymax = max(qmax_pop)
    xmax = length(qmax_pop)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = paste(what_plot, 
                                                                                                           anno, quale_cluster))
    lines(mean_pop, lwd = 3, col = colore)
    poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
                     y = c(qmin_pop, rev(qmax_pop)))
    polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
            border = NA)
    axis(1, at = c(1, 1 + 31, 1 + 31 + 30, 1 + 31 + 30 + 
                     31, 1 + 31 + 30 + 31 + 31, 1 + 31 + 30 + 31 + 
                     31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                          "Aug", "Sept"), cex.axis = cex_axes, las = 2)
    scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                            T, F)
    axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
         labels = format(seq(0, ymax, length.out = 4), 
                         digits = 2, scientific = scientific_num), cex.axis = cex_axes)
    mtext(ytext, side = 2, line = 3.5, cex = cex_axes + 
            0.3)
  }
  #dev.off()
}


####dinamiche di popolazione GAZZE####
#quante sono le gazze rispetto alla popolazione totale di uccelli stimata??
anno_inizio = 2016
anno_fine = 2018
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Gazze/Simulazioni/"
FileDynName = "dynamics_2spec_Gazze_"
FolderPlotOut = "Output_WNV/Gazze/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Gazze_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "magpies"
quale_specie_2 = "Avian community"
cex_main = 2
cex_axes = 2
cex_lab_axes = 2
margins_plot = c(7, 7, 4, 2)
what_plot = "MI" #known_bird_pop_rate #avian_community #known_bird_population
to_plot_on_file = F
settimane = scan(vettore_date_catture)


for (anno in anno_inizio:anno_fine) {
  print(anno)
  nome_file_plot = paste0(FolderPlotOut, PlotName, what_plot, 
                          anno, ".jpg")
  if (to_plot_on_file) 
    jpeg(nome_file_plot, height = 1 * 1000, width = 1 * 
           1400, res = 200)
  par(mfrow = c(1, 1), mar = margins_plot, cex.main = cex_main, 
      cex.axis = cex_axes)
  numero_classi = 11
  for (quale_cluster in 1:numero_cluster) {
    if (con_cosa_inizio == 0) 
      nome_file_output_dynamics = paste0(FolderSimu, 
                                         FileDynName, "B_", anno, "_", quale_cluster, 
                                         ".txt")
    if (con_cosa_inizio == 1) 
      nome_file_output_dynamics = paste0(FolderSimu, 
                                         FileDynName, "M_", anno, "_", quale_cluster, 
                                         ".txt")
    output_dynamics = read.table(nome_file_output_dynamics)
    sel_MS = seq(1, nrow(output_dynamics), numero_classi)
    sel_ME = seq(2, nrow(output_dynamics), numero_classi)
    sel_MI = seq(3, nrow(output_dynamics), numero_classi)
    sel_BS = seq(4, nrow(output_dynamics), numero_classi)
    sel_BE = seq(5, nrow(output_dynamics), numero_classi)
    sel_BI = seq(6, nrow(output_dynamics), numero_classi)
    sel_BR = seq(7, nrow(output_dynamics), numero_classi)
    sel_BS2 = seq(8, nrow(output_dynamics), numero_classi)
    sel_BE2 = seq(9, nrow(output_dynamics), numero_classi)
    sel_BI2 = seq(10, nrow(output_dynamics), numero_classi)
    sel_BR2 = seq(11, nrow(output_dynamics), numero_classi)
    MS = output_dynamics[sel_MS, ]
    ME = output_dynamics[sel_ME, ]
    MI = output_dynamics[sel_MI, ]
    BS = output_dynamics[sel_BS, ]
    BE = output_dynamics[sel_BE, ]
    BI = output_dynamics[sel_BI, ]
    BR = output_dynamics[sel_BR, ]
    BS2 = output_dynamics[sel_BS2, ]
    BE2 = output_dynamics[sel_BE2, ]
    BI2 = output_dynamics[sel_BI2, ]
    BR2 = output_dynamics[sel_BR2, ]
    mosquito_population = MS + ME + MI
    mosquito_prevalence = (MI/mosquito_population) *100
    all_bird_population = BS + BE + BI + BR + BS2 + BE2 + BI2 + BR2
    all_bird_prevalence = ((BI + BI2)/(BS + BE + BI + 
                                         BR + BS2 + BE2 + BI2 + BR2)) * 100
    all_bird_seroprevalence = ((BR + BR2)/(BS + BE + 
                                             BI + BR + BS2 + BE2 + BI2 + BR2)) * 100
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "blue"
    }
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of birds"
      colore = "blue"
    }
    if (what_plot == "BI2") {
      pop = BI2
      ytext = "Number of I birds AC"
      colore = "blue"
    }
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of magpies"
      colore = "blue"
    }
    if (what_plot == "BI") {
      pop = BI
      ytext = "Number of I magpies"
      colore = "blue"
    }
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", 
                    quale_specie)
      colore = "black"
    }
    
    qmax_pop = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop = apply(pop, MARGIN = 2, mean)
    qmax_pop[which(is.na(qmax_pop))] = 0
    qmin_pop[which(is.na(qmin_pop))] = 0
    mean_pop[which(is.na(mean_pop))] = 0
    ymax =max(mean_pop) #max(qmax_pop)
    xmax = length(qmax_pop)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = anno)
    lines(mean_pop, lwd = 3, col = colore)
    #poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
    #                y = c(qmin_pop, rev(qmax_pop)))
    #polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
    #        border = NA)
    axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                     31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                     31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                          "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
    scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                            T, F)
    axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
         labels = format(seq(0, ymax, length.out = 4), 
                         digits = 2, scientific = scientific_num), cex.axis = cex_axes)
    mtext(ytext, side = 2, line = 5, cex = cex_axes)
  }
  # dev.off()
}



####dinamiche di popolazione GERMANI####
#quanti sono i germani rispetto alla popolazione totale di uccelli stimata??
anno_inizio = 2016
anno_fine = 2018
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Germani/Simulazioni/"
FileDynName = "dynamics_2spec_Germani_"
FolderPlotOut = "Output_WNV/Germani/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Germani_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "mallards"
quale_specie_2 = "Avian community"
cex_main = 2
cex_axes = 2
cex_lab_axes = 2
margins_plot = c(7, 7, 4, 2)
what_plot = "known_bird_population" #avian_community #known_bird_population #known_bird_pop_rate
to_plot_on_file = F
settimane = scan(vettore_date_catture)


for (anno in anno_inizio:anno_fine) {
  print(anno)
  nome_file_plot = paste0(FolderPlotOut, PlotName, what_plot, 
                          anno, ".jpg")
  if (to_plot_on_file) 
    jpeg(nome_file_plot, height = 1 * 1000, width = 1 * 
           1400, res = 200)
  par(mfrow = c(1, 1), mar = margins_plot, cex.main = cex_main, 
      cex.axis = cex_axes)
  numero_classi = 11
  for (quale_cluster in 1:numero_cluster) {
    if (con_cosa_inizio == 0) 
      nome_file_output_dynamics = paste0(FolderSimu, 
                                         FileDynName, "B_", anno, "_", quale_cluster, 
                                         ".txt")
    if (con_cosa_inizio == 1) 
      nome_file_output_dynamics = paste0(FolderSimu, 
                                         FileDynName, "M_", anno, "_", quale_cluster, 
                                         ".txt")
    output_dynamics = read.table(nome_file_output_dynamics)
    sel_MS = seq(1, nrow(output_dynamics), numero_classi)
    sel_ME = seq(2, nrow(output_dynamics), numero_classi)
    sel_MI = seq(3, nrow(output_dynamics), numero_classi)
    sel_BS = seq(4, nrow(output_dynamics), numero_classi)
    sel_BE = seq(5, nrow(output_dynamics), numero_classi)
    sel_BI = seq(6, nrow(output_dynamics), numero_classi)
    sel_BR = seq(7, nrow(output_dynamics), numero_classi)
    sel_BS2 = seq(8, nrow(output_dynamics), numero_classi)
    sel_BE2 = seq(9, nrow(output_dynamics), numero_classi)
    sel_BI2 = seq(10, nrow(output_dynamics), numero_classi)
    sel_BR2 = seq(11, nrow(output_dynamics), numero_classi)
    MS = output_dynamics[sel_MS, ]
    ME = output_dynamics[sel_ME, ]
    MI = output_dynamics[sel_MI, ]
    BS = output_dynamics[sel_BS, ]
    BE = output_dynamics[sel_BE, ]
    BI = output_dynamics[sel_BI, ]
    BR = output_dynamics[sel_BR, ]
    BS2 = output_dynamics[sel_BS2, ]
    BE2 = output_dynamics[sel_BE2, ]
    BI2 = output_dynamics[sel_BI2, ]
    BR2 = output_dynamics[sel_BR2, ]
    mosquito_population = MS + ME + MI
    mosquito_prevalence = (MI/mosquito_population) *100
    all_bird_population = BS + BE + BI + BR + BS2 + BE2 + BI2 + BR2
    all_bird_prevalence = ((BI + BI2)/(BS + BE + BI + 
                                         BR + BS2 + BE2 + BI2 + BR2)) * 100
    all_bird_seroprevalence = ((BR + BR2)/(BS + BE + 
                                             BI + BR + BS2 + BE2 + BI2 + BR2)) * 100
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of birds"
      colore = "blue"
    }
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of mallards"
      colore = "blue"
    }
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", 
                    quale_specie)
      colore = "black"
    }
    
    qmax_pop = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop = apply(pop, MARGIN = 2, mean)
    qmax_pop[which(is.na(qmax_pop))] = 0
    qmin_pop[which(is.na(qmin_pop))] = 0
    mean_pop[which(is.na(mean_pop))] = 0
    ymax = max(mean_pop) #max(qmax_pop)
    xmax = length(qmax_pop)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = anno)
    lines(mean_pop, lwd = 3, col = colore)
    #poligono = cbind(x = c(1:length(mean_pop), length(mean_pop):1), 
    #                y = c(qmin_pop, rev(qmax_pop)))
    #polygon(poligono, col = adjustcolor(colore, alpha = 0.2), 
    #        border = NA)
    axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                     31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                     31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                          "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
    scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                            T, F)
    axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
         labels = format(seq(0, ymax, length.out = 4), 
                         digits = 2, scientific = scientific_num), cex.axis = cex_axes)
    mtext(ytext, side = 2, line = 5, cex = cex_axes)
  }
  # dev.off()
}


#####boxplot biting rate MERLI####
#2016
anno_inizio = 2016
anno_fine = 2016
numero_cluster = 1
con_cosa_inizio = 1

OutLoc = "Output_WNV/Merli/MCMC"
FileAllParmsName = "parametri_2spec_Merli_"  #CAMBIA NOME A SECONDA DELLA SPECIE
FileParmsPerSimuName = "per_simulazione_2spec_Merli_"

nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                   "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                   "recB_ca")
nomi_parametri = c("b1_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2016 = read.table(nome_file_parametri)
output_mcmc_2016 = output_mcmc_2016[,c(7,13)]
burnin = nrow(output_mcmc_2016) * 0.1
if (ncol(output_mcmc_2016) <= 1) 
  output_mcmc_2016 = matrix(0, ncol = length(nomi_parametri) + 
                         1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                    output_mcmc_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_2016)) if (quantile(tab_2016[, j], probs = 0.975) > 
                             ymax) 
    ymax = quantile(tab_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_2016)) if (quantile(tab_2016[, j], probs = 0.025) < 
                             ymin) 
    ymin = quantile(tab_2016[, j], probs = 0.025)
}

#2017
anno_inizio = 2017
anno_fine = 2017
numero_cluster = 1
con_cosa_inizio = 1

OutLoc = "Output_WNV/Merli/MCMC"
FileAllParmsName = "parametri_2spec_Merli_"  #CAMBIA NOME A SECONDA DELLA SPECIE
FileParmsPerSimuName = "per_simulazione_2spec_Merli_"

nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                   "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                   "recB_ca")
nomi_parametri = c("b1_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2017 = read.table(nome_file_parametri)
output_mcmc_2017 = output_mcmc_2017[,c(7,13)]
burnin = nrow(output_mcmc_2017) * 0.1
if (ncol(output_mcmc_2017) <= 1) 
  output_mcmc_2017 = matrix(0, ncol = length(nomi_parametri) + 
                         1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                    output_mcmc_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")

for (i in 1:length(parametri_stimati)) {
  tab_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_2017)) if (quantile(tab_2017[, j], probs = 0.975) > 
                                  ymax) 
    ymax = quantile(tab_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_2017)) if (quantile(tab_2017[, j], probs = 0.025) < 
                                  ymin) 
    ymin = quantile(tab_2017[, j], probs = 0.025)
}

#2018
anno_inizio = 2018
anno_fine = 2018
numero_cluster = 1
con_cosa_inizio = 1

OutLoc = "Output_WNV/Merli/MCMC"
FileAllParmsName = "parametri_2spec_Merli_"  #CAMBIA NOME A SECONDA DELLA SPECIE
FileParmsPerSimuName = "per_simulazione_2spec_Merli_"

nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                   "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                   "recB_ca")
nomi_parametri = c("b1_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2018 = read.table(nome_file_parametri)
output_mcmc_2018 = output_mcmc_2018[,c(7,13)]
burnin = nrow(output_mcmc_2018) * 0.1
if (ncol(output_mcmc_2018) <= 1) 
  output_mcmc_2018= matrix(0, ncol = length(nomi_parametri) + 
                         1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                    output_mcmc_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")

for (i in 1:length(parametri_stimati)) {
  tab_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_2018)) if (quantile(tab_2018[, j], probs = 0.975) > 
                                  ymax) 
    ymax = quantile(tab_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_2018)) if (quantile(tab_2018[, j], probs = 0.025) < 
                                  ymin) 
    ymin = quantile(tab_2018[, j], probs = 0.025)
}




biting_rate <- as.data.frame(cbind(tab_2016, tab_2017, tab_2018))
colnames(biting_rate) <- c(2016,2017,2018)

par(cex.axis = 2, cex.lab = 2, cex.main = 2, bty = "l")

boxplot(biting_rate, ylim= c(0.3,0.88), bty = "l", ylab = "Biting rate",
        xlab = "years", main = "Biting rate", col = "grey")
abline(h = 0.35, lwd = 2, col = "red", lty = 2)
legend(2.7,0.9,
       legend = "avian community",
       lwd = 2, lty = F,
       fill= "grey",
       bty = "n",
       cex = 1.1)
legend(2.7,0.85,
       legend = "blackbirds",
       col = c("red"), lwd = 2, lty=2,
       bty = "n",
       cex = 1.1)



#####boxplot biting rate GAZZE####
#2016
anno_inizio = 2016
anno_fine = 2016
numero_cluster = 1
con_cosa_inizio = 1

OutLoc = "Output_WNV/Gazze/MCMC"
FileAllParmsName = "parametri_2spec_Gazze_"  #CAMBIA NOME A SECONDA DELLA SPECIE
FileParmsPerSimuName = "per_simulazione_2spec_Gazze_"

nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                   "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                   "recB_ca")
nomi_parametri = c("b1_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2016 = read.table(nome_file_parametri)
output_mcmc_2016 = output_mcmc_2016[,c(7,13)]
burnin = nrow(output_mcmc_2016) * 0.1
if (ncol(output_mcmc_2016) <= 1) 
  output_mcmc_2016 = matrix(0, ncol = length(nomi_parametri) + 
                              1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                         output_mcmc_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_2016)) if (quantile(tab_2016[, j], probs = 0.975) > 
                                  ymax) 
    ymax = quantile(tab_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_2016)) if (quantile(tab_2016[, j], probs = 0.025) < 
                                  ymin) 
    ymin = quantile(tab_2016[, j], probs = 0.025)
}

#2017
anno_inizio = 2017
anno_fine = 2017
numero_cluster = 1
con_cosa_inizio = 1

OutLoc = "Output_WNV/Gazze/MCMC"
FileAllParmsName = "parametri_2spec_Gazze_"  #CAMBIA NOME A SECONDA DELLA SPECIE
FileParmsPerSimuName = "per_simulazione_2spec_Gazze_"

nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                   "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                   "recB_ca")
nomi_parametri = c("b1_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2017 = read.table(nome_file_parametri)
output_mcmc_2017 = output_mcmc_2017[,c(7,13)]
burnin = nrow(output_mcmc_2017) * 0.1
if (ncol(output_mcmc_2017) <= 1) 
  output_mcmc_2017 = matrix(0, ncol = length(nomi_parametri) + 
                              1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                         output_mcmc_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")

for (i in 1:length(parametri_stimati)) {
  tab_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_2017)) if (quantile(tab_2017[, j], probs = 0.975) > 
                                  ymax) 
    ymax = quantile(tab_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_2017)) if (quantile(tab_2017[, j], probs = 0.025) < 
                                  ymin) 
    ymin = quantile(tab_2017[, j], probs = 0.025)
}

#2018
anno_inizio = 2018
anno_fine = 2018
numero_cluster = 1
con_cosa_inizio = 1

OutLoc = "Output_WNV/Gazze/MCMC"
FileAllParmsName = "parametri_2spec_Gazze_"  #CAMBIA NOME A SECONDA DELLA SPECIE
FileParmsPerSimuName = "per_simulazione_2spec_Gazze_"

nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                   "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                   "recB_ca")
nomi_parametri = c("b1_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2018 = read.table(nome_file_parametri)
output_mcmc_2018 = output_mcmc_2018[,c(7,13)]
burnin = nrow(output_mcmc_2018) * 0.1
if (ncol(output_mcmc_2018) <= 1) 
  output_mcmc_2018= matrix(0, ncol = length(nomi_parametri) + 
                             1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                         output_mcmc_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")

for (i in 1:length(parametri_stimati)) {
  tab_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_2018)) if (quantile(tab_2018[, j], probs = 0.975) > 
                                  ymax) 
    ymax = quantile(tab_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_2018)) if (quantile(tab_2018[, j], probs = 0.025) < 
                                  ymin) 
    ymin = quantile(tab_2018[, j], probs = 0.025)
}




biting_rate <- as.data.frame(cbind(tab_2016, tab_2017, tab_2018))
colnames(biting_rate) <- c(2016,2017,2018)

par(cex.axis = 2, cex.lab = 2, cex.main = 2, bty = "l")

boxplot(biting_rate, ylim= c(0,0.6), bty = "l", ylab = "Biting rate",
        xlab = "years", main = "Biting rate", col = "grey")
abline(h = 0.1, lwd = 2, col = "red", lty = 2)
legend(2.7,0.3,
       legend = "avian community",
       lwd = 2, lty = F,
       fill= "grey",
       bty = "n",
       cex = 1.1)
legend(2.7,0.25,
       legend = "magpies",
       col = c("red"), lwd = 2, lty=2,
       bty = "n",
       cex = 1.1)

#####boxplot biting rate GERMANI####
#2016
anno_inizio = 2016
anno_fine = 2016
numero_cluster = 1
con_cosa_inizio = 1

OutLoc = "Output_WNV/Germani/MCMC"
FileAllParmsName = "parametri_2spec_Germani_"  #CAMBIA NOME A SECONDA DELLA SPECIE
FileParmsPerSimuName = "per_simulazione_2spec_Germani_"

nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                   "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                   "recB_ca")
nomi_parametri = c("b1_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2016 = read.table(nome_file_parametri)
output_mcmc_2016 = output_mcmc_2016[,c(7,13)]
burnin = nrow(output_mcmc_2016) * 0.1
if (ncol(output_mcmc_2016) <= 1) 
  output_mcmc_2016 = matrix(0, ncol = length(nomi_parametri) + 
                              1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                         output_mcmc_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_2016)) if (quantile(tab_2016[, j], probs = 0.975) > 
                                  ymax) 
    ymax = quantile(tab_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_2016)) if (quantile(tab_2016[, j], probs = 0.025) < 
                                  ymin) 
    ymin = quantile(tab_2016[, j], probs = 0.025)
}

#2017
anno_inizio = 2017
anno_fine = 2017
numero_cluster = 1
con_cosa_inizio = 1

OutLoc = "Output_WNV/Germani/MCMC"
FileAllParmsName = "parametri_2spec_Germani_"  #CAMBIA NOME A SECONDA DELLA SPECIE
FileParmsPerSimuName = "per_simulazione_2spec_Germani_"

nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                   "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                   "recB_ca")
nomi_parametri = c("b1_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2017 = read.table(nome_file_parametri)
output_mcmc_2017 = output_mcmc_2017[,c(7,13)]
burnin = nrow(output_mcmc_2017) * 0.1
if (ncol(output_mcmc_2017) <= 1) 
  output_mcmc_2017 = matrix(0, ncol = length(nomi_parametri) + 
                              1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                         output_mcmc_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")

for (i in 1:length(parametri_stimati)) {
  tab_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_2017)) if (quantile(tab_2017[, j], probs = 0.975) > 
                                  ymax) 
    ymax = quantile(tab_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_2017)) if (quantile(tab_2017[, j], probs = 0.025) < 
                                  ymin) 
    ymin = quantile(tab_2017[, j], probs = 0.025)
}

#2018
anno_inizio = 2018
anno_fine = 2018
numero_cluster = 1
con_cosa_inizio = 1

OutLoc = "Output_WNV/Germani/MCMC"
FileAllParmsName = "parametri_2spec_Germani_"  #CAMBIA NOME A SECONDA DELLA SPECIE
FileParmsPerSimuName = "per_simulazione_2spec_Germani_"

nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                   "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                   "recB_ca")
nomi_parametri = c("b1_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2018 = read.table(nome_file_parametri)
output_mcmc_2018 = output_mcmc_2018[,c(7,13)]
burnin = nrow(output_mcmc_2018) * 0.1
if (ncol(output_mcmc_2018) <= 1) 
  output_mcmc_2018= matrix(0, ncol = length(nomi_parametri) + 
                             1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                         output_mcmc_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")

for (i in 1:length(parametri_stimati)) {
  tab_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_2018)) if (quantile(tab_2018[, j], probs = 0.975) > 
                                  ymax) 
    ymax = quantile(tab_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_2018)) if (quantile(tab_2018[, j], probs = 0.025) < 
                                  ymin) 
    ymin = quantile(tab_2018[, j], probs = 0.025)
}




biting_rate <- as.data.frame(cbind(tab_2016, tab_2017, tab_2018))
colnames(biting_rate) <- c(2016,2017,2018)

par(cex.axis = 2, cex.lab = 2, cex.main = 2, bty = "l")

boxplot(biting_rate, ylim= c(0,0.9), bty = "l", ylab = "Biting rate",
        xlab = "years", main = "Biting rate", col = "grey")
abline(h = 0.01, lwd = 2, col = "red", lty = 2)
legend(2.7,0.3,
       legend = "avian community",
       lwd = 2, lty = F,
       fill= "grey",
       bty = "n",
       cex = 1.1)
legend(2.7,0.25,
       legend = "mallards",
       col = c("red"), lwd = 2, lty=2,
       bty = "n",
       cex = 1.1)


