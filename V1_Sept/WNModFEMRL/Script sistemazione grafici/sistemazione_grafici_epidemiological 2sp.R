##################################### 05_Epidemiological_2sp ########################################
setwd("/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/WNModFEMRL")
################################MODEL FIT########################################
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

cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8
ymax=17

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

settimane = scan(vettore_date_catture)
colori = rainbow(n = numero_cluster)
quanti_dentro95 = 0
quante_osservazioni = 0

for (anno in anno_inizio:anno_fine) {
  print(anno)
  nome_file_plot = paste0(FolderPlotOut, PlotName, anno, 
                          ".pdf")

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
                                                        wd = 0.2, col = "#5E4FA2") #colori[quale_cluster])
    points(1:xmax, numero_pool_positivi[quale_cluster, 
                                        quando_ho_pool][1:xmax], pch = 19, col = "#FDAE61", 
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
            col = c("#FDAE61","#5E4FA2"), lwd = 2, lty = c(0,0),
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

cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8
ymax=17

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")


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
                                                        wd = 0.2, col = "#5E4FA2") #colori[quale_cluster])
    points(1:xmax, numero_pool_positivi[quale_cluster, 
                                        quando_ho_pool][1:xmax], pch = 19, col = "#FDAE61", 
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
            col = c("#FDAE61","#5E4FA2"), lwd = 2, lty = c(0,0),
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

cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8
ymax=17

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

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
                                                        wd = 0.2, col = "#5E4FA2") #colori[quale_cluster])
    points(1:xmax, numero_pool_positivi[quale_cluster, 
                                        quando_ho_pool][1:xmax], pch = 19, col = "#FDAE61", 
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
            col = c("#FDAE61","#5E4FA2"), lwd = 2, lty = c(0,0),
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



#################################BIRTH PULSE######################################
#### birth pulse plot MERLI ####
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8


par(mfrow = c(1, 1), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

#birth pulse della comunità aviare----
parms = "Output_WNV/Merli/MCMC/parametri_2spec_Merli_M_2018_1.txt" 
tmin = 91
tmax = 274
quante_specie = 2
anno = 2017
par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

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
  mean_black_ac_2018 = apply(birth_pulse, MARGIN = 1, function(a) {
    mean(a, na.rm = T)
  })
  qmin_black_ac_2018 = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.025, na.rm = T)
  })
  qmax_black_ac_2018 = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.975, na.rm = T)
  })
  ymax = 0.1 #max(qmax, qmin, mean)
  plot(0, col = "white", xlim = c(0, 366), ylim = c(0, ymax), 
       xlab = "", ylab = "", main = paste("Birth pulse blackbirds - AC",anno), cex.main = cex_main, axes = F)
  abline(v = c(tmin, tmax), lwd = 2, col = "#FDAE61")
  lines(mean_black_ac_2017, lwd = 3, col = "black")
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
                                                                           "Aug", "Sept", "Oct", "Nov", "Dec", "Gen"),
       cex.axis = 1.3, las = 2)
  
  axis(2, at = seq(0, ymax, 0.02), las = 2, 
       labels = format(seq(0, ymax, 0.02), 
                       digits = 2, scientific = F), cex.axis = 1.3)
  mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)
  
  #legend('topleft', legend = leg , cex = 0.8,  lwd = 2, col = colori, bty = 'n')
  
  
  #legend(275,0.1, legend = "AC-blackbirds", lty = 1, lwd = 3.5, col = "black", 
   #      cex = 1.2, box.lwd = F)
  

}

##plot birth pulse merli AC 3years
plot(0, col = "white", xlim = c(0, 366), ylim = c(0, ymax), 
     xlab = "", ylab = "", main = paste("Birth pulse blackbirds - AC"), cex.main = cex_main, axes = F)
abline(v = c(tmin, tmax), lwd = 2, col = "black")
lines(mean_black_ac_2016, lwd = 3, col = "#9E0142")
lines(mean_black_ac_2017, lwd = 3, col = "#F46D43")
lines(mean_black_ac_2018, lwd = 3, col = "#66C2A5")
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
                                                                         "Aug", "Sept", "Oct", "Nov", "Dec", "Gen"),
     cex.axis = 1.3, las = 2)

axis(2, at = seq(0, ymax, 0.02), las = 2, 
     labels = format(seq(0, ymax, 0.02), 
                     digits = 2, scientific = F), cex.axis = 1.3)
mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)

legend('topright', legend=c("2016","2017","2018"), lty = 1, lwd = 2.5,
       col = c("#9E0142","#F46D43","#66C2A5"), box.lwd = F)


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
  mean_black = apply(birth_pulse, MARGIN = 1, function(a) {
    mean(a, na.rm = T)
  })
  qmin_black = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.025, na.rm = T)
  })
  qmax_black = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.975, na.rm = T)
  })
  ymax = 0.1 #max(qmax, qmin, mean)
  plot(0, col = "white", xlim = c(0, 366), ylim = c(0, ymax), xlab = "", ylab = "", axes = F,
       main ="Birth pulse blackbirds")
  abline(v = c(tmin, tmax), lwd = 2, col = "#FDAE61")
  lines(mean_black, lwd = 3, col = "black")
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
  
  axis(2, at = seq(0, ymax, 0.02), las = 2, 
       labels = format(seq(0, ymax, 0.02), 
                       digits = 2, scientific = F), cex.axis = 1.3)
  mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)
  
  #legend(275,0.1, legend = "Blackbirds", lty = 1, lwd = 3, col = "black", 
   #      cex = 1.4, box.lwd = F)
}

## plot birth pulse AC_SN per anno
plot(0, col = "white", xlim = c(0, 366), ylim = c(0, ymax), 
     xlab = "", ylab = "", main = paste("Birth pulse 2018"), cex.main = cex_main, axes = F)
abline(v = c(tmin, tmax), lwd = 2, col = "black")
lines(mean_black_ac_2018, lwd = 3, col = "#D53E4F")
lines(mean_black, lwd = 3, col = "#9E0142")

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
                                                                         "Aug", "Sept", "Oct", "Nov", "Dec", "Gen"),
     cex.axis = 1.3, las = 2)

axis(2, at = seq(0, ymax, 0.02), las = 2, 
     labels = format(seq(0, ymax, 0.02), 
                     digits = 2, scientific = F), cex.axis = 1.3)
mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)

legend(1,0.101, legend=c("blackbirds","avian community"), lty = 1, lwd = 2.5,
       col = c("#9E0142","#D53E4F"), box.lwd = F)





#### birth pulse plot GAZZE ####
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8


par(mfrow = c(1, 1), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

#birth pulse della comunità aviare----
parms = "Output_WNV/Gazze/MCMC/parametri_2spec_Gazze_M_2018_1.txt" 
tmin = 91
tmax = 274
quante_specie = 2
anno = 2018
par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

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
  mean_mag_ac_2018 = apply(birth_pulse, MARGIN = 1, function(a) {
    mean(a, na.rm = T)
  })
  qmin_mag_ac_2018 = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.025, na.rm = T)
  })
  qmax_mag_ac_2018 = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.975, na.rm = T)
  })
  ymax = 0.1 #max(qmax, qmin, mean)
  plot(0, col = "white", xlim = c(0, 366), ylim = c(0, ymax), 
       xlab = "", ylab = "", main = paste("Birth pulse magpies - AC",anno), cex.main = cex_main, axes = F)
  abline(v = c(tmin, tmax), lwd = 2, col = "black")
  lines(mean_mag_ac_2018, lwd = 3, col = "#FDAE61")
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
                                                                           "Aug", "Sept", "Oct", "Nov", "Dec", "Gen"),
       cex.axis = 1.3, las = 2)
  
  axis(2, at = seq(0, ymax, 0.02), las = 2, 
       labels = format(seq(0, ymax, 0.02), 
                       digits = 2, scientific = F), cex.axis = 1.3)
  mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)
  
  #legend('topleft', legend = leg , cex = 0.8,  lwd = 2, col = colori, bty = 'n')
  
  
  #legend(275,0.1, legend = "AC-magpies", lty = 1, lwd = 3.5, col = "mag", 
  #      cex = 1.2, box.lwd = F)
  
  
}

##plot birth pulse Gazze AC 3years
plot(0, col = "white", xlim = c(0, 366), ylim = c(0, ymax), 
     xlab = "", ylab = "", main = paste("Birth pulse magpies - AC"), cex.main = cex_main, axes = F)
abline(v = c(tmin, tmax), lwd = 2, col = "black")
lines(mean_mag_ac_2016, lwd = 3, col = "#9E0142")
lines(mean_mag_ac_2017, lwd = 3, col = "#F46D43")
lines(mean_mag_ac_2018, lwd = 3, col = "#66C2A5")
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
                                                                         "Aug", "Sept", "Oct", "Nov", "Dec", "Gen"),
     cex.axis = 1.3, las = 2)

axis(2, at = seq(0, ymax, 0.02), las = 2, 
     labels = format(seq(0, ymax, 0.02), 
                     digits = 2, scientific = F), cex.axis = 1.3)
mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)

legend('topright', legend=c("2016","2017","2018"), lty = 1, lwd = 2.5,
       col = c("#9E0142","#F46D43","#66C2A5"), box.lwd = F)


#birth pulse Gazze ----
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
  mean_mag = apply(birth_pulse, MARGIN = 1, function(a) {
    mean(a, na.rm = T)
  })
  qmin_mag = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.025, na.rm = T)
  })
  qmax_mag = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.975, na.rm = T)
  })
  ymax = 0.1 #max(qmax, qmin, mean)
  plot(0, col = "white", xlim = c(0, 366), ylim = c(0, ymax), xlab = "", ylab = "", axes = F,
       main ="Birth pulse magpies")
  abline(v = c(tmin, tmax), lwd = 2, col = "black")
  lines(mean_mag, lwd = 3, col = "#F46D43")
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
  
  axis(2, at = seq(0, ymax, 0.02), las = 2, 
       labels = format(seq(0, ymax, 0.02), 
                       digits = 2, scientific = F), cex.axis = 1.3)
  mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)
  
  #legend(275,0.1, legend = "magpies", lty = 1, lwd = 3, col = "mag", 
  #      cex = 1.4, box.lwd = F)
}

## plot birth pulse AC_SN per anno
plot(0, col = "white", xlim = c(0, 366), ylim = c(0, ymax), 
     xlab = "", ylab = "", main = paste("Birth pulse 2018"), cex.main = cex_main, axes = F)
abline(v = c(tmin, tmax), lwd = 2, col = "black")
lines(mean_mag_ac_2018, lwd = 3, col = "#FDAE61")
lines(mean_mag, lwd = 3, col = "#F46D43")

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
                                                                         "Aug", "Sept", "Oct", "Nov", "Dec", "Gen"),
     cex.axis = 1.3, las = 2)

axis(2, at = seq(0, ymax, 0.02), las = 2, 
     labels = format(seq(0, ymax, 0.02), 
                     digits = 2, scientific = F), cex.axis = 1.3)
mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)

legend(1,0.101, legend=c("magpies","avian community"), lty = 1, lwd = 2.5,
       col = c("#F46D43","#FDAE61"), box.lwd = F)









































#### birth pulse plot GERMANI ####
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8


par(mfrow = c(1, 1), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

#birth pulse della comunità aviare----
parms = "Output_WNV/Germani/MCMC/parametri_2spec_Germani_M_2017_1.txt" 
tmin = 91
tmax = 274
quante_specie = 2
anno = 2017
par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

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
  mean_mall_ac_2017 = apply(birth_pulse, MARGIN = 1, function(a) {
    mean(a, na.rm = T)
  })
  qmin_mall_ac_2017 = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.025, na.rm = T)
  })
  qmax_mall_ac_2017 = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.975, na.rm = T)
  })
  ymax = 0.1 #max(qmax, qmin, mean)
  plot(0, col = "white", xlim = c(0, 366), ylim = c(0, ymax), 
       xlab = "", ylab = "", main = paste("Birth pulse mallards - AC",anno), cex.main = cex_main, axes = F)
  abline(v = c(tmin, tmax), lwd = 2, col = "#FDAE61")
  lines(mean_mall_ac_2017, lwd = 3, col = "black")
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
                                                                           "Aug", "Sept", "Oct", "Nov", "Dec", "Gen"),
       cex.axis = 1.3, las = 2)
  
  axis(2, at = seq(0, ymax, 0.02), las = 2, 
       labels = format(seq(0, ymax, 0.02), 
                       digits = 2, scientific = F), cex.axis = 1.3)
  mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)
  
  #legend('topleft', legend = leg , cex = 0.8,  lwd = 2, col = colori, bty = 'n')
  
  
  #legend(275,0.1, legend = "AC-mallards", lty = 1, lwd = 3.5, col = "mall", 
  #      cex = 1.2, box.lwd = F)
  
  
}

##plot birth pulse Germani AC 3years
plot(0, col = "white", xlim = c(0, 366), ylim = c(0, ymax), 
     xlab = "", ylab = "", main = paste("Birth pulse mallards - AC"), cex.main = cex_main, axes = F)
abline(v = c(tmin, tmax), lwd = 2, col = "black")
lines(mean_mall_ac_2016, lwd = 3, col = "#9E0142")
lines(mean_mall_ac_2017, lwd = 3, col = "#F46D43")
lines(mean_mall_ac_2018, lwd = 3, col = "#66C2A5")
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
                                                                         "Aug", "Sept", "Oct", "Nov", "Dec", "Gen"),
     cex.axis = 1.3, las = 2)

axis(2, at = seq(0, ymax, 0.02), las = 2, 
     labels = format(seq(0, ymax, 0.02), 
                     digits = 2, scientific = F), cex.axis = 1.3)
mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)

legend('topright', legend=c("2016","2017","2018"), lty = 1, lwd = 2.5,
       col = c("#9E0142","#F46D43","#66C2A5"), box.lwd = F)


#birth pulse Germani ----
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
  mean_mall = apply(birth_pulse, MARGIN = 1, function(a) {
    mean(a, na.rm = T)
  })
  qmin_mall = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.025, na.rm = T)
  })
  qmax_mall = apply(birth_pulse, MARGIN = 1, function(a) {
    quantile(a, probs = 0.975, na.rm = T)
  })
  ymax = 0.1 #max(qmax, qmin, mean)
  plot(0, col = "white", xlim = c(0, 366), ylim = c(0, ymax), xlab = "", ylab = "", axes = F,
       main ="Birth pulse mallards")
  abline(v = c(tmin, tmax), lwd = 2, col = "#FDAE61")
  lines(mean_mall, lwd = 3, col = "black")
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
  
  axis(2, at = seq(0, ymax, 0.02), las = 2, 
       labels = format(seq(0, ymax, 0.02), 
                       digits = 2, scientific = F), cex.axis = 1.3)
  mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)
  
  #legend(275,0.1, legend = "mallards", lty = 1, lwd = 3, col = "mall", 
  #      cex = 1.4, box.lwd = F)
}

## plot birth pulse AC_SN per anno
plot(0, col = "white", xlim = c(0, 366), ylim = c(0, ymax), 
     xlab = "", ylab = "", main = paste("Birth pulse 2018"), cex.main = cex_main, axes = F)
abline(v = c(tmin, tmax), lwd = 2, col = "black")
lines(mean_mall_ac_2018, lwd = 3, col = "#ABDDA4")
lines(mean_mall, lwd = 3, col = "#66C2A5")

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
                                                                         "Aug", "Sept", "Oct", "Nov", "Dec", "Gen"),
     cex.axis = 1.3, las = 2)

axis(2, at = seq(0, ymax, 0.02), las = 2, 
     labels = format(seq(0, ymax, 0.02), 
                     digits = 2, scientific = F), cex.axis = 1.3)
mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)

legend(1,0.101, legend=c("mallards","avian community"), lty = 1, lwd = 2.5,
       col = c("#66C2A5","#ABDDA4"), box.lwd = F)


#####plot confronto birth pulse 3 specie
#SN 3 specie
plot(0, col = "white", xlim = c(0, 366), ylim = c(0, ymax), 
     xlab = "", ylab = "", main = paste("Birth pulse three species"), cex.main = cex_main, axes = F)
abline(v = c(tmin, tmax), lwd = 2, col = "black")
lines(mean_black, lwd = 3, col = "#9E0142")
lines(mean_mag, lwd = 3, col = "#F46D43")
lines(mean_mall, lwd = 3, col = "#66C2A5")

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
                                                                         "Aug", "Sept", "Oct", "Nov", "Dec", "Gen"),
     cex.axis = 1.3, las = 2)

axis(2, at = seq(0, ymax, 0.02), las = 2, 
     labels = format(seq(0, ymax, 0.02), 
                     digits = 2, scientific = F), cex.axis = 1.3)
mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)

legend("topright", legend=c("blackbirds","magpies", "mallards"), lty = 1, lwd = 2.5,
       col = c("#9E0142","#F46D43","#66C2A5"), box.lwd = F)


#AC tre specie per anno
plot(0, col = "white", xlim = c(0, 366), ylim = c(0, ymax), 
     xlab = "", ylab = "", main = paste("Birth pulse three species AC 2018"), cex.main = cex_main, axes = F)
abline(v = c(tmin, tmax), lwd = 2, col = "black")
lines(mean_black_ac_2018, lwd = 3, col = "#D53E4F")
lines(mean_mag_ac_2018, lwd = 3, col = "#FDAE61")
lines(mean_mall_ac_2018, lwd = 3, col = "#ABDDA4")

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
                                                                         "Aug", "Sept", "Oct", "Nov", "Dec", "Gen"),
     cex.axis = 1.3, las = 2)

axis(2, at = seq(0, ymax, 0.02), las = 2, 
     labels = format(seq(0, ymax, 0.02), 
                     digits = 2, scientific = F), cex.axis = 1.3)
mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)

legend("topright", legend=c("AC-blackbirds","AC-magpies", "AC-mallards"), lty = 1, lwd = 2.5,
       col = c("#D53E4F","#FDAE61","#ABDDA4"), box.lwd = F)






######################################BOX PLOT####################################
#MERLI----
#biting rate----
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

cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8


par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
boxplot(biting_rate, ylim= c(0.3,0.88), bty = "l", ylab = "",
        xlab = "years", main = "Biting rate", col = "#D53E4F", las = 1)
abline(h = 0.35, lwd = 2, col = "#9E0142", lty = 2)
legend("topright",
       legend = "avian community", fill="#D53E4F", horiz=T, cex=1,box.lwd = F)
legend(2.35,0.85,
       legend = "blackbirds",
       col = c("#9E0142"), lwd = 2, lty=2,
       bty = "n",
       cex = 1)

#initial number of birds B0----
#2016_ca
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
nomi_parametri = c("B0_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_ca_2016 = read.table(nome_file_parametri)
output_mcmc_ca_2016 = output_mcmc_ca_2016[,c(3,13)]
burnin = nrow(output_mcmc_ca_2016) * 0.1
if (ncol(output_mcmc_ca_2016) <= 1) 
  output_mcmc_ca_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                 1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_ca_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                            output_mcmc_ca_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_ca_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_ca_2016)) if (quantile(tab_ca_2016[, j], probs = 0.975) > 
                                     ymax) 
    ymax = quantile(tab_ca_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_ca_2016)) if (quantile(tab_ca_2016[, j], probs = 0.025) < 
                                     ymin) 
    ymin = quantile(tab_ca_2016[, j], probs = 0.025)
}

#2016_sn
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
nomi_parametri = c("B0_sn")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_sn_2016 = read.table(nome_file_parametri)
output_mcmc_sn_2016 = output_mcmc_sn_2016[,c(4,13)]
burnin = nrow(output_mcmc_sn_2016) * 0.1
if (ncol(output_mcmc_sn_2016) <= 1) 
  output_mcmc_sn_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                 1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_sn_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                            output_mcmc_sn_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_sn_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_sn_2016)) if (quantile(tab_sn_2016[, j], probs = 0.975) > 
                                     ymax) 
    ymax = quantile(tab_sn_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_sn_2016)) if (quantile(tab_sn_2016[, j], probs = 0.025) < 
                                     ymin) 
    ymin = quantile(tab_sn_2016[, j], probs = 0.025)
}


#2017_ca
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
nomi_parametri = c("B0_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_ca_2017 = read.table(nome_file_parametri)
output_mcmc_ca_2017 = output_mcmc_ca_2017[,c(3,13)]
burnin = nrow(output_mcmc_ca_2017) * 0.1
if (ncol(output_mcmc_ca_2017) <= 1) 
  output_mcmc_ca_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                 1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_ca_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                            output_mcmc_ca_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_ca_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_ca_2017)) if (quantile(tab_ca_2017[, j], probs = 0.975) > 
                                     ymax) 
    ymax = quantile(tab_ca_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_ca_2017)) if (quantile(tab_ca_2017[, j], probs = 0.025) < 
                                     ymin) 
    ymin = quantile(tab_ca_2017[, j], probs = 0.025)
}

#2017_sn
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
nomi_parametri = c("B0_sn")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_sn_2017 = read.table(nome_file_parametri)
output_mcmc_sn_2017 = output_mcmc_sn_2017[,c(4,13)]
burnin = nrow(output_mcmc_sn_2017) * 0.1
if (ncol(output_mcmc_sn_2017) <= 1) 
  output_mcmc_sn_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                 1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_sn_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                            output_mcmc_sn_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_sn_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_sn_2017)) if (quantile(tab_sn_2017[, j], probs = 0.975) > 
                                     ymax) 
    ymax = quantile(tab_sn_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_sn_2017)) if (quantile(tab_sn_2017[, j], probs = 0.025) < 
                                     ymin) 
    ymin = quantile(tab_sn_2017[, j], probs = 0.025)
}


#2018_ca
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
nomi_parametri = c("B0_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_ca_2018 = read.table(nome_file_parametri)
output_mcmc_ca_2018 = output_mcmc_ca_2018[,c(3,13)]
burnin = nrow(output_mcmc_ca_2018) * 0.1
if (ncol(output_mcmc_ca_2018) <= 1) 
  output_mcmc_ca_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                 1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_ca_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                            output_mcmc_ca_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_ca_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_ca_2018)) if (quantile(tab_ca_2018[, j], probs = 0.975) > 
                                     ymax) 
    ymax = quantile(tab_ca_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_ca_2018)) if (quantile(tab_ca_2018[, j], probs = 0.025) < 
                                     ymin) 
    ymin = quantile(tab_ca_2018[, j], probs = 0.025)
}

#2018_sn
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
nomi_parametri = c("B0_sn")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_sn_2018 = read.table(nome_file_parametri)
output_mcmc_sn_2018 = output_mcmc_sn_2018[,c(4,13)]
burnin = nrow(output_mcmc_sn_2018) * 0.1
if (ncol(output_mcmc_sn_2018) <= 1) 
  output_mcmc_sn_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                 1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_sn_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                            output_mcmc_sn_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_sn_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_sn_2018)) if (quantile(tab_sn_2018[, j], probs = 0.975) > 
                                     ymax) 
    ymax = quantile(tab_sn_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_sn_2018)) if (quantile(tab_sn_2018[, j], probs = 0.025) < 
                                     ymin) 
    ymin = quantile(tab_sn_2018[, j], probs = 0.025)
}


initial_number_of_birds <- as.data.frame(cbind(tab_ca_2016, tab_sn_2016,
                                               tab_ca_2017, tab_sn_2017,
                                               tab_ca_2018, tab_sn_2018))
colnames(initial_number_of_birds) <- c("AC16","B16","AC17","B17","AC18","B18")

cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8


par(mfrow = c(1, numero_cluster), mar = c(5, 5, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

boxplot(initial_number_of_birds, ylim= c(0,100), bty = "l", ylab = "",
        xlab = "years", main = "Initial number of birds", 
        col = c("#D53E4F","#9E0142","#FDAE61","#F46D43","#ABDDA4","#66C2A5"),
        las = 1)

initial_number_of_AC_birds <- as.data.frame(cbind(tab_ca_2016,tab_ca_2017,
                                                  tab_ca_2018))
colnames(initial_number_of_AC_birds) <- c("AC2016","AC2017","AC2018")

boxplot(initial_number_of_AC_birds, ylim= c(0,100), bty = "l", ylab = "",
        xlab = "years", main = "Initial number of birds", 
        col = c("#D53E4F","#FDAE61","#ABDDA4"),
        las = 1)

initial_number_of_SN_birds <- as.data.frame(cbind(tab_sn_2016,tab_sn_2017,
                                                  tab_sn_2018))
colnames(initial_number_of_SN_birds) <- c("B2016","B2017","B2018")

boxplot(initial_number_of_SN_birds, ylim= c(0,100), bty = "l", ylab = "",
        xlab = "years", main = "Initial number of birds", 
        col = c("#9E0142","#F46D43","#66C2A5"),
        las = 1)


#muB----
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
nomi_parametri = c("muB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2016 = read.table(nome_file_parametri)
output_mcmc_2016 = output_mcmc_2016[,c(8,13)]
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
nomi_parametri = c("muB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2017 = read.table(nome_file_parametri)
output_mcmc_2017 = output_mcmc_2017[,c(8,13)]
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
nomi_parametri = c("muB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2018 = read.table(nome_file_parametri)
output_mcmc_2018 = output_mcmc_2018[,c(8,13)]
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




muB <- as.data.frame(cbind(tab_2016, tab_2017, tab_2018))
colnames(muB) <- c(2016,2017,2018)

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
boxplot(muB, ylim= c(0,0.025), bty = "l", ylab = "",
        xlab = "years", main = "number of eggs laid", col = "grey", las = 1)
abline(h = 0.013, lwd = 2, col = "#9E0142", lty = 2)
legend(0.45,0.025,
       legend = "avian community", fill="grey", horiz=T, cex=1,box.lwd = F)
legend(0.45,0.023,
       legend = "blackbirds",
       col = c("#9E0142"), lwd = 2, lty=2,
       bty = "n",
       cex = 1)


#s----
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
nomi_parametri = c("s_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2016 = read.table(nome_file_parametri)
output_mcmc_2016 = output_mcmc_2016[,c(9,13)]
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
nomi_parametri = c("s_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2017 = read.table(nome_file_parametri)
output_mcmc_2017 = output_mcmc_2017[,c(9,13)]
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
nomi_parametri = c("s_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2018 = read.table(nome_file_parametri)
output_mcmc_2018 = output_mcmc_2018[,c(9,13)]
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




s <- as.data.frame(cbind(tab_2016, tab_2017, tab_2018))
colnames(s) <- c(2016,2017,2018)

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

boxplot(s, ylim= c(5,20), bty = "l", ylab = "",
        xlab = "years", main = "birth synchronization", col = "#D53E4F", las = 1)
abline(h = 6, lwd = 2, col = "#9E0142", lty = 2)
#legend(2.7,0.9,
#       legend = "avian community",
#      lwd = 2, lty = F,
#     fill= "grey",
#    bty = "n",
#   cex = 1.1)
#legend(2.7,0.85,
#      legend = "blackbirds",
#     col = c("red"), lwd = 2, lty=2,
#    bty = "n",
#   cex = 1.1)
#phi----
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
nomi_parametri = c("phi_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2016 = read.table(nome_file_parametri)
output_mcmc_2016 = output_mcmc_2016[,c(10,13)]
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
nomi_parametri = c("phi_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2017 = read.table(nome_file_parametri)
output_mcmc_2017 = output_mcmc_2017[,c(10,13)]
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
nomi_parametri = c("phi_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2018 = read.table(nome_file_parametri)
output_mcmc_2018 = output_mcmc_2018[,c(10,13)]
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




phi <- as.data.frame(cbind(tab_2016, tab_2017, tab_2018))
colnames(phi) <- c(2016,2017,2018)

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

boxplot(phi, ylim= c(0.33,0.67), bty = "l", ylab = "",
        xlab = "years", main = "time of birth", col = "#D53E4F", las =1)
abline(h = 0.35, lwd = 2, col = "#9E0142", lty = 2)
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
#niB----
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
nomi_parametri = c("niB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2016 = read.table(nome_file_parametri)
output_mcmc_2016 = output_mcmc_2016[,c(11,13)]
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
nomi_parametri = c("niB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2017 = read.table(nome_file_parametri)
output_mcmc_2017 = output_mcmc_2017[,c(11,13)]
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
nomi_parametri = c("niB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2018 = read.table(nome_file_parametri)
output_mcmc_2018 = output_mcmc_2018[,c(11,13)]
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




niB <- as.data.frame(cbind(tab_2016, tab_2017, tab_2018))
colnames(niB) <- c(2016,2017,2018)

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

boxplot(niB, ylim= c(0.3,1.03), bty = "l", ylab = "",
        xlab = "years", main = "infection rate", col = "#D53E4F", las =1)
abline(h = 1, lwd = 2, col = "#9E0142", lty = 2)
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
#recB----
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
nomi_parametri = c("recB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2016 = read.table(nome_file_parametri)
output_mcmc_2016 = output_mcmc_2016[,c(12,13)]
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
nomi_parametri = c("recB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2017 = read.table(nome_file_parametri)
output_mcmc_2017 = output_mcmc_2017[,c(12,13)]
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
nomi_parametri = c("recB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2018 = read.table(nome_file_parametri)
output_mcmc_2018 = output_mcmc_2018[,c(12,13)]
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




recB <- as.data.frame(cbind(tab_2016, tab_2017, tab_2018))
colnames(recB) <- c(2016,2017,2018)

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

boxplot(recB, ylim= c(0.1,0.35), bty = "l", ylab = "",
        xlab = "years", main = "recovery rate", col = "#D53E4F", las=1)
abline(h = 0.22, lwd = 2, col = "#9E0142", lty = 2)
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




#GAZZE----
#biting rate----
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

cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8


par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
boxplot(biting_rate, ylim= c(0,0.6), bty = "l", ylab = "",
        xlab = "years", main = "Biting rate", col = c("#D53E4F" ,"#FDAE61" ,"#ABDDA4"), las = 1)
abline(h = 0.1, lwd = 2, col = "black", lty = 2)
legend("right",
       legend = c("avian community 2016"), fill="#D53E4F", horiz=T, cex=1,box.lwd = F)
legend(2.11,0.28,
       legend = c("avian community 2017"), fill="#FDAE61", horiz=T, cex=1,box.lwd = F)

legend(2.11,0.21,
       legend = c("avian community 2018"), fill="#ABDDA4", horiz=T, cex=1,box.lwd = F)
legend(2.41,0.29,
       legend = "magpies",
       col = c("#F46D43"), lwd = 2, lty=2,
       bty = "n",
       cex = 1)

#initial number of birds B0----
#2016_ca
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
nomi_parametri = c("B0_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_ca_2016 = read.table(nome_file_parametri)
output_mcmc_ca_2016 = output_mcmc_ca_2016[,c(3,13)]
burnin = nrow(output_mcmc_ca_2016) * 0.1
if (ncol(output_mcmc_ca_2016) <= 1) 
  output_mcmc_ca_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                 1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_ca_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                            output_mcmc_ca_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_ca_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_ca_2016)) if (quantile(tab_ca_2016[, j], probs = 0.975) > 
                                     ymax) 
    ymax = quantile(tab_ca_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_ca_2016)) if (quantile(tab_ca_2016[, j], probs = 0.025) < 
                                     ymin) 
    ymin = quantile(tab_ca_2016[, j], probs = 0.025)
}

#2016_sn
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
nomi_parametri = c("B0_sn")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_sn_2016 = read.table(nome_file_parametri)
output_mcmc_sn_2016 = output_mcmc_sn_2016[,c(4,13)]
burnin = nrow(output_mcmc_sn_2016) * 0.1
if (ncol(output_mcmc_sn_2016) <= 1) 
  output_mcmc_sn_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                 1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_sn_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                            output_mcmc_sn_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_sn_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_sn_2016)) if (quantile(tab_sn_2016[, j], probs = 0.975) > 
                                     ymax) 
    ymax = quantile(tab_sn_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_sn_2016)) if (quantile(tab_sn_2016[, j], probs = 0.025) < 
                                     ymin) 
    ymin = quantile(tab_sn_2016[, j], probs = 0.025)
}


#2017_ca
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
nomi_parametri = c("B0_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_ca_2017 = read.table(nome_file_parametri)
output_mcmc_ca_2017 = output_mcmc_ca_2017[,c(3,13)]
burnin = nrow(output_mcmc_ca_2017) * 0.1
if (ncol(output_mcmc_ca_2017) <= 1) 
  output_mcmc_ca_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                 1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_ca_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                            output_mcmc_ca_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_ca_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_ca_2017)) if (quantile(tab_ca_2017[, j], probs = 0.975) > 
                                     ymax) 
    ymax = quantile(tab_ca_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_ca_2017)) if (quantile(tab_ca_2017[, j], probs = 0.025) < 
                                     ymin) 
    ymin = quantile(tab_ca_2017[, j], probs = 0.025)
}

#2017_sn
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
nomi_parametri = c("B0_sn")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_sn_2017 = read.table(nome_file_parametri)
output_mcmc_sn_2017 = output_mcmc_sn_2017[,c(4,13)]
burnin = nrow(output_mcmc_sn_2017) * 0.1
if (ncol(output_mcmc_sn_2017) <= 1) 
  output_mcmc_sn_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                 1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_sn_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                            output_mcmc_sn_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_sn_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_sn_2017)) if (quantile(tab_sn_2017[, j], probs = 0.975) > 
                                     ymax) 
    ymax = quantile(tab_sn_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_sn_2017)) if (quantile(tab_sn_2017[, j], probs = 0.025) < 
                                     ymin) 
    ymin = quantile(tab_sn_2017[, j], probs = 0.025)
}


#2018_ca
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
nomi_parametri = c("B0_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_ca_2018 = read.table(nome_file_parametri)
output_mcmc_ca_2018 = output_mcmc_ca_2018[,c(3,13)]
burnin = nrow(output_mcmc_ca_2018) * 0.1
if (ncol(output_mcmc_ca_2018) <= 1) 
  output_mcmc_ca_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                 1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_ca_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                            output_mcmc_ca_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_ca_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_ca_2018)) if (quantile(tab_ca_2018[, j], probs = 0.975) > 
                                     ymax) 
    ymax = quantile(tab_ca_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_ca_2018)) if (quantile(tab_ca_2018[, j], probs = 0.025) < 
                                     ymin) 
    ymin = quantile(tab_ca_2018[, j], probs = 0.025)
}

#2018_sn
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
nomi_parametri = c("B0_sn")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_sn_2018 = read.table(nome_file_parametri)
output_mcmc_sn_2018 = output_mcmc_sn_2018[,c(4,13)]
burnin = nrow(output_mcmc_sn_2018) * 0.1
if (ncol(output_mcmc_sn_2018) <= 1) 
  output_mcmc_sn_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                 1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_sn_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                            output_mcmc_sn_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_sn_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_sn_2018)) if (quantile(tab_sn_2018[, j], probs = 0.975) > 
                                     ymax) 
    ymax = quantile(tab_sn_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_sn_2018)) if (quantile(tab_sn_2018[, j], probs = 0.025) < 
                                     ymin) 
    ymin = quantile(tab_sn_2018[, j], probs = 0.025)
}


initial_number_of_birds <- as.data.frame(cbind(tab_ca_2016, tab_sn_2016,
                                               tab_ca_2017, tab_sn_2017,
                                               tab_ca_2018, tab_sn_2018))
colnames(initial_number_of_birds) <- c("AC16","B16","AC17","B17","AC18","B18")

cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8


par(mfrow = c(1, numero_cluster), mar = c(5, 5, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

boxplot(initial_number_of_birds, ylim= c(0,100), bty = "l", ylab = "",
        xlab = "years", main = "Initial number of birds", 
        col = c("#D53E4F","#9E0142","#FDAE61","#F46D43","#ABDDA4","#66C2A5"),
        las = 1)

initial_number_of_AC_birds <- as.data.frame(cbind(tab_ca_2016,tab_ca_2017,
                                                  tab_ca_2018))
colnames(initial_number_of_AC_birds) <- c("AC2016","AC2017","AC2018")

boxplot(initial_number_of_AC_birds, ylim= c(0,100), bty = "l", ylab = "",
        xlab = "years", main = "Initial number of birds", 
        col = c("#D53E4F","#FDAE61","#ABDDA4"),
        las = 1)

initial_number_of_SN_birds <- as.data.frame(cbind(tab_sn_2016,tab_sn_2017,
                                                  tab_sn_2018))
colnames(initial_number_of_SN_birds) <- c("B2016","B2017","B2018")

boxplot(initial_number_of_SN_birds, ylim= c(0,100), bty = "l", ylab = "",
        xlab = "years", main = "Initial number of birds", 
        col = c("#9E0142","#F46D43","#66C2A5"),
        las = 1)


#muB----
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
nomi_parametri = c("muB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2016 = read.table(nome_file_parametri)
output_mcmc_2016 = output_mcmc_2016[,c(8,13)]
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
nomi_parametri = c("muB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2017 = read.table(nome_file_parametri)
output_mcmc_2017 = output_mcmc_2017[,c(8,13)]
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
nomi_parametri = c("muB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2018 = read.table(nome_file_parametri)
output_mcmc_2018 = output_mcmc_2018[,c(8,13)]
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




muB <- as.data.frame(cbind(tab_2016, tab_2017, tab_2018))
colnames(muB) <- c(2016,2017,2018)

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
boxplot(muB, ylim= c(0,0.025), bty = "l", ylab = "",
        xlab = "years", main = "number of eggs laid", col = c("#D53E4F" ,"#FDAE61" ,"#ABDDA4"), las = 1)
abline(h = 0.0075, lwd = 2, col = "black", lty = 2)
legend(0.45,0.025,
       legend = "avian community 2016", fill="#D53E4F", horiz=T, cex=1,box.lwd = F)
legend(0.45,0.022,
       legend = "avian community 2017", fill="#FDAE61", horiz=T, cex=1,box.lwd = F)
legend(0.45,0.019,
       legend = "avian community 2018", fill="#ABDDA4", horiz=T, cex=1,box.lwd = F)
legend(0.45,0.023,
       legend = "magpies",
       col = c("#F46D43"), lwd = 2, lty=2,
       bty = "n",
       cex = 1)


#s----
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
nomi_parametri = c("s_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2016 = read.table(nome_file_parametri)
output_mcmc_2016 = output_mcmc_2016[,c(9,13)]
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
nomi_parametri = c("s_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2017 = read.table(nome_file_parametri)
output_mcmc_2017 = output_mcmc_2017[,c(9,13)]
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
nomi_parametri = c("s_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2018 = read.table(nome_file_parametri)
output_mcmc_2018 = output_mcmc_2018[,c(9,13)]
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




s <- as.data.frame(cbind(tab_2016, tab_2017, tab_2018))
colnames(s) <- c(2016,2017,2018)

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

boxplot(s, ylim= c(5,20), bty = "l", ylab = "",
        xlab = "years", main = "birth synchronization", col = c("#D53E4F" ,"#FDAE61" ,"#ABDDA4"), las = 1)
abline(h = 6, lwd = 2, col = "black", lty = 2)
#legend(2.7,0.9,
#       legend = "avian community",
#      lwd = 2, lty = F,
#     fill= "grey",
#    bty = "n",
#   cex = 1.1)
#legend(2.7,0.85,
#      legend = "magpies",
#     col = c("red"), lwd = 2, lty=2,
#    bty = "n",
#   cex = 1.1)
#phi----
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
nomi_parametri = c("phi_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2016 = read.table(nome_file_parametri)
output_mcmc_2016 = output_mcmc_2016[,c(10,13)]
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
nomi_parametri = c("phi_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2017 = read.table(nome_file_parametri)
output_mcmc_2017 = output_mcmc_2017[,c(10,13)]
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
nomi_parametri = c("phi_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2018 = read.table(nome_file_parametri)
output_mcmc_2018 = output_mcmc_2018[,c(10,13)]
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




phi <- as.data.frame(cbind(tab_2016, tab_2017, tab_2018))
colnames(phi) <- c(2016,2017,2018)

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

boxplot(phi, ylim= c(0.33,0.67), bty = "l", ylab = "",
        xlab = "years", main = "time of birth", col = c("#D53E4F" ,"#FDAE61" ,"#ABDDA4"), las =1)
abline(h = 0.4, lwd = 2, col = "black", lty = 2)
legend(2.7,0.9,
       legend = "avian community",
       lwd = 2, lty = F,
       fill= "grey",
       bty = "n",
       cex = 1.1)
legend(2.7,0.85,
       legend = "magpies",
       col = c("red"), lwd = 2, lty=2,
       bty = "n",
       cex = 1.1)
#niB----
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
nomi_parametri = c("niB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2016 = read.table(nome_file_parametri)
output_mcmc_2016 = output_mcmc_2016[,c(11,13)]
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
nomi_parametri = c("niB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2017 = read.table(nome_file_parametri)
output_mcmc_2017 = output_mcmc_2017[,c(11,13)]
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
nomi_parametri = c("niB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2018 = read.table(nome_file_parametri)
output_mcmc_2018 = output_mcmc_2018[,c(11,13)]
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




niB <- as.data.frame(cbind(tab_2016, tab_2017, tab_2018))
colnames(niB) <- c(2016,2017,2018)

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

boxplot(niB, ylim= c(0.3,1.03), bty = "l", ylab = "",
        xlab = "years", main = "infection rate", col = c("#D53E4F" ,"#FDAE61" ,"#ABDDA4"), las =1)
abline(h = 0.66, lwd = 2, col = "black", lty = 2)
legend(2.7,0.9,
       legend = "avian community",
       lwd = 2, lty = F,
       fill= "grey",
       bty = "n",
       cex = 1.1)
legend(2.7,0.85,
       legend = "magpies",
       col = c("red"), lwd = 2, lty=2,
       bty = "n",
       cex = 1.1)
#recB----
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
nomi_parametri = c("recB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2016 = read.table(nome_file_parametri)
output_mcmc_2016 = output_mcmc_2016[,c(12,13)]
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
nomi_parametri = c("recB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2017 = read.table(nome_file_parametri)
output_mcmc_2017 = output_mcmc_2017[,c(12,13)]
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
nomi_parametri = c("recB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2018 = read.table(nome_file_parametri)
output_mcmc_2018 = output_mcmc_2018[,c(12,13)]
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




recB <- as.data.frame(cbind(tab_2016, tab_2017, tab_2018))
colnames(recB) <- c(2016,2017,2018)

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

boxplot(recB, ylim= c(0.05,0.3), bty = "l", ylab = "",
        xlab = "years", main = "recovery rate", col = c("#D53E4F" ,"#FDAE61" ,"#ABDDA4"), las=1)
abline(h = 0.2, lwd = 2, col = "black", lty = 2)
legend(2.7,0.9,
       legend = "avian community",
       lwd = 2, lty = F,
       fill= "grey",
       bty = "n",
       cex = 1.1)
legend(2.7,0.85,
       legend = "magpies",
       col = c("red"), lwd = 2, lty=2,
       bty = "n",
       cex = 1.1)















#GERMANI----
#biting rate----
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

cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8


par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
boxplot(biting_rate, ylim= c(0,0.88), bty = "l", ylab = "",
        xlab = "years", main = "Biting rate", col = "grey", las = 1)
abline(h = 0.01, lwd = 2, col = "#66C2A5", lty = 2)
legend("topleft",
       legend = "avian community", fill="grey", horiz=T, cex=1,box.lwd = F)
legend(0.4,0.8,
       legend = "mallards",
       col = c("#66C2A5"), lwd = 2, lty=2,
       bty = "n",
       cex = 1)

#initial number of birds B0----
#2016_ca
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
nomi_parametri = c("B0_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_ca_2016 = read.table(nome_file_parametri)
output_mcmc_ca_2016 = output_mcmc_ca_2016[,c(3,13)]
burnin = nrow(output_mcmc_ca_2016) * 0.1
if (ncol(output_mcmc_ca_2016) <= 1) 
  output_mcmc_ca_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                 1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_ca_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                            output_mcmc_ca_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_ca_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_ca_2016)) if (quantile(tab_ca_2016[, j], probs = 0.975) > 
                                     ymax) 
    ymax = quantile(tab_ca_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_ca_2016)) if (quantile(tab_ca_2016[, j], probs = 0.025) < 
                                     ymin) 
    ymin = quantile(tab_ca_2016[, j], probs = 0.025)
}

#2016_sn
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
nomi_parametri = c("B0_sn")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_sn_2016 = read.table(nome_file_parametri)
output_mcmc_sn_2016 = output_mcmc_sn_2016[,c(4,13)]
burnin = nrow(output_mcmc_sn_2016) * 0.1
if (ncol(output_mcmc_sn_2016) <= 1) 
  output_mcmc_sn_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                 1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_sn_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                            output_mcmc_sn_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_sn_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_sn_2016)) if (quantile(tab_sn_2016[, j], probs = 0.975) > 
                                     ymax) 
    ymax = quantile(tab_sn_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_sn_2016)) if (quantile(tab_sn_2016[, j], probs = 0.025) < 
                                     ymin) 
    ymin = quantile(tab_sn_2016[, j], probs = 0.025)
}


#2017_ca
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
nomi_parametri = c("B0_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_ca_2017 = read.table(nome_file_parametri)
output_mcmc_ca_2017 = output_mcmc_ca_2017[,c(3,13)]
burnin = nrow(output_mcmc_ca_2017) * 0.1
if (ncol(output_mcmc_ca_2017) <= 1) 
  output_mcmc_ca_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                 1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_ca_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                            output_mcmc_ca_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_ca_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_ca_2017)) if (quantile(tab_ca_2017[, j], probs = 0.975) > 
                                     ymax) 
    ymax = quantile(tab_ca_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_ca_2017)) if (quantile(tab_ca_2017[, j], probs = 0.025) < 
                                     ymin) 
    ymin = quantile(tab_ca_2017[, j], probs = 0.025)
}

#2017_sn
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
nomi_parametri = c("B0_sn")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_sn_2017 = read.table(nome_file_parametri)
output_mcmc_sn_2017 = output_mcmc_sn_2017[,c(4,13)]
burnin = nrow(output_mcmc_sn_2017) * 0.1
if (ncol(output_mcmc_sn_2017) <= 1) 
  output_mcmc_sn_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                 1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_sn_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                            output_mcmc_sn_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_sn_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_sn_2017)) if (quantile(tab_sn_2017[, j], probs = 0.975) > 
                                     ymax) 
    ymax = quantile(tab_sn_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_sn_2017)) if (quantile(tab_sn_2017[, j], probs = 0.025) < 
                                     ymin) 
    ymin = quantile(tab_sn_2017[, j], probs = 0.025)
}


#2018_ca
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
nomi_parametri = c("B0_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_ca_2018 = read.table(nome_file_parametri)
output_mcmc_ca_2018 = output_mcmc_ca_2018[,c(3,13)]
burnin = nrow(output_mcmc_ca_2018) * 0.1
if (ncol(output_mcmc_ca_2018) <= 1) 
  output_mcmc_ca_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                 1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_ca_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                            output_mcmc_ca_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_ca_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_ca_2018)) if (quantile(tab_ca_2018[, j], probs = 0.975) > 
                                     ymax) 
    ymax = quantile(tab_ca_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_ca_2018)) if (quantile(tab_ca_2018[, j], probs = 0.025) < 
                                     ymin) 
    ymin = quantile(tab_ca_2018[, j], probs = 0.025)
}

#2018_sn
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
nomi_parametri = c("B0_sn")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_sn_2018 = read.table(nome_file_parametri)
output_mcmc_sn_2018 = output_mcmc_sn_2018[,c(4,13)]
burnin = nrow(output_mcmc_sn_2018) * 0.1
if (ncol(output_mcmc_sn_2018) <= 1) 
  output_mcmc_sn_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                 1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_sn_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                            output_mcmc_sn_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_sn_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_sn_2018)) if (quantile(tab_sn_2018[, j], probs = 0.975) > 
                                     ymax) 
    ymax = quantile(tab_sn_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_sn_2018)) if (quantile(tab_sn_2018[, j], probs = 0.025) < 
                                     ymin) 
    ymin = quantile(tab_sn_2018[, j], probs = 0.025)
}


initial_number_of_birds <- as.data.frame(cbind(tab_ca_2016, tab_sn_2016,
                                               tab_ca_2017, tab_sn_2017,
                                               tab_ca_2018, tab_sn_2018))
colnames(initial_number_of_birds) <- c("AC16","B16","AC17","B17","AC18","B18")

cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8


par(mfrow = c(1, numero_cluster), mar = c(5, 5, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

boxplot(initial_number_of_birds, ylim= c(0,100), bty = "l", ylab = "",
        xlab = "years", main = "Initial number of birds", 
        col = c("#D53E4F","#9E0142","#FDAE61","#F46D43","#ABDDA4","#66C2A5"),
        las = 1)

initial_number_of_AC_birds <- as.data.frame(cbind(tab_ca_2016,tab_ca_2017,
                                                  tab_ca_2018))
colnames(initial_number_of_AC_birds) <- c("AC2016","AC2017","AC2018")

boxplot(initial_number_of_AC_birds, ylim= c(0,100), bty = "l", ylab = "",
        xlab = "years", main = "Initial number of birds", 
        col = c("#D53E4F","#FDAE61","#ABDDA4"),
        las = 1)

initial_number_of_SN_birds <- as.data.frame(cbind(tab_sn_2016,tab_sn_2017,
                                                  tab_sn_2018))
colnames(initial_number_of_SN_birds) <- c("B2016","B2017","B2018")

boxplot(initial_number_of_SN_birds, ylim= c(0,100), bty = "l", ylab = "",
        xlab = "years", main = "Initial number of birds", 
        col = c("#9E0142","#F46D43","#66C2A5"),
        las = 1)


#muB----
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
nomi_parametri = c("muB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2016 = read.table(nome_file_parametri)
output_mcmc_2016 = output_mcmc_2016[,c(8,13)]
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
nomi_parametri = c("muB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2017 = read.table(nome_file_parametri)
output_mcmc_2017 = output_mcmc_2017[,c(8,13)]
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
nomi_parametri = c("muB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2018 = read.table(nome_file_parametri)
output_mcmc_2018 = output_mcmc_2018[,c(8,13)]
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




muB <- as.data.frame(cbind(tab_2016, tab_2017, tab_2018))
colnames(muB) <- c(2016,2017,2018)

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
boxplot(muB, ylim= c(0,0.025), bty = "l", ylab = "",
        xlab = "years", main = "number of eggs laid", col = "grey", las = 1)
abline(h = 0.014, lwd = 2, col = "#66C2A5", lty = 2)
legend(0.45,0.025,
       legend = "avian community", fill="grey", horiz=T, cex=1,box.lwd = F)
legend(0.45,0.023,
       legend = "mallards",
       col = c("#66C2A5"), lwd = 2, lty=2,
       bty = "n",
       cex = 1)


#s----
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
nomi_parametri = c("s_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2016 = read.table(nome_file_parametri)
output_mcmc_2016 = output_mcmc_2016[,c(9,13)]
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
nomi_parametri = c("s_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2017 = read.table(nome_file_parametri)
output_mcmc_2017 = output_mcmc_2017[,c(9,13)]
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
nomi_parametri = c("s_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2018 = read.table(nome_file_parametri)
output_mcmc_2018 = output_mcmc_2018[,c(9,13)]
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




s <- as.data.frame(cbind(tab_2016, tab_2017, tab_2018))
colnames(s) <- c(2016,2017,2018)

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

boxplot(s, ylim= c(5,20), bty = "l", ylab = "",
        xlab = "years", main = "birth synchronization", col = "grey", las = 1)
abline(h = 10, lwd = 2, col = "#66C2A5", lty = 2)
#legend(2.7,0.9,
#       legend = "avian community",
#      lwd = 2, lty = F,
#     fill= "grey",
#    bty = "n",
#   cex = 1.1)
#legend(2.7,0.85,
#      legend = "mallards",
#     col = c("red"), lwd = 2, lty=2,
#    bty = "n",
#   cex = 1.1)
#phi----
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
nomi_parametri = c("phi_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2016 = read.table(nome_file_parametri)
output_mcmc_2016 = output_mcmc_2016[,c(10,13)]
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
nomi_parametri = c("phi_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2017 = read.table(nome_file_parametri)
output_mcmc_2017 = output_mcmc_2017[,c(10,13)]
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
nomi_parametri = c("phi_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2018 = read.table(nome_file_parametri)
output_mcmc_2018 = output_mcmc_2018[,c(10,13)]
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




phi <- as.data.frame(cbind(tab_2016, tab_2017, tab_2018))
colnames(phi) <- c(2016,2017,2018)

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

boxplot(phi, ylim= c(0.3,0.7), bty = "l", ylab = "",
        xlab = "years", main = "time of birth", col = "grey", las =1)
abline(h = 0.33, lwd = 2, col = "#66C2A5", lty = 2)
legend(2.7,0.9,
       legend = "avian community",
       lwd = 2, lty = F,
       fill= "grey",
       bty = "n",
       cex = 1.1)
legend(2.7,0.85,
       legend = "mallards",
       col = c("red"), lwd = 2, lty=2,
       bty = "n",
       cex = 1.1)
#niB----
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
nomi_parametri = c("niB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2016 = read.table(nome_file_parametri)
output_mcmc_2016 = output_mcmc_2016[,c(11,13)]
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
nomi_parametri = c("niB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2017 = read.table(nome_file_parametri)
output_mcmc_2017 = output_mcmc_2017[,c(11,13)]
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
nomi_parametri = c("niB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2018 = read.table(nome_file_parametri)
output_mcmc_2018 = output_mcmc_2018[,c(11,13)]
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




niB <- as.data.frame(cbind(tab_2016, tab_2017, tab_2018))
colnames(niB) <- c(2016,2017,2018)

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

boxplot(niB, ylim= c(0.35,1), bty = "l", ylab = "",
        xlab = "years", main = "infection rate", col = "grey", las =1)
abline(h = 0.66, lwd = 2, col = "#66C2A5", lty = 2)
legend(2.7,0.9,
       legend = "avian community",
       lwd = 2, lty = F,
       fill= "grey",
       bty = "n",
       cex = 1.1)
legend(2.7,0.85,
       legend = "mallards",
       col = c("red"), lwd = 2, lty=2,
       bty = "n",
       cex = 1.1)
#recB----
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
nomi_parametri = c("recB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2016 = read.table(nome_file_parametri)
output_mcmc_2016 = output_mcmc_2016[,c(12,13)]
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
nomi_parametri = c("recB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2017 = read.table(nome_file_parametri)
output_mcmc_2017 = output_mcmc_2017[,c(12,13)]
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
nomi_parametri = c("recB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_2018 = read.table(nome_file_parametri)
output_mcmc_2018 = output_mcmc_2018[,c(12,13)]
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




recB <- as.data.frame(cbind(tab_2016, tab_2017, tab_2018))
colnames(recB) <- c(2016,2017,2018)

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

boxplot(recB, ylim= c(0.05,0.3), bty = "l", ylab = "",
        xlab = "years", main = "recovery rate", col = "grey", las=1)
abline(h = 0.25, lwd = 2, col = "#66C2A5", lty = 2)
legend(2.7,0.9,
       legend = "avian community",
       lwd = 2, lty = F,
       fill= "grey",
       bty = "n",
       cex = 1.1)
legend(2.7,0.85,
       legend = "mallards",
       col = c("red"), lwd = 2, lty=2,
       bty = "n",
       cex = 1.1)







#####Confronto boxplot 3 specie####
#biting rate 2016----
#MERLI
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
output_mcmc_black_2016 = read.table(nome_file_parametri)
output_mcmc_black_2016 = output_mcmc_black_2016[,c(7,13)]
burnin = nrow(output_mcmc_black_2016) * 0.1
if (ncol(output_mcmc_black_2016) <= 1) 
  output_mcmc_black_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                    1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                               output_mcmc_black_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_2016)) if (quantile(tab_black_2016[, j], probs = 0.975) > 
                                        ymax) 
    ymax = quantile(tab_black_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_2016)) if (quantile(tab_black_2016[, j], probs = 0.025) < 
                                        ymin) 
    ymin = quantile(tab_black_2016[, j], probs = 0.025)
}
#GAZZE
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
output_mcmc_mag_2016 = read.table(nome_file_parametri)
output_mcmc_mag_2016 = output_mcmc_mag_2016[,c(7,13)]
burnin = nrow(output_mcmc_mag_2016) * 0.1
if (ncol(output_mcmc_mag_2016) <= 1) 
  output_mcmc_mag_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                  1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_mag_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_2016)) if (quantile(tab_mag_2016[, j], probs = 0.975) > 
                                      ymax) 
    ymax = quantile(tab_mag_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_2016)) if (quantile(tab_mag_2016[, j], probs = 0.025) < 
                                      ymin) 
    ymin = quantile(tab_mag_2016[, j], probs = 0.025)
}

#GERMANI
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
output_mcmc_mall_2016 = read.table(nome_file_parametri)
output_mcmc_mall_2016 = output_mcmc_mall_2016[,c(7,13)]
burnin = nrow(output_mcmc_mall_2016) * 0.1
if (ncol(output_mcmc_mall_2016) <= 1) 
  output_mcmc_mall_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                   1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                              output_mcmc_mall_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_2016)) if (quantile(tab_mall_2016[, j], probs = 0.975) > 
                                       ymax) 
    ymax = quantile(tab_mall_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_2016)) if (quantile(tab_mall_2016[, j], probs = 0.025) < 
                                       ymin) 
    ymin = quantile(tab_mall_2016[, j], probs = 0.025)
}



biting_rate_2016 <- as.data.frame(cbind(tab_black_2016, tab_mag_2016, tab_mall_2016))
colnames(biting_rate_2016) <- c("blackbirds","magpies","mallards")

par(cex.axis = 2, cex.lab = 2, cex.main = 2, bty = "l")

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
boxplot(biting_rate_2016, ylim= c(0,0.6), bty = "l", ylab = "Biting rate",
        xlab = "", main = "Biting rate 2016", col = c("#D53E4F","#FDAE61","#ABDDA4"))
abline(h = c(0.35,0.1,0.01), lwd = 2, col = c("#9E0142","#F46D43","#66C2A5"), lty = 2)


#biting rate 2017----
#MERLI
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
output_mcmc_black_2017 = read.table(nome_file_parametri)
output_mcmc_black_2017 = output_mcmc_black_2017[,c(7,13)]
burnin = nrow(output_mcmc_black_2017) * 0.1
if (ncol(output_mcmc_black_2017) <= 1) 
  output_mcmc_black_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                    1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                               output_mcmc_black_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_2017)) if (quantile(tab_black_2017[, j], probs = 0.975) > 
                                        ymax) 
    ymax = quantile(tab_black_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_2017)) if (quantile(tab_black_2017[, j], probs = 0.025) < 
                                        ymin) 
    ymin = quantile(tab_black_2017[, j], probs = 0.025)
}
#GAZZE
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
output_mcmc_mag_2017 = read.table(nome_file_parametri)
output_mcmc_mag_2017 = output_mcmc_mag_2017[,c(7,13)]
burnin = nrow(output_mcmc_mag_2017) * 0.1
if (ncol(output_mcmc_mag_2017) <= 1) 
  output_mcmc_mag_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                  1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_mag_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_2017)) if (quantile(tab_mag_2017[, j], probs = 0.975) > 
                                      ymax) 
    ymax = quantile(tab_mag_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_2017)) if (quantile(tab_mag_2017[, j], probs = 0.025) < 
                                      ymin) 
    ymin = quantile(tab_mag_2017[, j], probs = 0.025)
}

#GERMANI
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
output_mcmc_mall_2017 = read.table(nome_file_parametri)
output_mcmc_mall_2017 = output_mcmc_mall_2017[,c(7,13)]
burnin = nrow(output_mcmc_mall_2017) * 0.1
if (ncol(output_mcmc_mall_2017) <= 1) 
  output_mcmc_mall_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                   1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                              output_mcmc_mall_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_2017)) if (quantile(tab_mall_2017[, j], probs = 0.975) > 
                                       ymax) 
    ymax = quantile(tab_mall_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_2017)) if (quantile(tab_mall_2017[, j], probs = 0.025) < 
                                       ymin) 
    ymin = quantile(tab_mall_2017[, j], probs = 0.025)
}



biting_rate_2017 <- as.data.frame(cbind(tab_black_2017, tab_mag_2017, tab_mall_2017))
colnames(biting_rate_2017) <- c("blackbirds","magpies","mallards")

par(cex.axis = 2, cex.lab = 2, cex.main = 2, bty = "l")

boxplot(biting_rate_2017, ylim= c(0,0.8), bty = "l", ylab = "Biting rate",
        xlab = "", main = "Biting rate 2017", col = c("#D53E4F","#FDAE61","#ABDDA4"))
abline(h = c(0.35,0.1,0.01), lwd = 2, col = c("#9E0142","#F46D43","#66C2A5"), lty = 2)

#legend(2.7,0.9,
# legend = "avian community",
#lwd = 2, lty = F,
#fill= "grey",
# bty = "n",
# cex = 1.1)
#legend(2.7,0.85,
# legend = "blackbirds",
# col = c("red"), lwd = 2, lty=2,
# bty = "n",
# cex = 1.1)

#biting rate 2018----
#MERLI
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
output_mcmc_black_2018 = read.table(nome_file_parametri)
output_mcmc_black_2018 = output_mcmc_black_2018[,c(7,13)]
burnin = nrow(output_mcmc_black_2018) * 0.1
if (ncol(output_mcmc_black_2018) <= 1) 
  output_mcmc_black_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                    1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                               output_mcmc_black_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_2018)) if (quantile(tab_black_2018[, j], probs = 0.975) > 
                                        ymax) 
    ymax = quantile(tab_black_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_2018)) if (quantile(tab_black_2018[, j], probs = 0.025) < 
                                        ymin) 
    ymin = quantile(tab_black_2018[, j], probs = 0.025)
}
#GAZZE
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
output_mcmc_mag_2018 = read.table(nome_file_parametri)
output_mcmc_mag_2018 = output_mcmc_mag_2018[,c(7,13)]
burnin = nrow(output_mcmc_mag_2018) * 0.1
if (ncol(output_mcmc_mag_2018) <= 1) 
  output_mcmc_mag_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                  1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_mag_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_2018)) if (quantile(tab_mag_2018[, j], probs = 0.975) > 
                                      ymax) 
    ymax = quantile(tab_mag_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_2018)) if (quantile(tab_mag_2018[, j], probs = 0.025) < 
                                      ymin) 
    ymin = quantile(tab_mag_2018[, j], probs = 0.025)
}

#GERMANI
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
output_mcmc_mall_2018 = read.table(nome_file_parametri)
output_mcmc_mall_2018 = output_mcmc_mall_2018[,c(7,13)]
burnin = nrow(output_mcmc_mall_2018) * 0.1
if (ncol(output_mcmc_mall_2018) <= 1) 
  output_mcmc_mall_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                   1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                              output_mcmc_mall_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_2018)) if (quantile(tab_mall_2018[, j], probs = 0.975) > 
                                       ymax) 
    ymax = quantile(tab_mall_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_2018)) if (quantile(tab_mall_2018[, j], probs = 0.025) < 
                                       ymin) 
    ymin = quantile(tab_mall_2018[, j], probs = 0.025)
}



biting_rate_2018 <- as.data.frame(cbind(tab_black_2018, tab_mag_2018, tab_mall_2018))
colnames(biting_rate_2018) <- c("blackbirds","magpies","mallards")

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
boxplot(biting_rate_2018, ylim= c(0,0.9), bty = "l", ylab = "Biting rate",
        xlab = "", main = "Biting rate 2018", col = c("#D53E4F","#FDAE61","#ABDDA4"))
abline(h = c(0.35,0.1,0.01), lwd = 2, col = c("#9E0142","#F46D43","#66C2A5"), lty = 2)

#legend(2.7,0.9,
#    legend = "avian community",
#   lwd = 2, lty = F,
#   fill= "grey",
#   bty = "n",
#  cex = 1.1)
#legend(2.7,0.85,
#  legend = "blackbirds",
# col = c("red"), lwd = 2, lty=2,
#  bty = "n",
#  cex = 1.1)



#B0 2016----
#MERLI ca
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
nomi_parametri = c("B0_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_ca_2016 = read.table(nome_file_parametri)
output_mcmc_black_ca_2016 = output_mcmc_black_ca_2016[,c(3,13)]
burnin = nrow(output_mcmc_black_ca_2016) * 0.1
if (ncol(output_mcmc_black_ca_2016) <= 1) 
  output_mcmc_black_ca_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                       1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_ca_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                                  output_mcmc_black_ca_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_ca_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_ca_2016)) if (quantile(tab_black_ca_2016[, j], probs = 0.975) > 
                                           ymax) 
    ymax = quantile(tab_black_ca_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_ca_2016)) if (quantile(tab_black_ca_2016[, j], probs = 0.025) < 
                                           ymin) 
    ymin = quantile(tab_black_ca_2016[, j], probs = 0.025)
}

#MERLI sn
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
nomi_parametri = c("B0_sn")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_sn_2016 = read.table(nome_file_parametri)
output_mcmc_black_sn_2016 = output_mcmc_black_sn_2016[,c(4,13)]
burnin = nrow(output_mcmc_black_sn_2016) * 0.1
if (ncol(output_mcmc_black_sn_2016) <= 1) 
  output_mcmc_black_sn_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                       1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_sn_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                                  output_mcmc_black_sn_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_sn_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_sn_2016)) if (quantile(tab_black_sn_2016[, j], probs = 0.975) > 
                                           ymax) 
    ymax = quantile(tab_black_sn_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_sn_2016)) if (quantile(tab_black_sn_2016[, j], probs = 0.025) < 
                                           ymin) 
    ymin = quantile(tab_black_sn_2016[, j], probs = 0.025)
}

#GAZZE ca
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
nomi_parametri = c("B0_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_ca_2016 = read.table(nome_file_parametri)
output_mcmc_mag_ca_2016 = output_mcmc_mag_ca_2016[,c(3,13)]
burnin = nrow(output_mcmc_mag_ca_2016) * 0.1
if (ncol(output_mcmc_mag_ca_2016) <= 1) 
  output_mcmc_mag_ca_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                     1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_ca_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                                output_mcmc_mag_ca_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_ca_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_ca_2016)) if (quantile(tab_mag_ca_2016[, j], probs = 0.975) > 
                                         ymax) 
    ymax = quantile(tab_mag_ca_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_ca_2016)) if (quantile(tab_mag_ca_2016[, j], probs = 0.025) < 
                                         ymin) 
    ymin = quantile(tab_mag_ca_2016[, j], probs = 0.025)
}

#GAZZE sn
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
nomi_parametri = c("B0_sn")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_sn_2016 = read.table(nome_file_parametri)
output_mcmc_mag_sn_2016 = output_mcmc_mag_sn_2016[,c(4,13)]
burnin = nrow(output_mcmc_mag_sn_2016) * 0.1
if (ncol(output_mcmc_mag_sn_2016) <= 1) 
  output_mcmc_mag_sn_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                     1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_sn_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                                output_mcmc_mag_sn_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_sn_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_sn_2016)) if (quantile(tab_mag_sn_2016[, j], probs = 0.975) > 
                                         ymax) 
    ymax = quantile(tab_mag_sn_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_sn_2016)) if (quantile(tab_mag_sn_2016[, j], probs = 0.025) < 
                                         ymin) 
    ymin = quantile(tab_mag_sn_2016[, j], probs = 0.025)
}

#GERMANI ca
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
nomi_parametri = c("B0_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_ca_2016 = read.table(nome_file_parametri)
output_mcmc_mall_ca_2016 = output_mcmc_mall_ca_2016[,c(3,13)]
burnin = nrow(output_mcmc_mall_ca_2016) * 0.1
if (ncol(output_mcmc_mall_ca_2016) <= 1) 
  output_mcmc_mall_ca_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                      1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_ca_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                                 output_mcmc_mall_ca_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_ca_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_ca_2016)) if (quantile(tab_mall_ca_2016[, j], probs = 0.975) > 
                                          ymax) 
    ymax = quantile(tab_mall_ca_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_ca_2016)) if (quantile(tab_mall_ca_2016[, j], probs = 0.025) < 
                                          ymin) 
    ymin = quantile(tab_mall_ca_2016[, j], probs = 0.025)
}

#GERMANI sn
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
nomi_parametri = c("B0_sn")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_sn_2016 = read.table(nome_file_parametri)
output_mcmc_mall_sn_2016 = output_mcmc_mall_sn_2016[,c(4,13)]
burnin = nrow(output_mcmc_mall_sn_2016) * 0.1
if (ncol(output_mcmc_mall_sn_2016) <= 1) 
  output_mcmc_mall_sn_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                      1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_sn_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                                 output_mcmc_mall_sn_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_sn_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_sn_2016)) if (quantile(tab_mall_sn_2016[, j], probs = 0.975) > 
                                          ymax) 
    ymax = quantile(tab_mall_sn_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_sn_2016)) if (quantile(tab_mall_sn_2016[, j], probs = 0.025) < 
                                          ymin) 
    ymin = quantile(tab_mall_sn_2016[, j], probs = 0.025)
}

initial_number_of_ca_birds_2016 <- as.data.frame(cbind(tab_black_ca_2016,
                                                       tab_mag_ca_2016,
                                                       tab_mall_ca_2016))
colnames(initial_number_of_ca_birds_2016) <- c("blackbirds","magpies","mallards")

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
boxplot(initial_number_of_ca_birds_2016, ylim= c(0,100), bty = "l", ylab = "Initial number of birds",
        xlab = "", main = "Initial number of AC birds 2016", col = c("#D53E4F", "#FDAE61","#ABDDA4"))


initial_number_of_sn_birds_2016 <- as.data.frame(cbind(tab_black_sn_2016,
                                                       tab_mag_sn_2016,
                                                       tab_mall_sn_2016))
colnames(initial_number_of_sn_birds_2016) <- c("blackbirds","magpies","mallards")

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
boxplot(initial_number_of_sn_birds_2016, ylim= c(0,100), bty = "l", ylab = "Initial number of birds",
        xlab = "", main = "Initial number of birds 2016", col = c("#9E0142", "#F46D43","#66C2A5"))


initial_number_of_ca_sn_birds_2016 <- as.data.frame(cbind(tab_black_ca_2016,tab_black_sn_2016,
                                                          tab_mag_ca_2016,tab_mag_sn_2016,
                                                          tab_mall_ca_2016,tab_mall_sn_2016))
colnames(initial_number_of_ca_sn_birds_2016) <- c("AC","blackbirds",
                                                  "AC","magpies",
                                                  "AC","mallards")

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
boxplot(initial_number_of_ca_sn_birds_2016, ylim= c(0,100), bty = "l", ylab = "Initial number of birds",
        xlab = "", main = "Initial number of birds 2016", col = c("#D53E4F","#9E0142",
                                                                  "#FDAE61","#F46D43",
                                                                  "#ABDDA4","#66C2A5"))

#B0 2017----
#MERLI ca
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
nomi_parametri = c("B0_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_ca_2017 = read.table(nome_file_parametri)
output_mcmc_black_ca_2017 = output_mcmc_black_ca_2017[,c(3,13)]
burnin = nrow(output_mcmc_black_ca_2017) * 0.1
if (ncol(output_mcmc_black_ca_2017) <= 1) 
  output_mcmc_black_ca_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                       1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_ca_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                                  output_mcmc_black_ca_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_ca_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_ca_2017)) if (quantile(tab_black_ca_2017[, j], probs = 0.975) > 
                                           ymax) 
    ymax = quantile(tab_black_ca_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_ca_2017)) if (quantile(tab_black_ca_2017[, j], probs = 0.025) < 
                                           ymin) 
    ymin = quantile(tab_black_ca_2017[, j], probs = 0.025)
}

#MERLI sn
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
nomi_parametri = c("B0_sn")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_sn_2017 = read.table(nome_file_parametri)
output_mcmc_black_sn_2017 = output_mcmc_black_sn_2017[,c(4,13)]
burnin = nrow(output_mcmc_black_sn_2017) * 0.1
if (ncol(output_mcmc_black_sn_2017) <= 1) 
  output_mcmc_black_sn_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                       1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_sn_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                                  output_mcmc_black_sn_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_sn_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_sn_2017)) if (quantile(tab_black_sn_2017[, j], probs = 0.975) > 
                                           ymax) 
    ymax = quantile(tab_black_sn_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_sn_2017)) if (quantile(tab_black_sn_2017[, j], probs = 0.025) < 
                                           ymin) 
    ymin = quantile(tab_black_sn_2017[, j], probs = 0.025)
}

#GAZZE ca
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
nomi_parametri = c("B0_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_ca_2017 = read.table(nome_file_parametri)
output_mcmc_mag_ca_2017 = output_mcmc_mag_ca_2017[,c(3,13)]
burnin = nrow(output_mcmc_mag_ca_2017) * 0.1
if (ncol(output_mcmc_mag_ca_2017) <= 1) 
  output_mcmc_mag_ca_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                     1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_ca_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                                output_mcmc_mag_ca_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_ca_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_ca_2017)) if (quantile(tab_mag_ca_2017[, j], probs = 0.975) > 
                                         ymax) 
    ymax = quantile(tab_mag_ca_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_ca_2017)) if (quantile(tab_mag_ca_2017[, j], probs = 0.025) < 
                                         ymin) 
    ymin = quantile(tab_mag_ca_2017[, j], probs = 0.025)
}

#GAZZE sn
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
nomi_parametri = c("B0_sn")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_sn_2017 = read.table(nome_file_parametri)
output_mcmc_mag_sn_2017 = output_mcmc_mag_sn_2017[,c(4,13)]
burnin = nrow(output_mcmc_mag_sn_2017) * 0.1
if (ncol(output_mcmc_mag_sn_2017) <= 1) 
  output_mcmc_mag_sn_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                     1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_sn_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                                output_mcmc_mag_sn_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_sn_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_sn_2017)) if (quantile(tab_mag_sn_2017[, j], probs = 0.975) > 
                                         ymax) 
    ymax = quantile(tab_mag_sn_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_sn_2017)) if (quantile(tab_mag_sn_2017[, j], probs = 0.025) < 
                                         ymin) 
    ymin = quantile(tab_mag_sn_2017[, j], probs = 0.025)
}

#GERMANI ca
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
nomi_parametri = c("B0_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_ca_2017 = read.table(nome_file_parametri)
output_mcmc_mall_ca_2017 = output_mcmc_mall_ca_2017[,c(3,13)]
burnin = nrow(output_mcmc_mall_ca_2017) * 0.1
if (ncol(output_mcmc_mall_ca_2017) <= 1) 
  output_mcmc_mall_ca_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                      1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_ca_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                                 output_mcmc_mall_ca_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_ca_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_ca_2017)) if (quantile(tab_mall_ca_2017[, j], probs = 0.975) > 
                                          ymax) 
    ymax = quantile(tab_mall_ca_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_ca_2017)) if (quantile(tab_mall_ca_2017[, j], probs = 0.025) < 
                                          ymin) 
    ymin = quantile(tab_mall_ca_2017[, j], probs = 0.025)
}

#GERMANI sn
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
nomi_parametri = c("B0_sn")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_sn_2017 = read.table(nome_file_parametri)
output_mcmc_mall_sn_2017 = output_mcmc_mall_sn_2017[,c(4,13)]
burnin = nrow(output_mcmc_mall_sn_2017) * 0.1
if (ncol(output_mcmc_mall_sn_2017) <= 1) 
  output_mcmc_mall_sn_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                      1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_sn_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                                 output_mcmc_mall_sn_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_sn_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_sn_2017)) if (quantile(tab_mall_sn_2017[, j], probs = 0.975) > 
                                          ymax) 
    ymax = quantile(tab_mall_sn_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_sn_2017)) if (quantile(tab_mall_sn_2017[, j], probs = 0.025) < 
                                          ymin) 
    ymin = quantile(tab_mall_sn_2017[, j], probs = 0.025)
}

initial_number_of_ca_birds_2017 <- as.data.frame(cbind(tab_black_ca_2017,
                                                       tab_mag_ca_2017,
                                                       tab_mall_ca_2017))
colnames(initial_number_of_ca_birds_2017) <- c("blackbirds","magpies","mallards")


boxplot(initial_number_of_ca_birds_2017, ylim= c(0,100), bty = "l", ylab = "Initial number of birds",
        xlab = "", main = "Initial number of AC birds 2017", col = c("#D53E4F", "#FDAE61","#ABDDA4"))


initial_number_of_sn_birds_2017 <- as.data.frame(cbind(tab_black_sn_2017,
                                                       tab_mag_sn_2017,
                                                       tab_mall_sn_2017))
colnames(initial_number_of_sn_birds_2017) <- c("blackbirds","magpies","mallards")


boxplot(initial_number_of_sn_birds_2017, ylim= c(0,100), bty = "l", ylab = "Initial number of birds",
        xlab = "", main = "Initial number of birds 2017", col = c("#9E0142", "#F46D43","#66C2A5"))


initial_number_of_ca_sn_birds_2017 <- as.data.frame(cbind(tab_black_ca_2017,tab_black_sn_2017,
                                                          tab_mag_ca_2017,tab_mag_sn_2017,
                                                          tab_mall_ca_2017,tab_mall_sn_2017))
colnames(initial_number_of_ca_sn_birds_2017) <- c("AC","blackbirds",
                                                  "AC","magpies",
                                                  "AC","mallards")


boxplot(initial_number_of_ca_sn_birds_2017, ylim= c(0,100), bty = "l", ylab = "Initial number of birds",
        xlab = "", main = "Initial number of birds 2017", col = c("#D53E4F","#9E0142",
                                                                  "#FDAE61","#F46D43",
                                                                  "#ABDDA4","#66C2A5"))


#B0 2018----
#MERLI ca
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
nomi_parametri = c("B0_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_ca_2018 = read.table(nome_file_parametri)
output_mcmc_black_ca_2018 = output_mcmc_black_ca_2018[,c(3,13)]
burnin = nrow(output_mcmc_black_ca_2018) * 0.1
if (ncol(output_mcmc_black_ca_2018) <= 1) 
  output_mcmc_black_ca_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                       1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_ca_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                                  output_mcmc_black_ca_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_ca_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_ca_2018)) if (quantile(tab_black_ca_2018[, j], probs = 0.975) > 
                                           ymax) 
    ymax = quantile(tab_black_ca_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_ca_2018)) if (quantile(tab_black_ca_2018[, j], probs = 0.025) < 
                                           ymin) 
    ymin = quantile(tab_black_ca_2018[, j], probs = 0.025)
}

#MERLI sn
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
nomi_parametri = c("B0_sn")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_sn_2018 = read.table(nome_file_parametri)
output_mcmc_black_sn_2018 = output_mcmc_black_sn_2018[,c(4,13)]
burnin = nrow(output_mcmc_black_sn_2018) * 0.1
if (ncol(output_mcmc_black_sn_2018) <= 1) 
  output_mcmc_black_sn_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                       1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_sn_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                                  output_mcmc_black_sn_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_sn_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_sn_2018)) if (quantile(tab_black_sn_2018[, j], probs = 0.975) > 
                                           ymax) 
    ymax = quantile(tab_black_sn_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_sn_2018)) if (quantile(tab_black_sn_2018[, j], probs = 0.025) < 
                                           ymin) 
    ymin = quantile(tab_black_sn_2018[, j], probs = 0.025)
}

#GAZZE ca
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
nomi_parametri = c("B0_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_ca_2018 = read.table(nome_file_parametri)
output_mcmc_mag_ca_2018 = output_mcmc_mag_ca_2018[,c(3,13)]
burnin = nrow(output_mcmc_mag_ca_2018) * 0.1
if (ncol(output_mcmc_mag_ca_2018) <= 1) 
  output_mcmc_mag_ca_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                     1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_ca_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                                output_mcmc_mag_ca_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_ca_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_ca_2018)) if (quantile(tab_mag_ca_2018[, j], probs = 0.975) > 
                                         ymax) 
    ymax = quantile(tab_mag_ca_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_ca_2018)) if (quantile(tab_mag_ca_2018[, j], probs = 0.025) < 
                                         ymin) 
    ymin = quantile(tab_mag_ca_2018[, j], probs = 0.025)
}

#GAZZE sn
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
nomi_parametri = c("B0_sn")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_sn_2018 = read.table(nome_file_parametri)
output_mcmc_mag_sn_2018 = output_mcmc_mag_sn_2018[,c(4,13)]
burnin = nrow(output_mcmc_mag_sn_2018) * 0.1
if (ncol(output_mcmc_mag_sn_2018) <= 1) 
  output_mcmc_mag_sn_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                     1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_sn_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                                output_mcmc_mag_sn_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_sn_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_sn_2018)) if (quantile(tab_mag_sn_2018[, j], probs = 0.975) > 
                                         ymax) 
    ymax = quantile(tab_mag_sn_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_sn_2018)) if (quantile(tab_mag_sn_2018[, j], probs = 0.025) < 
                                         ymin) 
    ymin = quantile(tab_mag_sn_2018[, j], probs = 0.025)
}

#GERMANI ca
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
nomi_parametri = c("B0_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_ca_2018 = read.table(nome_file_parametri)
output_mcmc_mall_ca_2018 = output_mcmc_mall_ca_2018[,c(3,13)]
burnin = nrow(output_mcmc_mall_ca_2018) * 0.1
if (ncol(output_mcmc_mall_ca_2018) <= 1) 
  output_mcmc_mall_ca_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                      1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_ca_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                                 output_mcmc_mall_ca_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_ca_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_ca_2018)) if (quantile(tab_mall_ca_2018[, j], probs = 0.975) > 
                                          ymax) 
    ymax = quantile(tab_mall_ca_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_ca_2018)) if (quantile(tab_mall_ca_2018[, j], probs = 0.025) < 
                                          ymin) 
    ymin = quantile(tab_mall_ca_2018[, j], probs = 0.025)
}

#GERMANI sn
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
nomi_parametri = c("B0_sn")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_sn_2018 = read.table(nome_file_parametri)
output_mcmc_mall_sn_2018 = output_mcmc_mall_sn_2018[,c(4,13)]
burnin = nrow(output_mcmc_mall_sn_2018) * 0.1
if (ncol(output_mcmc_mall_sn_2018) <= 1) 
  output_mcmc_mall_sn_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                      1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_sn_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                                 output_mcmc_mall_sn_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_sn_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_sn_2018)) if (quantile(tab_mall_sn_2018[, j], probs = 0.975) > 
                                          ymax) 
    ymax = quantile(tab_mall_sn_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_sn_2018)) if (quantile(tab_mall_sn_2018[, j], probs = 0.025) < 
                                          ymin) 
    ymin = quantile(tab_mall_sn_2018[, j], probs = 0.025)
}

initial_number_of_ca_birds_2018 <- as.data.frame(cbind(tab_black_ca_2018,
                                                       tab_mag_ca_2018,
                                                       tab_mall_ca_2018))
colnames(initial_number_of_ca_birds_2018) <- c("blackbirds","magpies","mallards")


boxplot(initial_number_of_ca_birds_2018, ylim= c(0,100), bty = "l", ylab = "Initial number of birds",
        xlab = "", main = "Initial number of AC birds 2018", col = c("#D53E4F", "#FDAE61","#ABDDA4"))


initial_number_of_sn_birds_2018 <- as.data.frame(cbind(tab_black_sn_2018,
                                                       tab_mag_sn_2018,
                                                       tab_mall_sn_2018))
colnames(initial_number_of_sn_birds_2018) <- c("blackbirds","magpies","mallards")


boxplot(initial_number_of_sn_birds_2018, ylim= c(0,100), bty = "l", ylab = "Initial number of birds",
        xlab = "", main = "Initial number of birds 2018", col = c("#9E0142", "#F46D43","#66C2A5"))


initial_number_of_ca_sn_birds_2018 <- as.data.frame(cbind(tab_black_ca_2018,tab_black_sn_2018,
                                                          tab_mag_ca_2018,tab_mag_sn_2018,
                                                          tab_mall_ca_2018,tab_mall_sn_2018))
colnames(initial_number_of_ca_sn_birds_2018) <- c("AC","blackbirds",
                                                  "AC","magpies",
                                                  "AC","mallards")


boxplot(initial_number_of_ca_sn_birds_2018, ylim= c(0,100), bty = "l", ylab = "Initial number of birds",
        xlab = "", main = "Initial number of birds 2018", col = c("#D53E4F","#9E0142",
                                                                  "#FDAE61","#F46D43",
                                                                  "#ABDDA4","#66C2A5"))



#muB 2016----
#MERLI
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
nomi_parametri = c("muB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_2016 = read.table(nome_file_parametri)
output_mcmc_black_2016 = output_mcmc_black_2016[,c(8,13)]
burnin = nrow(output_mcmc_black_2016) * 0.1
if (ncol(output_mcmc_black_2016) <= 1) 
  output_mcmc_black_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                    1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                               output_mcmc_black_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_2016)) if (quantile(tab_black_2016[, j], probs = 0.975) > 
                                        ymax) 
    ymax = quantile(tab_black_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_2016)) if (quantile(tab_black_2016[, j], probs = 0.025) < 
                                        ymin) 
    ymin = quantile(tab_black_2016[, j], probs = 0.025)
}

#GAZZE
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
nomi_parametri = c("muB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_2016 = read.table(nome_file_parametri)
output_mcmc_mag_2016 = output_mcmc_mag_2016[,c(8,13)]
burnin = nrow(output_mcmc_mag_2016) * 0.1
if (ncol(output_mcmc_mag_2016) <= 1) 
  output_mcmc_mag_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                  1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_mag_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_2016)) if (quantile(tab_mag_2016[, j], probs = 0.975) > 
                                      ymax) 
    ymax = quantile(tab_mag_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_2016)) if (quantile(tab_mag_2016[, j], probs = 0.025) < 
                                      ymin) 
    ymin = quantile(tab_mag_2016[, j], probs = 0.025)
}

#GERMANI
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
nomi_parametri = c("muB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_2016 = read.table(nome_file_parametri)
output_mcmc_mall_2016 = output_mcmc_mall_2016[,c(8,13)]
burnin = nrow(output_mcmc_mall_2016) * 0.1
if (ncol(output_mcmc_mall_2016) <= 1) 
  output_mcmc_mall_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                   1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                              output_mcmc_mall_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_2016)) if (quantile(tab_mall_2016[, j], probs = 0.975) > 
                                       ymax) 
    ymax = quantile(tab_mall_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_2016)) if (quantile(tab_mall_2016[, j], probs = 0.025) < 
                                       ymin) 
    ymin = quantile(tab_mall_2016[, j], probs = 0.025)
}


muB_2016 <- as.data.frame(cbind(tab_black_2016, tab_mag_2016, tab_mall_2016))
colnames(muB_2016) <- c("blackbirds","magpies","mallards")

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
boxplot(muB_2016, ylim= c(0,0.02), bty = "l", ylab = "number of eggs laid",
        xlab = "", main = "number of eggs laid 2016", col = c("#D53E4F","#FDAE61","#ABDDA4"))
abline(h = c(0.013,0.0075,0.014), lwd = 2, col = c("#9E0142","#F46D43","#66C2A5"), lty = 2)

#muB 2017----
#MERLI
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
nomi_parametri = c("muB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_2017 = read.table(nome_file_parametri)
output_mcmc_black_2017 = output_mcmc_black_2017[,c(8,13)]
burnin = nrow(output_mcmc_black_2017) * 0.1
if (ncol(output_mcmc_black_2017) <= 1) 
  output_mcmc_black_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                    1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                               output_mcmc_black_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_2017)) if (quantile(tab_black_2017[, j], probs = 0.975) > 
                                        ymax) 
    ymax = quantile(tab_black_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_2017)) if (quantile(tab_black_2017[, j], probs = 0.025) < 
                                        ymin) 
    ymin = quantile(tab_black_2017[, j], probs = 0.025)
}

#GAZZE
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
nomi_parametri = c("muB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_2017 = read.table(nome_file_parametri)
output_mcmc_mag_2017 = output_mcmc_mag_2017[,c(8,13)]
burnin = nrow(output_mcmc_mag_2017) * 0.1
if (ncol(output_mcmc_mag_2017) <= 1) 
  output_mcmc_mag_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                  1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_mag_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_2017)) if (quantile(tab_mag_2017[, j], probs = 0.975) > 
                                      ymax) 
    ymax = quantile(tab_mag_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_2017)) if (quantile(tab_mag_2017[, j], probs = 0.025) < 
                                      ymin) 
    ymin = quantile(tab_mag_2017[, j], probs = 0.025)
}

#GERMANI
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
nomi_parametri = c("muB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_2017 = read.table(nome_file_parametri)
output_mcmc_mall_2017 = output_mcmc_mall_2017[,c(8,13)]
burnin = nrow(output_mcmc_mall_2017) * 0.1
if (ncol(output_mcmc_mall_2017) <= 1) 
  output_mcmc_mall_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                   1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                              output_mcmc_mall_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_2017)) if (quantile(tab_mall_2017[, j], probs = 0.975) > 
                                       ymax) 
    ymax = quantile(tab_mall_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_2017)) if (quantile(tab_mall_2017[, j], probs = 0.025) < 
                                       ymin) 
    ymin = quantile(tab_mall_2017[, j], probs = 0.025)
}


muB_2017 <- as.data.frame(cbind(tab_black_2017, tab_mag_2017, tab_mall_2017))
colnames(muB_2017) <- c("blackbirds","magpies","mallards")

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
boxplot(muB_2017, ylim= c(0,0.02), bty = "l", ylab = "number of eggs laid",
        xlab = "", main = "number of eggs laid 2017", col = c("#D53E4F","#FDAE61","#ABDDA4"))
abline(h = c(0.013,0.0075,0.014), lwd = 2, col = c("#9E0142","#F46D43","#66C2A5"), lty = 2)

#muB 2018----
#MERLI
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
nomi_parametri = c("muB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_2018 = read.table(nome_file_parametri)
output_mcmc_black_2018 = output_mcmc_black_2018[,c(8,13)]
burnin = nrow(output_mcmc_black_2018) * 0.1
if (ncol(output_mcmc_black_2018) <= 1) 
  output_mcmc_black_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                    1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                               output_mcmc_black_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_2018)) if (quantile(tab_black_2018[, j], probs = 0.975) > 
                                        ymax) 
    ymax = quantile(tab_black_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_2018)) if (quantile(tab_black_2018[, j], probs = 0.025) < 
                                        ymin) 
    ymin = quantile(tab_black_2018[, j], probs = 0.025)
}

#GAZZE
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
nomi_parametri = c("muB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_2018 = read.table(nome_file_parametri)
output_mcmc_mag_2018 = output_mcmc_mag_2018[,c(8,13)]
burnin = nrow(output_mcmc_mag_2018) * 0.1
if (ncol(output_mcmc_mag_2018) <= 1) 
  output_mcmc_mag_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                  1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_mag_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_2018)) if (quantile(tab_mag_2018[, j], probs = 0.975) > 
                                      ymax) 
    ymax = quantile(tab_mag_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_2018)) if (quantile(tab_mag_2018[, j], probs = 0.025) < 
                                      ymin) 
    ymin = quantile(tab_mag_2018[, j], probs = 0.025)
}

#GERMANI
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
nomi_parametri = c("muB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_2018 = read.table(nome_file_parametri)
output_mcmc_mall_2018 = output_mcmc_mall_2018[,c(8,13)]
burnin = nrow(output_mcmc_mall_2018) * 0.1
if (ncol(output_mcmc_mall_2018) <= 1) 
  output_mcmc_mall_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                   1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                              output_mcmc_mall_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_2018)) if (quantile(tab_mall_2018[, j], probs = 0.975) > 
                                       ymax) 
    ymax = quantile(tab_mall_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_2018)) if (quantile(tab_mall_2018[, j], probs = 0.025) < 
                                       ymin) 
    ymin = quantile(tab_mall_2018[, j], probs = 0.025)
}


muB_2018 <- as.data.frame(cbind(tab_black_2018, tab_mag_2018, tab_mall_2018))
colnames(muB_2018) <- c("blackbirds","magpies","mallards")

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
boxplot(muB_2018, ylim= c(0,0.025), bty = "l", ylab = "number of eggs laid",
        xlab = "", main = "number of eggs laid 2018", col = c("#D53E4F","#FDAE61","#ABDDA4"))
abline(h = c(0.013,0.0075,0.014), lwd = 2, col = c("#9E0142","#F46D43","#66C2A5"), lty = 2)


#s 2016----
#MERLI
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
nomi_parametri = c("s_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_2016 = read.table(nome_file_parametri)
output_mcmc_black_2016 = output_mcmc_black_2016[,c(9,13)]
burnin = nrow(output_mcmc_black_2016) * 0.1
if (ncol(output_mcmc_black_2016) <= 1) 
  output_mcmc_black_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                    1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                               output_mcmc_black_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_2016)) if (quantile(tab_black_2016[, j], probs = 0.975) > 
                                        ymax) 
    ymax = quantile(tab_black_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_2016)) if (quantile(tab_black_2016[, j], probs = 0.025) < 
                                        ymin) 
    ymin = quantile(tab_black_2016[, j], probs = 0.025)
}

#GAZZE
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
nomi_parametri = c("s_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_2016 = read.table(nome_file_parametri)
output_mcmc_mag_2016 = output_mcmc_mag_2016[,c(9,13)]
burnin = nrow(output_mcmc_mag_2016) * 0.1
if (ncol(output_mcmc_mag_2016) <= 1) 
  output_mcmc_mag_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                  1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_mag_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_2016)) if (quantile(tab_mag_2016[, j], probs = 0.975) > 
                                      ymax) 
    ymax = quantile(tab_mag_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_2016)) if (quantile(tab_mag_2016[, j], probs = 0.025) < 
                                      ymin) 
    ymin = quantile(tab_mag_2016[, j], probs = 0.025)
}

#GERMANI
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
nomi_parametri = c("s_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_2016 = read.table(nome_file_parametri)
output_mcmc_mall_2016 = output_mcmc_mall_2016[,c(9,13)]
burnin = nrow(output_mcmc_mall_2016) * 0.1
if (ncol(output_mcmc_mall_2016) <= 1) 
  output_mcmc_mall_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                   1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                              output_mcmc_mall_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_2016)) if (quantile(tab_mall_2016[, j], probs = 0.975) > 
                                       ymax) 
    ymax = quantile(tab_mall_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_2016)) if (quantile(tab_mall_2016[, j], probs = 0.025) < 
                                       ymin) 
    ymin = quantile(tab_mall_2016[, j], probs = 0.025)
}

s_2016 <- as.data.frame(cbind(tab_black_2016, tab_mag_2016, tab_mall_2016))
colnames(s_2016) <- c("blackbirds","magpies","mallards")

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
boxplot(s_2016, ylim= c(5,20), bty = "l", ylab = "birth synchronization",
        xlab = "", main = "birth synchronization 2016", col = c("#D53E4F","#FDAE61","#ABDDA4"))
abline(h = c(6,6,10), lwd = 2, col = c("#9E0142","#F46D43","#66C2A5"), lty = 2)

#s 2017----
#MERLI
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
nomi_parametri = c("s_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_2017 = read.table(nome_file_parametri)
output_mcmc_black_2017 = output_mcmc_black_2017[,c(9,13)]
burnin = nrow(output_mcmc_black_2017) * 0.1
if (ncol(output_mcmc_black_2017) <= 1) 
  output_mcmc_black_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                    1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                               output_mcmc_black_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_2017)) if (quantile(tab_black_2017[, j], probs = 0.975) > 
                                        ymax) 
    ymax = quantile(tab_black_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_2017)) if (quantile(tab_black_2017[, j], probs = 0.025) < 
                                        ymin) 
    ymin = quantile(tab_black_2017[, j], probs = 0.025)
}

#GAZZE
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
nomi_parametri = c("s_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_2017 = read.table(nome_file_parametri)
output_mcmc_mag_2017 = output_mcmc_mag_2017[,c(9,13)]
burnin = nrow(output_mcmc_mag_2017) * 0.1
if (ncol(output_mcmc_mag_2017) <= 1) 
  output_mcmc_mag_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                  1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_mag_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_2017)) if (quantile(tab_mag_2017[, j], probs = 0.975) > 
                                      ymax) 
    ymax = quantile(tab_mag_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_2017)) if (quantile(tab_mag_2017[, j], probs = 0.025) < 
                                      ymin) 
    ymin = quantile(tab_mag_2017[, j], probs = 0.025)
}

#GERMANI
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
nomi_parametri = c("s_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_2017 = read.table(nome_file_parametri)
output_mcmc_mall_2017 = output_mcmc_mall_2017[,c(9,13)]
burnin = nrow(output_mcmc_mall_2017) * 0.1
if (ncol(output_mcmc_mall_2017) <= 1) 
  output_mcmc_mall_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                   1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                              output_mcmc_mall_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_2017)) if (quantile(tab_mall_2017[, j], probs = 0.975) > 
                                       ymax) 
    ymax = quantile(tab_mall_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_2017)) if (quantile(tab_mall_2017[, j], probs = 0.025) < 
                                       ymin) 
    ymin = quantile(tab_mall_2017[, j], probs = 0.025)
}

s_2017 <- as.data.frame(cbind(tab_black_2017, tab_mag_2017, tab_mall_2017))
colnames(s_2017) <- c("blackbirds","magpies","mallards")


boxplot(s_2017, ylim= c(5,20), bty = "l", ylab = "birth synchronization",
        xlab = "", main = "birth synchronization 2017", col = c("#D53E4F","#FDAE61","#ABDDA4"))
abline(h = c(6,6,10), lwd = 2, col = c("#9E0142","#F46D43","#66C2A5"), lty = 2)

#s 2018----
#MERLI
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
nomi_parametri = c("s_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_2018 = read.table(nome_file_parametri)
output_mcmc_black_2018 = output_mcmc_black_2018[,c(9,13)]
burnin = nrow(output_mcmc_black_2018) * 0.1
if (ncol(output_mcmc_black_2018) <= 1) 
  output_mcmc_black_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                    1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                               output_mcmc_black_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_2018)) if (quantile(tab_black_2018[, j], probs = 0.975) > 
                                        ymax) 
    ymax = quantile(tab_black_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_2018)) if (quantile(tab_black_2018[, j], probs = 0.025) < 
                                        ymin) 
    ymin = quantile(tab_black_2018[, j], probs = 0.025)
}

#GAZZE
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
nomi_parametri = c("s_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_2018 = read.table(nome_file_parametri)
output_mcmc_mag_2018 = output_mcmc_mag_2018[,c(9,13)]
burnin = nrow(output_mcmc_mag_2018) * 0.1
if (ncol(output_mcmc_mag_2018) <= 1) 
  output_mcmc_mag_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                  1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_mag_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_2018)) if (quantile(tab_mag_2018[, j], probs = 0.975) > 
                                      ymax) 
    ymax = quantile(tab_mag_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_2018)) if (quantile(tab_mag_2018[, j], probs = 0.025) < 
                                      ymin) 
    ymin = quantile(tab_mag_2018[, j], probs = 0.025)
}

#GERMANI
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
nomi_parametri = c("s_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_2018 = read.table(nome_file_parametri)
output_mcmc_mall_2018 = output_mcmc_mall_2018[,c(9,13)]
burnin = nrow(output_mcmc_mall_2018) * 0.1
if (ncol(output_mcmc_mall_2018) <= 1) 
  output_mcmc_mall_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                   1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                              output_mcmc_mall_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_2018)) if (quantile(tab_mall_2018[, j], probs = 0.975) > 
                                       ymax) 
    ymax = quantile(tab_mall_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_2018)) if (quantile(tab_mall_2018[, j], probs = 0.025) < 
                                       ymin) 
    ymin = quantile(tab_mall_2018[, j], probs = 0.025)
}

s_2018 <- as.data.frame(cbind(tab_black_2018, tab_mag_2018, tab_mall_2018))
colnames(s_2018) <- c("blackbirds","magpies","mallards")

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
boxplot(s_2018, ylim= c(5,20), bty = "l", ylab = "birth synchronization",
        xlab = "", main = "birth synchronization 2018", col = c("#D53E4F","#FDAE61","#ABDDA4"))
abline(h = c(6,6,10), lwd = 2, col = c("#9E0142","#F46D43","#66C2A5"), lty = 2)


#phi 2016----
#MERLI
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
nomi_parametri = c("phi_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_2016 = read.table(nome_file_parametri)
output_mcmc_black_2016 = output_mcmc_black_2016[,c(10,13)]
burnin = nrow(output_mcmc_black_2016) * 0.1
if (ncol(output_mcmc_black_2016) <= 1) 
  output_mcmc_black_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                    1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                               output_mcmc_black_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_2016)) if (quantile(tab_black_2016[, j], probs = 0.975) > 
                                        ymax) 
    ymax = quantile(tab_black_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_2016)) if (quantile(tab_black_2016[, j], probs = 0.025) < 
                                        ymin) 
    ymin = quantile(tab_black_2016[, j], probs = 0.025)
}

#GAZZE
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
nomi_parametri = c("phi_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_2016 = read.table(nome_file_parametri)
output_mcmc_mag_2016 = output_mcmc_mag_2016[,c(10,13)]
burnin = nrow(output_mcmc_mag_2016) * 0.1
if (ncol(output_mcmc_mag_2016) <= 1) 
  output_mcmc_mag_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                  1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_mag_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_2016)) if (quantile(tab_mag_2016[, j], probs = 0.975) > 
                                      ymax) 
    ymax = quantile(tab_mag_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_2016)) if (quantile(tab_mag_2016[, j], probs = 0.025) < 
                                      ymin) 
    ymin = quantile(tab_mag_2016[, j], probs = 0.025)
}

#GERMANI
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
nomi_parametri = c("phi_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_2016 = read.table(nome_file_parametri)
output_mcmc_mall_2016 = output_mcmc_mall_2016[,c(10,13)]
burnin = nrow(output_mcmc_mall_2016) * 0.1
if (ncol(output_mcmc_mall_2016) <= 1) 
  output_mcmc_mall_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                   1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                              output_mcmc_mall_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_2016)) if (quantile(tab_mall_2016[, j], probs = 0.975) > 
                                       ymax) 
    ymax = quantile(tab_mall_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_2016)) if (quantile(tab_mall_2016[, j], probs = 0.025) < 
                                       ymin) 
    ymin = quantile(tab_mall_2016[, j], probs = 0.025)
}

phi_2016 <- as.data.frame(cbind(tab_black_2016, tab_mag_2016, tab_mall_2016))
colnames(phi_2016) <- c("blackbirds","magpies","mallards")

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

boxplot(phi_2016, ylim= c(0.33,0.67), bty = "l", ylab = "time of birth",
        xlab = "years", main = "time of birth 2016", col = c("#D53E4F","#FDAE61","#ABDDA4"))
abline(h = c(0.35,0.4,0.33), lwd = 2, col = c("#9E0142","#F46D43","#66C2A5"), lty = 2)

#phi 2017----
#MERLI
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
nomi_parametri = c("phi_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_2017 = read.table(nome_file_parametri)
output_mcmc_black_2017 = output_mcmc_black_2017[,c(10,13)]
burnin = nrow(output_mcmc_black_2017) * 0.1
if (ncol(output_mcmc_black_2017) <= 1) 
  output_mcmc_black_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                    1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                               output_mcmc_black_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_2017)) if (quantile(tab_black_2017[, j], probs = 0.975) > 
                                        ymax) 
    ymax = quantile(tab_black_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_2017)) if (quantile(tab_black_2017[, j], probs = 0.025) < 
                                        ymin) 
    ymin = quantile(tab_black_2017[, j], probs = 0.025)
}

#GAZZE
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
nomi_parametri = c("phi_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_2017 = read.table(nome_file_parametri)
output_mcmc_mag_2017 = output_mcmc_mag_2017[,c(10,13)]
burnin = nrow(output_mcmc_mag_2017) * 0.1
if (ncol(output_mcmc_mag_2017) <= 1) 
  output_mcmc_mag_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                  1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_mag_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_2017)) if (quantile(tab_mag_2017[, j], probs = 0.975) > 
                                      ymax) 
    ymax = quantile(tab_mag_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_2017)) if (quantile(tab_mag_2017[, j], probs = 0.025) < 
                                      ymin) 
    ymin = quantile(tab_mag_2017[, j], probs = 0.025)
}

#GERMANI
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
nomi_parametri = c("phi_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_2017 = read.table(nome_file_parametri)
output_mcmc_mall_2017 = output_mcmc_mall_2017[,c(10,13)]
burnin = nrow(output_mcmc_mall_2017) * 0.1
if (ncol(output_mcmc_mall_2017) <= 1) 
  output_mcmc_mall_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                   1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                              output_mcmc_mall_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_2017)) if (quantile(tab_mall_2017[, j], probs = 0.975) > 
                                       ymax) 
    ymax = quantile(tab_mall_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_2017)) if (quantile(tab_mall_2017[, j], probs = 0.025) < 
                                       ymin) 
    ymin = quantile(tab_mall_2017[, j], probs = 0.025)
}

phi_2017 <- as.data.frame(cbind(tab_black_2017, tab_mag_2017, tab_mall_2017))
colnames(phi_2017) <- c("blackbirds","magpies","mallards")

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
boxplot(phi_2017, ylim= c(0.33,0.67), bty = "l", ylab = "time of birth",
        xlab = "", main = "time of birth 2017", col = c("#D53E4F" ,"#FDAE61" ,"#ABDDA4"))
abline(h = c(0.35,0.4,0.33), lwd = 2, col = c("#9E0142","#F46D43","#66C2A5" ), lty = 2)

#phi 2018----
#MERLI
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
nomi_parametri = c("phi_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_2018 = read.table(nome_file_parametri)
output_mcmc_black_2018 = output_mcmc_black_2018[,c(10,13)]
burnin = nrow(output_mcmc_black_2018) * 0.1
if (ncol(output_mcmc_black_2018) <= 1) 
  output_mcmc_black_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                    1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                               output_mcmc_black_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_2018)) if (quantile(tab_black_2018[, j], probs = 0.975) > 
                                        ymax) 
    ymax = quantile(tab_black_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_2018)) if (quantile(tab_black_2018[, j], probs = 0.025) < 
                                        ymin) 
    ymin = quantile(tab_black_2018[, j], probs = 0.025)
}

#GAZZE
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
nomi_parametri = c("phi_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_2018 = read.table(nome_file_parametri)
output_mcmc_mag_2018 = output_mcmc_mag_2018[,c(10,13)]
burnin = nrow(output_mcmc_mag_2018) * 0.1
if (ncol(output_mcmc_mag_2018) <= 1) 
  output_mcmc_mag_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                  1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_mag_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_2018)) if (quantile(tab_mag_2018[, j], probs = 0.975) > 
                                      ymax) 
    ymax = quantile(tab_mag_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_2018)) if (quantile(tab_mag_2018[, j], probs = 0.025) < 
                                      ymin) 
    ymin = quantile(tab_mag_2018[, j], probs = 0.025)
}

#GERMANI
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
nomi_parametri = c("phi_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_2018 = read.table(nome_file_parametri)
output_mcmc_mall_2018 = output_mcmc_mall_2018[,c(10,13)]
burnin = nrow(output_mcmc_mall_2018) * 0.1
if (ncol(output_mcmc_mall_2018) <= 1) 
  output_mcmc_mall_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                   1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                              output_mcmc_mall_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_2018)) if (quantile(tab_mall_2018[, j], probs = 0.975) > 
                                       ymax) 
    ymax = quantile(tab_mall_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_2018)) if (quantile(tab_mall_2018[, j], probs = 0.025) < 
                                       ymin) 
    ymin = quantile(tab_mall_2018[, j], probs = 0.025)
}

phi_2018 <- as.data.frame(cbind(tab_black_2018, tab_mag_2018, tab_mall_2018))
colnames(phi_2018) <- c("blackbirds","magpies","mallards")

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
boxplot(phi_2018, ylim= c(0.33,0.67), bty = "l", ylab = "time of birth",
        xlab = "", main = "time of birth 2018", col = c("#D53E4F" ,"#FDAE61" ,"#ABDDA4"))
abline(h = c(0.35,0.4,0.33), lwd = 2, col = c("#9E0142","#F46D43","#66C2A5" ), lty = 2)


#niB 2016----
#MERLI
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
nomi_parametri = c("niB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_2016 = read.table(nome_file_parametri)
output_mcmc_black_2016 = output_mcmc_black_2016[,c(11,13)]
burnin = nrow(output_mcmc_black_2016) * 0.1
if (ncol(output_mcmc_black_2016) <= 1) 
  output_mcmc_black_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                    1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                               output_mcmc_black_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_2016)) if (quantile(tab_black_2016[, j], probs = 0.975) > 
                                        ymax) 
    ymax = quantile(tab_black_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_2016)) if (quantile(tab_black_2016[, j], probs = 0.025) < 
                                        ymin) 
    ymin = quantile(tab_black_2016[, j], probs = 0.025)
}

#GAZZE
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
nomi_parametri = c("niB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_2016 = read.table(nome_file_parametri)
output_mcmc_mag_2016 = output_mcmc_mag_2016[,c(11,13)]
burnin = nrow(output_mcmc_mag_2016) * 0.1
if (ncol(output_mcmc_mag_2016) <= 1) 
  output_mcmc_mag_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                  1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_mag_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_2016)) if (quantile(tab_mag_2016[, j], probs = 0.975) > 
                                      ymax) 
    ymax = quantile(tab_mag_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_2016)) if (quantile(tab_mag_2016[, j], probs = 0.025) < 
                                      ymin) 
    ymin = quantile(tab_mag_2016[, j], probs = 0.025)
}

#GERMANI
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
nomi_parametri = c("niB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_2016 = read.table(nome_file_parametri)
output_mcmc_mall_2016 = output_mcmc_mall_2016[,c(11,13)]
burnin = nrow(output_mcmc_mall_2016) * 0.1
if (ncol(output_mcmc_mall_2016) <= 1) 
  output_mcmc_mall_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                   1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                              output_mcmc_mall_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_2016)) if (quantile(tab_mall_2016[, j], probs = 0.975) > 
                                       ymax) 
    ymax = quantile(tab_mall_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_2016)) if (quantile(tab_mall_2016[, j], probs = 0.025) < 
                                       ymin) 
    ymin = quantile(tab_mall_2016[, j], probs = 0.025)
}

niB_2016 <- as.data.frame(cbind(tab_black_2016, tab_mag_2016, tab_mall_2016))
colnames(niB_2016) <- c("blackbirds","magpies","mallards")

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
boxplot(niB_2016, ylim= c(0.3,1.03), bty = "l", ylab = "infection rate",
        xlab = "", main = "infection rate 2016", col = c("#D53E4F" ,"#FDAE61" ,"#ABDDA4"))
abline(h = c(1,0.66,0.66), lwd = 2, col = c("#9E0142","#F46D43","#66C2A5" ), lty = 2)

#niB 2017----
#MERLI
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
nomi_parametri = c("niB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_2017 = read.table(nome_file_parametri)
output_mcmc_black_2017 = output_mcmc_black_2017[,c(11,13)]
burnin = nrow(output_mcmc_black_2017) * 0.1
if (ncol(output_mcmc_black_2017) <= 1) 
  output_mcmc_black_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                    1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                               output_mcmc_black_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_2017)) if (quantile(tab_black_2017[, j], probs = 0.975) > 
                                        ymax) 
    ymax = quantile(tab_black_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_2017)) if (quantile(tab_black_2017[, j], probs = 0.025) < 
                                        ymin) 
    ymin = quantile(tab_black_2017[, j], probs = 0.025)
}

#GAZZE
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
nomi_parametri = c("niB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_2017 = read.table(nome_file_parametri)
output_mcmc_mag_2017 = output_mcmc_mag_2017[,c(11,13)]
burnin = nrow(output_mcmc_mag_2017) * 0.1
if (ncol(output_mcmc_mag_2017) <= 1) 
  output_mcmc_mag_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                  1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_mag_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_2017)) if (quantile(tab_mag_2017[, j], probs = 0.975) > 
                                      ymax) 
    ymax = quantile(tab_mag_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_2017)) if (quantile(tab_mag_2017[, j], probs = 0.025) < 
                                      ymin) 
    ymin = quantile(tab_mag_2017[, j], probs = 0.025)
}

#GERMANI
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
nomi_parametri = c("niB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_2017 = read.table(nome_file_parametri)
output_mcmc_mall_2017 = output_mcmc_mall_2017[,c(11,13)]
burnin = nrow(output_mcmc_mall_2017) * 0.1
if (ncol(output_mcmc_mall_2017) <= 1) 
  output_mcmc_mall_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                   1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                              output_mcmc_mall_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_2017)) if (quantile(tab_mall_2017[, j], probs = 0.975) > 
                                       ymax) 
    ymax = quantile(tab_mall_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_2017)) if (quantile(tab_mall_2017[, j], probs = 0.025) < 
                                       ymin) 
    ymin = quantile(tab_mall_2017[, j], probs = 0.025)
}

niB_2017 <- as.data.frame(cbind(tab_black_2017, tab_mag_2017, tab_mall_2017))
colnames(niB_2017) <- c("blackbirds","magpies","mallards")


boxplot(niB_2017, ylim= c(0.3,1.03), bty = "l", ylab = "infection rate",
        xlab = "", main = "infection rate 2017", col = c("#D53E4F" ,"#FDAE61" ,"#ABDDA4"))
abline(h = c(1,0.66,0.66), lwd = 2, col = c("#9E0142","#F46D43","#66C2A5" ), lty = 2)

#niB 2018----
#MERLI
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
nomi_parametri = c("niB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_2018 = read.table(nome_file_parametri)
output_mcmc_black_2018 = output_mcmc_black_2018[,c(11,13)]
burnin = nrow(output_mcmc_black_2018) * 0.1
if (ncol(output_mcmc_black_2018) <= 1) 
  output_mcmc_black_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                    1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                               output_mcmc_black_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_2018)) if (quantile(tab_black_2018[, j], probs = 0.975) > 
                                        ymax) 
    ymax = quantile(tab_black_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_2018)) if (quantile(tab_black_2018[, j], probs = 0.025) < 
                                        ymin) 
    ymin = quantile(tab_black_2018[, j], probs = 0.025)
}

#GAZZE
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
nomi_parametri = c("niB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_2018 = read.table(nome_file_parametri)
output_mcmc_mag_2018 = output_mcmc_mag_2018[,c(11,13)]
burnin = nrow(output_mcmc_mag_2018) * 0.1
if (ncol(output_mcmc_mag_2018) <= 1) 
  output_mcmc_mag_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                  1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_mag_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_2018)) if (quantile(tab_mag_2018[, j], probs = 0.975) > 
                                      ymax) 
    ymax = quantile(tab_mag_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_2018)) if (quantile(tab_mag_2018[, j], probs = 0.025) < 
                                      ymin) 
    ymin = quantile(tab_mag_2018[, j], probs = 0.025)
}

#GERMANI
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
nomi_parametri = c("niB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_2018 = read.table(nome_file_parametri)
output_mcmc_mall_2018 = output_mcmc_mall_2018[,c(11,13)]
burnin = nrow(output_mcmc_mall_2018) * 0.1
if (ncol(output_mcmc_mall_2018) <= 1) 
  output_mcmc_mall_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                   1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                              output_mcmc_mall_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_2018)) if (quantile(tab_mall_2018[, j], probs = 0.975) > 
                                       ymax) 
    ymax = quantile(tab_mall_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_2018)) if (quantile(tab_mall_2018[, j], probs = 0.025) < 
                                       ymin) 
    ymin = quantile(tab_mall_2018[, j], probs = 0.025)
}

niB_2018 <- as.data.frame(cbind(tab_black_2018, tab_mag_2018, tab_mall_2018))
colnames(niB_2018) <- c("blackbirds","magpies","mallards")

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

boxplot(niB_2018, ylim= c(0.3,1.03), bty = "l", ylab = "infection rate",
        xlab = "", main = "infection rate 2018", col = c("#D53E4F" ,"#FDAE61" ,"#ABDDA4"))
abline(h = c(1,0.66,0.66), lwd = 2, col = c("#9E0142","#F46D43","#66C2A5" ), lty = 2)


#recB 2016----
#MERLI
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
nomi_parametri = c("recB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_2016 = read.table(nome_file_parametri)
output_mcmc_black_2016 = output_mcmc_black_2016[,c(12,13)]
burnin = nrow(output_mcmc_black_2016) * 0.1
if (ncol(output_mcmc_black_2016) <= 1) 
  output_mcmc_black_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                    1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                               output_mcmc_black_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_2016)) if (quantile(tab_black_2016[, j], probs = 0.975) > 
                                        ymax) 
    ymax = quantile(tab_black_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_2016)) if (quantile(tab_black_2016[, j], probs = 0.025) < 
                                        ymin) 
    ymin = quantile(tab_black_2016[, j], probs = 0.025)
}

#GAZZE
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
nomi_parametri = c("recB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_2016 = read.table(nome_file_parametri)
output_mcmc_mag_2016 = output_mcmc_mag_2016[,c(12,13)]
burnin = nrow(output_mcmc_mag_2016) * 0.1
if (ncol(output_mcmc_mag_2016) <= 1) 
  output_mcmc_mag_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                  1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_mag_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_2016)) if (quantile(tab_mag_2016[, j], probs = 0.975) > 
                                      ymax) 
    ymax = quantile(tab_mag_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_2016)) if (quantile(tab_mag_2016[, j], probs = 0.025) < 
                                      ymin) 
    ymin = quantile(tab_mag_2016[, j], probs = 0.025)
}

#GERMANI
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
nomi_parametri = c("recB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_2016 = read.table(nome_file_parametri)
output_mcmc_mall_2016 = output_mcmc_mall_2016[,c(12,13)]
burnin = nrow(output_mcmc_mall_2016) * 0.1
if (ncol(output_mcmc_mall_2016) <= 1) 
  output_mcmc_mall_2016 = matrix(0, ncol = length(nomi_parametri) + 
                                   1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                              output_mcmc_mall_2016[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_2016 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_2016)) if (quantile(tab_mall_2016[, j], probs = 0.975) > 
                                       ymax) 
    ymax = quantile(tab_mall_2016[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_2016)) if (quantile(tab_mall_2016[, j], probs = 0.025) < 
                                       ymin) 
    ymin = quantile(tab_mall_2016[, j], probs = 0.025)
}


recB_2016 <- as.data.frame(cbind(tab_black_2016, tab_mag_2016, tab_mall_2016))
colnames(recB_2016) <- c("blackbirds","magpies","mallards")


boxplot(recB_2016, ylim= c(0.05,0.3), bty = "l", ylab = "recovery rate",
        xlab = "", main = "recovery rate 2016", col = c("#D53E4F" ,"#FDAE61" ,"#ABDDA4"))
abline(h = c(0.22,0.2,0.25), lwd = 2, col = c("#9E0142","#F46D43","#66C2A5" ), lty = 2)

#recB 2017----
#MERLI
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
nomi_parametri = c("recB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_2017 = read.table(nome_file_parametri)
output_mcmc_black_2017 = output_mcmc_black_2017[,c(12,13)]
burnin = nrow(output_mcmc_black_2017) * 0.1
if (ncol(output_mcmc_black_2017) <= 1) 
  output_mcmc_black_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                    1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                               output_mcmc_black_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_2017)) if (quantile(tab_black_2017[, j], probs = 0.975) > 
                                        ymax) 
    ymax = quantile(tab_black_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_2017)) if (quantile(tab_black_2017[, j], probs = 0.025) < 
                                        ymin) 
    ymin = quantile(tab_black_2017[, j], probs = 0.025)
}

#GAZZE
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
nomi_parametri = c("recB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_2017 = read.table(nome_file_parametri)
output_mcmc_mag_2017 = output_mcmc_mag_2017[,c(12,13)]
burnin = nrow(output_mcmc_mag_2017) * 0.1
if (ncol(output_mcmc_mag_2017) <= 1) 
  output_mcmc_mag_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                  1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_mag_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_2017)) if (quantile(tab_mag_2017[, j], probs = 0.975) > 
                                      ymax) 
    ymax = quantile(tab_mag_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_2017)) if (quantile(tab_mag_2017[, j], probs = 0.025) < 
                                      ymin) 
    ymin = quantile(tab_mag_2017[, j], probs = 0.025)
}

#GERMANI
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
nomi_parametri = c("recB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_2017 = read.table(nome_file_parametri)
output_mcmc_mall_2017 = output_mcmc_mall_2017[,c(12,13)]
burnin = nrow(output_mcmc_mall_2017) * 0.1
if (ncol(output_mcmc_mall_2017) <= 1) 
  output_mcmc_mall_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                   1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                              output_mcmc_mall_2017[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_2017 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_2017)) if (quantile(tab_mall_2017[, j], probs = 0.975) > 
                                       ymax) 
    ymax = quantile(tab_mall_2017[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_2017)) if (quantile(tab_mall_2017[, j], probs = 0.025) < 
                                       ymin) 
    ymin = quantile(tab_mall_2017[, j], probs = 0.025)
}


recB_2017 <- as.data.frame(cbind(tab_black_2017, tab_mag_2017, tab_mall_2017))
colnames(recB_2017) <- c("blackbirds","magpies","mallards")

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
boxplot(recB_2017, ylim= c(0.05,0.35), bty = "l", ylab = "recovery rate",
        xlab = "", main = "recovery rate 2017", col = c("#D53E4F" ,"#FDAE61" ,"#ABDDA4"))
abline(h = c(0.22,0.2,0.25), lwd = 2, col = c("#9E0142","#F46D43","#66C2A5" ), lty = 2)

#recB 2018----
#MERLI
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
nomi_parametri = c("recB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_black_2018 = read.table(nome_file_parametri)
output_mcmc_black_2018 = output_mcmc_black_2018[,c(12,13)]
burnin = nrow(output_mcmc_black_2018) * 0.1
if (ncol(output_mcmc_black_2018) <= 1) 
  output_mcmc_black_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                    1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_black_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                               output_mcmc_black_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_black_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_black_2018)) if (quantile(tab_black_2018[, j], probs = 0.975) > 
                                        ymax) 
    ymax = quantile(tab_black_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_black_2018)) if (quantile(tab_black_2018[, j], probs = 0.025) < 
                                        ymin) 
    ymin = quantile(tab_black_2018[, j], probs = 0.025)
}

#GAZZE
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
nomi_parametri = c("recB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mag_2018 = read.table(nome_file_parametri)
output_mcmc_mag_2018 = output_mcmc_mag_2018[,c(12,13)]
burnin = nrow(output_mcmc_mag_2018) * 0.1
if (ncol(output_mcmc_mag_2018) <= 1) 
  output_mcmc_mag_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                  1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mag_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_mag_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mag_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mag_2018)) if (quantile(tab_mag_2018[, j], probs = 0.975) > 
                                      ymax) 
    ymax = quantile(tab_mag_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mag_2018)) if (quantile(tab_mag_2018[, j], probs = 0.025) < 
                                      ymin) 
    ymin = quantile(tab_mag_2018[, j], probs = 0.025)
}

#GERMANI
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
nomi_parametri = c("recB_ca")

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
}
for (quale_cluster in 1:numero_cluster) {
  if (con_cosa_inizio == 1) 
    nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                 "M_", anno, "_", quale_cluster, ".txt")
}
output_mcmc_mall_2018 = read.table(nome_file_parametri)
output_mcmc_mall_2018 = output_mcmc_mall_2018[,c(12,13)]
burnin = nrow(output_mcmc_mall_2018) * 0.1
if (ncol(output_mcmc_mall_2018) <= 1) 
  output_mcmc_mall_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                   1, nrow = max_iter_MCMC)
for (j in 1:(ncol(output_mcmc_mall_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                              output_mcmc_mall_2018[-c(1:burnin), j])

if (con_cosa_inizio == 1) 
  nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                     "M_", anno, "_", quale_cluster, ".jpg")


for (i in 1:length(parametri_stimati)) {
  tab_mall_2018 = parametri_stimati[[i]]
  ymax = 0
  for (j in 1:ncol(tab_mall_2018)) if (quantile(tab_mall_2018[, j], probs = 0.975) > 
                                       ymax) 
    ymax = quantile(tab_mall_2018[, j], probs = 0.975)
  ymin = ymax
  for (j in 1:ncol(tab_mall_2018)) if (quantile(tab_mall_2018[, j], probs = 0.025) < 
                                       ymin) 
    ymin = quantile(tab_mall_2018[, j], probs = 0.025)
}


recB_2018 <- as.data.frame(cbind(tab_black_2018, tab_mag_2018, tab_mall_2018))
colnames(recB_2018) <- c("blackbirds","magpies","mallards")


boxplot(recB_2018, ylim= c(0.05,0.35), bty = "l", ylab = "recovery rate",
        xlab = "", main = "recovery rate 2018", col = c("#D53E4F" ,"#FDAE61" ,"#ABDDA4"))
abline(h = c(0.22,0.2,0.25), lwd = 2, col = c("#9E0142","#F46D43","#66C2A5" ), lty = 2)



######################################DYNAMICS######################################
###MERLI----
what_plot = "known_bird_pop_rate"
#2016
anno_inizio = 2016
anno_fine = 2016
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Merli/Simulazioni/"
FileDynName = "dynamics_2spec_Merli_"
FolderPlotOut = "Output_WNV/Merli/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Merli_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Blackbirds"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#9e0142"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#9e0142"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#9e0142"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#9e0142"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#9e0142"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#9e0142"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#9e0142"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#9e0142"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#9e0142"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#D53E4F"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#D53E4F"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#D53E4F"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#D53E4F"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#D53E4F"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#D53E4F"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#D53E4F"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of blackbirds"
      colore = "#9e0142"
      main = paste("Blackbirds",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S blackbirds"
      colore = "#9e0142"
      main = paste("Susceptible blackbirds",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E blackbirds"
      colore = "#9e0142"
      main = paste("Exposed blackbirds",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I blackbirds"
      colore = "#9e0142"
      main = paste("Infected blackbirds",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R blackbirds"
      colore = "#9e0142"
      main = paste("Recovered blackbirds",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#9e0142"
      main = paste("Blackbirds prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#9e0142"
      main = paste("Blackbirds seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#9e0142"
      main = anno
    }
    
    
    qmax_pop_black_2016 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_black_2016 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_black_2016 = apply(pop, MARGIN = 2, mean)
    qmax_pop_black_2016[which(is.na(qmax_pop_black_2016))] = 0
    qmin_pop_black_2016[which(is.na(qmin_pop_black_2016))] = 0
    mean_pop_black_2016[which(is.na(mean_pop_black_2016))] = 0
    ymax = max(mean_pop_black_2016) #max(qmax_pop)
    xmax = length(qmax_pop_black_2016)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_black_2016, lwd = 3, col = colore)

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

#2017
anno_inizio = 2017
anno_fine = 2017
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Merli/Simulazioni/"
FileDynName = "dynamics_2spec_Merli_"
FolderPlotOut = "Output_WNV/Merli/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Merli_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Blackbirds"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#f46d43"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#f46d43"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#f46d43"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#f46d43"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#f46d43"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#f46d43"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#f46d43"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#f46d43"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#f46d43"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#FDAE61"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#FDAE61"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#FDAE61"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#FDAE61"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#FDAE61"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#FDAE61"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#FDAE61"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of blackbirds"
      colore = "#f46d43"
      main = paste("Blackbirds",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S blackbirds"
      colore = "#f46d43"
      main = paste("Susceptible blackbirds",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E blackbirds"
      colore = "#f46d43"
      main = paste("Exposed blackbirds",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I blackbirds"
      colore = "#f46d43"
      main = paste("Infected blackbirds",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R blackbirds"
      colore = "#f46d43"
      main = paste("Recovered blackbirds",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#f46d43"
      main = paste("Blackbirds prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#f46d43"
      main = paste("Blackbirds seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#f46d43"
      main = anno
    }
    
    
    qmax_pop_black_2017 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_black_2017 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_black_2017 = apply(pop, MARGIN = 2, mean)
    qmax_pop_black_2017[which(is.na(qmax_pop_black_2017))] = 0
    qmin_pop_black_2017[which(is.na(qmin_pop_black_2017))] = 0
    mean_pop_black_2017[which(is.na(mean_pop_black_2017))] = 0
    ymax = max(mean_pop_black_2017) #max(qmax_pop)
    xmax = length(qmax_pop_black_2017)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_black_2017, lwd = 3, col = colore)
 
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

#2018
anno_inizio = 2018
anno_fine = 2018
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Merli/Simulazioni/"
FileDynName = "dynamics_2spec_Merli_"
FolderPlotOut = "Output_WNV/Merli/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Merli_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Blackbirds"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#66c2a5"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#66c2a5"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#66c2a5"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#66c2a5"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#66c2a5"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#66c2a5"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#66c2a5"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#66c2a5"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#66c2a5"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#ABDDA4"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#ABDDA4"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#ABDDA4"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#ABDDA4"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#ABDDA4"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#ABDDA4"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#ABDDA4"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of blackbirds"
      colore = "#66c2a5"
      main = paste("Blackbirds",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S blackbirds"
      colore = "#66c2a5"
      main = paste("Susceptible blackbirds",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E blackbirds"
      colore = "#66c2a5"
      main = paste("Exposed blackbirds",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I blackbirds"
      colore = "#66c2a5"
      main = paste("Infected blackbirds",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R blackbirds"
      colore = "#66c2a5"
      main = paste("Recovered blackbirds",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Blackbirds prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Blackbirds seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#66c2a5"
      main = anno
    }
    
    
    qmax_pop_black_2018 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_black_2018 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_black_2018 = apply(pop, MARGIN = 2, mean)
    qmax_pop_black_2018[which(is.na(qmax_pop_black_2018))] = 0
    qmin_pop_black_2018[which(is.na(qmin_pop_black_2018))] = 0
    mean_pop_black_2018[which(is.na(mean_pop_black_2018))] = 0
    ymax = max(mean_pop_black_2018) #max(qmax_pop)
    xmax = length(qmax_pop_black_2018)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_black_2018, lwd = 3, col = colore)
 
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



####plot confronto anni#####
ymax = 100
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "% of Blackbirds")
lines(mean_pop_black_2016, lwd = 3, col = "#9E0142")
lines(mean_pop_black_2017, lwd = 3, col = "#F46D43")
lines(mean_pop_black_2018, lwd = 3, col = "#66C2A5")

axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("topright",
       legend = c("2016","2017","2018"),
       col = c("#9E0142","#F46D43","#66C2A5"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)



###confronto AC-SN----
#AC----
what_plot = "seroprevalence_avian_community"
#2016
anno_inizio = 2016
anno_fine = 2016
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Merli/Simulazioni/"
FileDynName = "dynamics_2spec_Merli_"
FolderPlotOut = "Output_WNV/Merli/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Merli_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Blackbirds"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#9e0142"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#9e0142"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#9e0142"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#9e0142"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#9e0142"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#9e0142"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#9e0142"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#9e0142"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#9e0142"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#D53E4F"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#D53E4F"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#D53E4F"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#D53E4F"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#D53E4F"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#D53E4F"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#D53E4F"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of blackbirds"
      colore = "#9e0142"
      main = paste("Blackbirds",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S blackbirds"
      colore = "#9e0142"
      main = paste("Susceptible blackbirds",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E blackbirds"
      colore = "#9e0142"
      main = paste("Exposed blackbirds",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I blackbirds"
      colore = "#9e0142"
      main = paste("Infected blackbirds",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R blackbirds"
      colore = "#9e0142"
      main = paste("Recovered blackbirds",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#9e0142"
      main = paste("Blackbirds prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#9e0142"
      main = paste("Blackbirds seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#9e0142"
      main = anno
    }
    
    
    qmax_pop_ac_black_2016 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_ac_black_2016 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_ac_black_2016 = apply(pop, MARGIN = 2, mean)
    qmax_pop_ac_black_2016[which(is.na(qmax_pop_ac_black_2016))] = 0
    qmin_pop_ac_black_2016[which(is.na(qmin_pop_ac_black_2016))] = 0
    mean_pop_ac_black_2016[which(is.na(mean_pop_ac_black_2016))] = 0
    ymax = max(mean_pop_ac_black_2016) #max(qmax_pop)
    xmax = length(qmax_pop_ac_black_2016)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_ac_black_2016, lwd = 3, col = colore)

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

#2017
anno_inizio = 2017
anno_fine = 2017
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Merli/Simulazioni/"
FileDynName = "dynamics_2spec_Merli_"
FolderPlotOut = "Output_WNV/Merli/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Merli_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Blackbirds"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#f46d43"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#f46d43"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#f46d43"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#f46d43"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#f46d43"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#f46d43"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#f46d43"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#f46d43"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#f46d43"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#FDAE61"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#FDAE61"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#FDAE61"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#FDAE61"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#FDAE61"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#FDAE61"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#FDAE61"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of blackbirds"
      colore = "#f46d43"
      main = paste("Blackbirds",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S blackbirds"
      colore = "#f46d43"
      main = paste("Susceptible blackbirds",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E blackbirds"
      colore = "#f46d43"
      main = paste("Exposed blackbirds",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I blackbirds"
      colore = "#f46d43"
      main = paste("Infected blackbirds",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R blackbirds"
      colore = "#f46d43"
      main = paste("Recovered blackbirds",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#f46d43"
      main = paste("Blackbirds prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#f46d43"
      main = paste("Blackbirds seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#f46d43"
      main = anno
    }
    
    
    qmax_pop_ac_black_2017 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_ac_black_2017 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_ac_black_2017 = apply(pop, MARGIN = 2, mean)
    qmax_pop_ac_black_2017[which(is.na(qmax_pop_ac_black_2017))] = 0
    qmin_pop_ac_black_2017[which(is.na(qmin_pop_ac_black_2017))] = 0
    mean_pop_ac_black_2017[which(is.na(mean_pop_ac_black_2017))] = 0
    ymax = max(mean_pop_ac_black_2017) #max(qmax_pop)
    xmax = length(qmax_pop_ac_black_2017)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_ac_black_2017, lwd = 3, col = colore)
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

#2018
anno_inizio = 2018
anno_fine = 2018
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Merli/Simulazioni/"
FileDynName = "dynamics_2spec_Merli_"
FolderPlotOut = "Output_WNV/Merli/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Merli_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Blackbirds"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#66c2a5"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#66c2a5"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#66c2a5"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#66c2a5"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#66c2a5"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#66c2a5"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#66c2a5"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#66c2a5"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#66c2a5"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#ABDDA4"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#ABDDA4"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#ABDDA4"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#ABDDA4"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#ABDDA4"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#ABDDA4"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#ABDDA4"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of blackbirds"
      colore = "#66c2a5"
      main = paste("Blackbirds",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S blackbirds"
      colore = "#66c2a5"
      main = paste("Susceptible blackbirds",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E blackbirds"
      colore = "#66c2a5"
      main = paste("Exposed blackbirds",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I blackbirds"
      colore = "#66c2a5"
      main = paste("Infected blackbirds",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R blackbirds"
      colore = "#66c2a5"
      main = paste("Recovered blackbirds",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Blackbirds prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Blackbirds seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#66c2a5"
      main = anno
    }
    
    
    qmax_pop_ac_black_2018 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_ac_black_2018 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_ac_black_2018 = apply(pop, MARGIN = 2, mean)
    qmax_pop_ac_black_2018[which(is.na(qmax_pop_ac_black_2018))] = 0
    qmin_pop_ac_black_2018[which(is.na(qmin_pop_ac_black_2018))] = 0
    mean_pop_ac_black_2018[which(is.na(mean_pop_ac_black_2018))] = 0
    ymax = max(mean_pop_ac_black_2018) #max(qmax_pop)
    xmax = length(qmax_pop_ac_black_2018)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_ac_black_2018, lwd = 3, col = colore)

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

#SN----
what_plot = "seroprev_known_bird_population"
#2016
anno_inizio = 2016
anno_fine = 2016
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Merli/Simulazioni/"
FileDynName = "dynamics_2spec_Merli_"
FolderPlotOut = "Output_WNV/Merli/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Merli_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Blackbirds"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#9e0142"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#9e0142"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#9e0142"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#9e0142"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#9e0142"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#9e0142"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#9e0142"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#9e0142"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#9e0142"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#D53E4F"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#D53E4F"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#D53E4F"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#D53E4F"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#D53E4F"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#D53E4F"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#D53E4F"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of blackbirds"
      colore = "#9e0142"
      main = paste("Blackbirds",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S blackbirds"
      colore = "#9e0142"
      main = paste("Susceptible blackbirds",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E blackbirds"
      colore = "#9e0142"
      main = paste("Exposed blackbirds",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I blackbirds"
      colore = "#9e0142"
      main = paste("Infected blackbirds",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R blackbirds"
      colore = "#9e0142"
      main = paste("Recovered blackbirds",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#9e0142"
      main = paste("Blackbirds prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#9e0142"
      main = paste("Blackbirds seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#9e0142"
      main = anno
    }
    
    
    qmax_pop_sn_black_2016 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_sn_black_2016 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_sn_black_2016 = apply(pop, MARGIN = 2, mean)
    qmax_pop_sn_black_2016[which(is.na(qmax_pop_sn_black_2016))] = 0
    qmin_pop_sn_black_2016[which(is.na(qmin_pop_sn_black_2016))] = 0
    mean_pop_sn_black_2016[which(is.na(mean_pop_sn_black_2016))] = 0
    ymax = max(mean_pop_sn_black_2016) #max(qmax_pop)
    xmax = length(qmax_pop_sn_black_2016)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_sn_black_2016, lwd = 3, col = colore)
 
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

#2017
anno_inizio = 2017
anno_fine = 2017
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Merli/Simulazioni/"
FileDynName = "dynamics_2spec_Merli_"
FolderPlotOut = "Output_WNV/Merli/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Merli_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Blackbirds"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#f46d43"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#f46d43"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#f46d43"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#f46d43"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#f46d43"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#f46d43"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#f46d43"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#f46d43"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#f46d43"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#FDAE61"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#FDAE61"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#FDAE61"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#FDAE61"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#FDAE61"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#FDAE61"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#FDAE61"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of blackbirds"
      colore = "#f46d43"
      main = paste("Blackbirds",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S blackbirds"
      colore = "#f46d43"
      main = paste("Susceptible blackbirds",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E blackbirds"
      colore = "#f46d43"
      main = paste("Exposed blackbirds",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I blackbirds"
      colore = "#f46d43"
      main = paste("Infected blackbirds",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R blackbirds"
      colore = "#f46d43"
      main = paste("Recovered blackbirds",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#f46d43"
      main = paste("Blackbirds prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#f46d43"
      main = paste("Blackbirds seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#f46d43"
      main = anno
    }
    
    
    qmax_pop_sn_black_2017 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_sn_black_2017 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_sn_black_2017 = apply(pop, MARGIN = 2, mean)
    qmax_pop_sn_black_2017[which(is.na(qmax_pop_sn_black_2017))] = 0
    qmin_pop_sn_black_2017[which(is.na(qmin_pop_sn_black_2017))] = 0
    mean_pop_sn_black_2017[which(is.na(mean_pop_sn_black_2017))] = 0
    ymax = max(mean_pop_sn_black_2017) #max(qmax_pop)
    xmax = length(qmax_pop_sn_black_2017)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_sn_black_2017, lwd = 3, col = colore)
   
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

#2018
anno_inizio = 2018
anno_fine = 2018
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Merli/Simulazioni/"
FileDynName = "dynamics_2spec_Merli_"
FolderPlotOut = "Output_WNV/Merli/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Merli_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Blackbirds"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#66c2a5"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#66c2a5"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#66c2a5"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#66c2a5"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#66c2a5"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#66c2a5"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#66c2a5"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#66c2a5"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#66c2a5"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#ABDDA4"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#ABDDA4"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#ABDDA4"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#ABDDA4"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#ABDDA4"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#ABDDA4"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#ABDDA4"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of blackbirds"
      colore = "#66c2a5"
      main = paste("Blackbirds",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S blackbirds"
      colore = "#66c2a5"
      main = paste("Susceptible blackbirds",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E blackbirds"
      colore = "#66c2a5"
      main = paste("Exposed blackbirds",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I blackbirds"
      colore = "#66c2a5"
      main = paste("Infected blackbirds",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R blackbirds"
      colore = "#66c2a5"
      main = paste("Recovered blackbirds",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Blackbirds prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Blackbirds seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#66c2a5"
      main = anno
    }
    
    
    qmax_pop_sn_black_2018 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_sn_black_2018 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_sn_black_2018 = apply(pop, MARGIN = 2, mean)
    qmax_pop_sn_black_2018[which(is.na(qmax_pop_sn_black_2018))] = 0
    qmin_pop_sn_black_2018[which(is.na(qmin_pop_sn_black_2018))] = 0
    mean_pop_sn_black_2018[which(is.na(mean_pop_sn_black_2018))] = 0
    ymax = max(mean_pop_sn_black_2018) #max(qmax_pop)
    xmax = length(qmax_pop_sn_black_2018)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_sn_black_2018, lwd = 3, col = colore)
  
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

###plot----
#2016
ymax = 9.8
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Bird seroprevalence")
lines(mean_pop_ac_black_2016, lwd = 3, col = "#D53E4F")
lines(mean_pop_sn_black_2016, lwd = 3, col = "#9E0142")

axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("topleft",
       legend = c("avian community 2016","blackbirds 2016"),
       col = c("#D53E4F","#9E0142"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)

#2017
ymax = 21.4
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Bird seroprevalence")
lines(mean_pop_ac_black_2017, lwd = 3, col = "#FDAE61")
lines(mean_pop_sn_black_2017, lwd = 3, col = "#F46D43")

axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("topleft",
       legend = c("avian community 2017","blackbirds 2017"),
       col = c("#FDAE61","#F46D43"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)

#2018
ymax = 15.6
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Bird seroprevalence")
lines(mean_pop_ac_black_2018, lwd = 3, col = "#ABDDA4")
lines(mean_pop_sn_black_2018, lwd = 3, col = "#66C2A5")

axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("topleft",
       legend = c("avian community 2018","blackbirds 2018"),
       col = c("#ABDDA4","#66C2A5"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)


#2016-2017-2018
ymax = 21.4
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Bird seroprevalence")

lines(mean_pop_ac_black_2016, lwd = 3, col = "#D53E4F")
lines(mean_pop_sn_black_2016, lwd = 3, col = "#9E0142")
lines(mean_pop_ac_black_2017, lwd = 3, col = "#FDAE61")
lines(mean_pop_sn_black_2017, lwd = 3, col = "#F46D43")
lines(mean_pop_ac_black_2018, lwd = 3, col = "#ABDDA4")
lines(mean_pop_sn_black_2018, lwd = 3, col = "#66C2A5")

axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("topleft",
       legend = c("avian community 2016","blackbirds 2016","avian community 2017","blackbirds 2017","avian community 2018","blackbirds 2018"),
       col = c("#D53E4F","#9E0142","#FDAE61","#F46D43","#ABDDA4","#66C2A5"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)

















###GAZZE----
what_plot = "known_bird_pop_rate"
#2016
anno_inizio = 2016
anno_fine = 2016
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Gazze/Simulazioni/"
FileDynName = "dynamics_2spec_Gazze_"
FolderPlotOut = "Output_WNV/Gazze/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Gazze_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Magpies"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#9e0142"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#9e0142"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#9e0142"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#9e0142"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#9e0142"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#9e0142"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#9e0142"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#9e0142"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#9e0142"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#D53E4F"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#D53E4F"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#D53E4F"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#D53E4F"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#D53E4F"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#D53E4F"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#D53E4F"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of magpies"
      colore = "#9e0142"
      main = paste("Magpies",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S magpies"
      colore = "#9e0142"
      main = paste("Susceptible magpies",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E magpies"
      colore = "#9e0142"
      main = paste("Exposed magpies",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I magpies"
      colore = "#9e0142"
      main = paste("Infected magpies",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R magpies"
      colore = "#9e0142"
      main = paste("Recovered magpies",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#9e0142"
      main = paste("Magpies prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#9e0142"
      main = paste("Magpies seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#9e0142"
      main = anno
    }
    
    
    qmax_pop_mag_2016 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_mag_2016 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_mag_2016 = apply(pop, MARGIN = 2, mean)
    qmax_pop_mag_2016[which(is.na(qmax_pop_mag_2016))] = 0
    qmin_pop_mag_2016[which(is.na(qmin_pop_mag_2016))] = 0
    mean_pop_mag_2016[which(is.na(mean_pop_mag_2016))] = 0
    ymax = max(mean_pop_mag_2016) #max(qmax_pop)
    xmax = length(qmax_pop_mag_2016)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_mag_2016, lwd = 3, col = colore)
  
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

#2017
anno_inizio = 2017
anno_fine = 2017
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Gazze/Simulazioni/"
FileDynName = "dynamics_2spec_Gazze_"
FolderPlotOut = "Output_WNV/Gazze/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Gazze_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Magpies"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#f46d43"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#f46d43"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#f46d43"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#f46d43"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#f46d43"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#f46d43"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#f46d43"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#f46d43"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#f46d43"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#FDAE61"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#FDAE61"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#FDAE61"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#FDAE61"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#FDAE61"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#FDAE61"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#FDAE61"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of magpies"
      colore = "#f46d43"
      main = paste("Magpies",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S magpies"
      colore = "#f46d43"
      main = paste("Susceptible magpies",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E magpies"
      colore = "#f46d43"
      main = paste("Exposed magpies",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I magpies"
      colore = "#f46d43"
      main = paste("Infected magpies",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R magpies"
      colore = "#f46d43"
      main = paste("Recovered magpies",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#f46d43"
      main = paste("Magpies prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#f46d43"
      main = paste("Magpies seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#f46d43"
      main = anno
    }
    
    
    qmax_pop_mag_2017 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_mag_2017 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_mag_2017 = apply(pop, MARGIN = 2, mean)
    qmax_pop_mag_2017[which(is.na(qmax_pop_mag_2017))] = 0
    qmin_pop_mag_2017[which(is.na(qmin_pop_mag_2017))] = 0
    mean_pop_mag_2017[which(is.na(mean_pop_mag_2017))] = 0
    ymax = max(mean_pop_mag_2017) #max(qmax_pop)
    xmax = length(qmax_pop_mag_2017)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_mag_2017, lwd = 3, col = colore)
  
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

#2018
anno_inizio = 2018
anno_fine = 2018
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Gazze/Simulazioni/"
FileDynName = "dynamics_2spec_Gazze_"
FolderPlotOut = "Output_WNV/Gazze/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Gazze_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Magpies"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#66c2a5"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#66c2a5"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#66c2a5"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#66c2a5"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#66c2a5"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#66c2a5"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#66c2a5"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#66c2a5"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#66c2a5"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#ABDDA4"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#ABDDA4"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#ABDDA4"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#ABDDA4"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#ABDDA4"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#ABDDA4"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#ABDDA4"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of magpies"
      colore = "#66c2a5"
      main = paste("Magpies",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S magpies"
      colore = "#66c2a5"
      main = paste("Susceptible magpies",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E magpies"
      colore = "#66c2a5"
      main = paste("Exposed magpies",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I magpies"
      colore = "#66c2a5"
      main = paste("Infected magpies",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R magpies"
      colore = "#66c2a5"
      main = paste("Recovered magpies",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Magpies prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Magpies seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#66c2a5"
      main = anno
    }
    
    
    qmax_pop_mag_2018 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_mag_2018 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_mag_2018 = apply(pop, MARGIN = 2, mean)
    qmax_pop_mag_2018[which(is.na(qmax_pop_mag_2018))] = 0
    qmin_pop_mag_2018[which(is.na(qmin_pop_mag_2018))] = 0
    mean_pop_mag_2018[which(is.na(mean_pop_mag_2018))] = 0
    ymax = max(mean_pop_mag_2018) #max(qmax_pop)
    xmax = length(qmax_pop_mag_2018)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_mag_2018, lwd = 3, col = colore)

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



####plot confronto anni#####
ymax = 100
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "% of magpies")
lines(mean_pop_mag_2016, lwd = 3, col = "#9E0142")
lines(mean_pop_mag_2017, lwd = 3, col = "#F46D43")
lines(mean_pop_mag_2018, lwd = 3, col = "#66C2A5")

axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("topright",
       legend = c("2016","2017","2018"),
       col = c("#9E0142","#F46D43","#66C2A5"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)



###confronto AC-SN----
#AC----
what_plot = "seroprevalence_avian_community"
#2016
anno_inizio = 2016
anno_fine = 2016
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Gazze/Simulazioni/"
FileDynName = "dynamics_2spec_Gazze_"
FolderPlotOut = "Output_WNV/Gazze/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Gazze_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Magpies"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#9e0142"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#9e0142"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#9e0142"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#9e0142"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#9e0142"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#9e0142"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#9e0142"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#9e0142"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#9e0142"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#D53E4F"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#D53E4F"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#D53E4F"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#D53E4F"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#D53E4F"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#D53E4F"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#D53E4F"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of magpies"
      colore = "#9e0142"
      main = paste("magpies",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S magpies"
      colore = "#9e0142"
      main = paste("Susceptible magpies",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E magpies"
      colore = "#9e0142"
      main = paste("Exposed magpies",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I magpies"
      colore = "#9e0142"
      main = paste("Infected magpies",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R magpies"
      colore = "#9e0142"
      main = paste("Recovered magpies",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#9e0142"
      main = paste("Magpies prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#9e0142"
      main = paste("Magpies seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#9e0142"
      main = anno
    }
    
    
    qmax_pop_ac_mag_2016 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_ac_mag_2016 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_ac_mag_2016 = apply(pop, MARGIN = 2, mean)
    qmax_pop_ac_mag_2016[which(is.na(qmax_pop_ac_mag_2016))] = 0
    qmin_pop_ac_mag_2016[which(is.na(qmin_pop_ac_mag_2016))] = 0
    mean_pop_ac_mag_2016[which(is.na(mean_pop_ac_mag_2016))] = 0
    ymax = max(mean_pop_ac_mag_2016) #max(qmax_pop)
    xmax = length(qmax_pop_ac_mag_2016)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_ac_mag_2016, lwd = 3, col = colore)

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

#2017
anno_inizio = 2017
anno_fine = 2017
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Gazze/Simulazioni/"
FileDynName = "dynamics_2spec_Gazze_"
FolderPlotOut = "Output_WNV/Gazze/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Gazze_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Magpies"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#f46d43"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#f46d43"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#f46d43"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#f46d43"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#f46d43"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#f46d43"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#f46d43"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#f46d43"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#f46d43"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#FDAE61"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#FDAE61"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#FDAE61"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#FDAE61"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#FDAE61"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#FDAE61"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#FDAE61"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of magpies"
      colore = "#f46d43"
      main = paste("magpies",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S magpies"
      colore = "#f46d43"
      main = paste("Susceptible magpies",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E magpies"
      colore = "#f46d43"
      main = paste("Exposed magpies",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I magpies"
      colore = "#f46d43"
      main = paste("Infected magpies",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R magpies"
      colore = "#f46d43"
      main = paste("Recovered magpies",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#f46d43"
      main = paste("Magpies prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#f46d43"
      main = paste("Magpies seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#f46d43"
      main = anno
    }
    
    
    qmax_pop_ac_mag_2017 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_ac_mag_2017 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_ac_mag_2017 = apply(pop, MARGIN = 2, mean)
    qmax_pop_ac_mag_2017[which(is.na(qmax_pop_ac_mag_2017))] = 0
    qmin_pop_ac_mag_2017[which(is.na(qmin_pop_ac_mag_2017))] = 0
    mean_pop_ac_mag_2017[which(is.na(mean_pop_ac_mag_2017))] = 0
    ymax = max(mean_pop_ac_mag_2017) #max(qmax_pop)
    xmax = length(qmax_pop_ac_mag_2017)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_ac_mag_2017, lwd = 3, col = colore)
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

#2018
anno_inizio = 2018
anno_fine = 2018
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Gazze/Simulazioni/"
FileDynName = "dynamics_2spec_Gazze_"
FolderPlotOut = "Output_WNV/Gazze/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Gazze_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Magpies"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#66c2a5"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#66c2a5"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#66c2a5"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#66c2a5"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#66c2a5"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#66c2a5"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#66c2a5"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#66c2a5"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#66c2a5"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#ABDDA4"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#ABDDA4"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#ABDDA4"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#ABDDA4"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#ABDDA4"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#ABDDA4"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#ABDDA4"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of magpies"
      colore = "#66c2a5"
      main = paste("Magpies",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S magpies"
      colore = "#66c2a5"
      main = paste("Susceptible magpies",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E magpies"
      colore = "#66c2a5"
      main = paste("Exposed magpies",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I magpies"
      colore = "#66c2a5"
      main = paste("Infected magpies",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R magpies"
      colore = "#66c2a5"
      main = paste("Recovered magpies",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Magpies prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Magpies seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#66c2a5"
      main = anno
    }
    
    
    qmax_pop_ac_mag_2018 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_ac_mag_2018 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_ac_mag_2018 = apply(pop, MARGIN = 2, mean)
    qmax_pop_ac_mag_2018[which(is.na(qmax_pop_ac_mag_2018))] = 0
    qmin_pop_ac_mag_2018[which(is.na(qmin_pop_ac_mag_2018))] = 0
    mean_pop_ac_mag_2018[which(is.na(mean_pop_ac_mag_2018))] = 0
    ymax = max(mean_pop_ac_mag_2018) #max(qmax_pop)
    xmax = length(qmax_pop_ac_mag_2018)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_ac_mag_2018, lwd = 3, col = colore)
   
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

#SN----
what_plot = "seroprev_known_bird_population"
#2016
anno_inizio = 2016
anno_fine = 2016
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Gazze/Simulazioni/"
FileDynName = "dynamics_2spec_Gazze_"
FolderPlotOut = "Output_WNV/Gazze/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Gazze_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Magpies"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#9e0142"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#9e0142"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#9e0142"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#9e0142"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#9e0142"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#9e0142"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#9e0142"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#9e0142"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#9e0142"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#D53E4F"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#D53E4F"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#D53E4F"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#D53E4F"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#D53E4F"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#D53E4F"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#D53E4F"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of magpies"
      colore = "#9e0142"
      main = paste("Magpies",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S magpies"
      colore = "#9e0142"
      main = paste("Susceptible magpies",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E magpies"
      colore = "#9e0142"
      main = paste("Exposed magpies",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I magpies"
      colore = "#9e0142"
      main = paste("Infected magpies",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R magpies"
      colore = "#9e0142"
      main = paste("Recovered magpies",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#9e0142"
      main = paste("Magpies prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#9e0142"
      main = paste("Magpies seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#9e0142"
      main = anno
    }
    
    
    qmax_pop_sn_mag_2016 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_sn_mag_2016 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_sn_mag_2016 = apply(pop, MARGIN = 2, mean)
    qmax_pop_sn_mag_2016[which(is.na(qmax_pop_sn_mag_2016))] = 0
    qmin_pop_sn_mag_2016[which(is.na(qmin_pop_sn_mag_2016))] = 0
    mean_pop_sn_mag_2016[which(is.na(mean_pop_sn_mag_2016))] = 0
    ymax = max(mean_pop_sn_mag_2016) #max(qmax_pop)
    xmax = length(qmax_pop_sn_mag_2016)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_sn_mag_2016, lwd = 3, col = colore)
  
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

#2017
anno_inizio = 2017
anno_fine = 2017
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Gazze/Simulazioni/"
FileDynName = "dynamics_2spec_Gazze_"
FolderPlotOut = "Output_WNV/Gazze/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Gazze_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Magpies"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#f46d43"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#f46d43"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#f46d43"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#f46d43"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#f46d43"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#f46d43"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#f46d43"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#f46d43"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#f46d43"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#FDAE61"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#FDAE61"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#FDAE61"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#FDAE61"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#FDAE61"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#FDAE61"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#FDAE61"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of magpies"
      colore = "#f46d43"
      main = paste("Magpies",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S magpies"
      colore = "#f46d43"
      main = paste("Susceptible magpies",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E magpies"
      colore = "#f46d43"
      main = paste("Exposed magpies",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I magpies"
      colore = "#f46d43"
      main = paste("Infected magpies",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R magpies"
      colore = "#f46d43"
      main = paste("Recovered magpies",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#f46d43"
      main = paste("Magpies prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#f46d43"
      main = paste("Magpies seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#f46d43"
      main = anno
    }
    
    
    qmax_pop_sn_mag_2017 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_sn_mag_2017 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_sn_mag_2017 = apply(pop, MARGIN = 2, mean)
    qmax_pop_sn_mag_2017[which(is.na(qmax_pop_sn_mag_2017))] = 0
    qmin_pop_sn_mag_2017[which(is.na(qmin_pop_sn_mag_2017))] = 0
    mean_pop_sn_mag_2017[which(is.na(mean_pop_sn_mag_2017))] = 0
    ymax = max(mean_pop_sn_mag_2017) #max(qmax_pop)
    xmax = length(qmax_pop_sn_mag_2017)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_sn_mag_2017, lwd = 3, col = colore)
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

#2018
anno_inizio = 2018
anno_fine = 2018
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Gazze/Simulazioni/"
FileDynName = "dynamics_2spec_Gazze_"
FolderPlotOut = "Output_WNV/Gazze/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Gazze_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Magpies"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#66c2a5"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#66c2a5"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#66c2a5"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#66c2a5"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#66c2a5"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#66c2a5"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#66c2a5"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#66c2a5"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#66c2a5"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#ABDDA4"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#ABDDA4"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#ABDDA4"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#ABDDA4"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#ABDDA4"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#ABDDA4"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#ABDDA4"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of magpies"
      colore = "#66c2a5"
      main = paste("Magpies",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S magpies"
      colore = "#66c2a5"
      main = paste("Susceptible magpies",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E magpies"
      colore = "#66c2a5"
      main = paste("Exposed magpies",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I magpies"
      colore = "#66c2a5"
      main = paste("Infected magpies",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R magpies"
      colore = "#66c2a5"
      main = paste("Recovered magpies",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Magpies prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Magpies seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#66c2a5"
      main = anno
    }
    
    
    qmax_pop_sn_mag_2018 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_sn_mag_2018 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_sn_mag_2018 = apply(pop, MARGIN = 2, mean)
    qmax_pop_sn_mag_2018[which(is.na(qmax_pop_sn_mag_2018))] = 0
    qmin_pop_sn_mag_2018[which(is.na(qmin_pop_sn_mag_2018))] = 0
    mean_pop_sn_mag_2018[which(is.na(mean_pop_sn_mag_2018))] = 0
    ymax = max(mean_pop_sn_mag_2018) #max(qmax_pop)
    xmax = length(qmax_pop_sn_mag_2018)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_sn_mag_2018, lwd = 3, col = colore)

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

###plot----
#2016
ymax = 26.2
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Bird seroprevalence")
lines(mean_pop_ac_mag_2016, lwd = 3, col = "#D53E4F")
lines(mean_pop_sn_mag_2016, lwd = 3, col = "#9E0142")

axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("topleft",
       legend = c("avian community 2016","magpies 2016"),
       col = c("#D53E4F","#9E0142"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)

#2017
ymax = 17.7
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Bird seroprevalence")
lines(mean_pop_ac_mag_2017, lwd = 3, col = "#FDAE61")
lines(mean_pop_sn_mag_2017, lwd = 3, col = "#F46D43")

axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("topright",
       legend = c("avian community 2017","magpies 2017"),
       col = c("#FDAE61","#F46D43"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)

#2018
ymax = 8.1
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Bird seroprevalence")
lines(mean_pop_ac_mag_2018, lwd = 3, col = "#ABDDA4")
lines(mean_pop_sn_mag_2018, lwd = 3, col = "#66C2A5")

axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("bottomright",
       legend = c("avian community 2018","magpies 2018"),
       col = c("#ABDDA4","#66C2A5"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)


#2016-2017-2018
ymax = 26.2
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Bird seroprevalence")

lines(mean_pop_ac_mag_2016, lwd = 3, col = "#D53E4F")
lines(mean_pop_sn_mag_2016, lwd = 3, col = "#9E0142")
lines(mean_pop_ac_mag_2017, lwd = 3, col = "#FDAE61")
lines(mean_pop_sn_mag_2017, lwd = 3, col = "#F46D43")
lines(mean_pop_ac_mag_2018, lwd = 3, col = "#ABDDA4")
lines(mean_pop_sn_mag_2018, lwd = 3, col = "#66C2A5")

axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend(1,27,
       legend = c("avian community 2016","magpies 2016","avian community 2017","magpies 2017","avian community 2018","magpies 2018"),
       col = c("#D53E4F","#9E0142","#FDAE61","#F46D43","#ABDDA4","#66C2A5"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)










###GERMANI----
what_plot = "known_bird_pop_rate"
#2016
anno_inizio = 2016
anno_fine = 2016
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Germani/Simulazioni/"
FileDynName = "dynamics_2spec_Germani_"
FolderPlotOut = "Output_WNV/Germani/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Germani_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Mallards"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#9e0142"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#9e0142"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#9e0142"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#9e0142"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#9e0142"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#9e0142"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#9e0142"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#9e0142"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#9e0142"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#D53E4F"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#D53E4F"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#D53E4F"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#D53E4F"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#D53E4F"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#D53E4F"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#D53E4F"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of mallards"
      colore = "#9e0142"
      main = paste("Mallards",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S mallards"
      colore = "#9e0142"
      main = paste("Susceptible mallards",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E mallards"
      colore = "#9e0142"
      main = paste("Exposed mallards",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I mallards"
      colore = "#9e0142"
      main = paste("Infected mallards",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R mallards"
      colore = "#9e0142"
      main = paste("Recovered mallards",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#9e0142"
      main = paste("Mallards prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#9e0142"
      main = paste("Mallards seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#9e0142"
      main = anno
    }
    
    
    qmax_pop_mall_2016 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_mall_2016 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_mall_2016 = apply(pop, MARGIN = 2, mean)
    qmax_pop_mall_2016[which(is.na(qmax_pop_mall_2016))] = 0
    qmin_pop_mall_2016[which(is.na(qmin_pop_mall_2016))] = 0
    mean_pop_mall_2016[which(is.na(mean_pop_mall_2016))] = 0
    ymax = max(mean_pop_mall_2016) #max(qmax_pop)
    xmax = length(qmax_pop_mall_2016)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_mall_2016, lwd = 3, col = colore)

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

#2017
anno_inizio = 2017
anno_fine = 2017
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Germani/Simulazioni/"
FileDynName = "dynamics_2spec_Germani_"
FolderPlotOut = "Output_WNV/Germani/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Germani_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Mallards"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#f46d43"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#f46d43"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#f46d43"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#f46d43"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#f46d43"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#f46d43"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#f46d43"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#f46d43"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#f46d43"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#FDAE61"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#FDAE61"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#FDAE61"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#FDAE61"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#FDAE61"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#FDAE61"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#FDAE61"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of mallards"
      colore = "#f46d43"
      main = paste("Mallards",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S mallards"
      colore = "#f46d43"
      main = paste("Susceptible mallards",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E mallards"
      colore = "#f46d43"
      main = paste("Exposed mallards",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I mallards"
      colore = "#f46d43"
      main = paste("Infected mallards",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R mallards"
      colore = "#f46d43"
      main = paste("Recovered mallards",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#f46d43"
      main = paste("Mallards prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#f46d43"
      main = paste("Mallards seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#f46d43"
      main = anno
    }
    
    
    qmax_pop_mall_2017 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_mall_2017 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_mall_2017 = apply(pop, MARGIN = 2, mean)
    qmax_pop_mall_2017[which(is.na(qmax_pop_mall_2017))] = 0
    qmin_pop_mall_2017[which(is.na(qmin_pop_mall_2017))] = 0
    mean_pop_mall_2017[which(is.na(mean_pop_mall_2017))] = 0
    ymax = max(mean_pop_mall_2017) #max(qmax_pop)
    xmax = length(qmax_pop_mall_2017)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_mall_2017, lwd = 3, col = colore)
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

#2018
anno_inizio = 2018
anno_fine = 2018
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Germani/Simulazioni/"
FileDynName = "dynamics_2spec_Germani_"
FolderPlotOut = "Output_WNV/Germani/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Germani_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Mallards"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#66c2a5"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#66c2a5"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#66c2a5"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#66c2a5"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#66c2a5"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#66c2a5"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#66c2a5"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#66c2a5"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#66c2a5"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#ABDDA4"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#ABDDA4"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#ABDDA4"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#ABDDA4"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#ABDDA4"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#ABDDA4"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#ABDDA4"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of mallards"
      colore = "#66c2a5"
      main = paste("Mallards",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S mallards"
      colore = "#66c2a5"
      main = paste("Susceptible mallards",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E mallards"
      colore = "#66c2a5"
      main = paste("Exposed mallards",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I mallards"
      colore = "#66c2a5"
      main = paste("Infected mallards",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R mallards"
      colore = "#66c2a5"
      main = paste("Recovered mallards",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Mallards prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Mallards seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#66c2a5"
      main = anno
    }
    
    
    qmax_pop_mall_2018 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_mall_2018 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_mall_2018 = apply(pop, MARGIN = 2, mean)
    qmax_pop_mall_2018[which(is.na(qmax_pop_mall_2018))] = 0
    qmin_pop_mall_2018[which(is.na(qmin_pop_mall_2018))] = 0
    mean_pop_mall_2018[which(is.na(mean_pop_mall_2018))] = 0
    ymax = max(mean_pop_mall_2018) #max(qmax_pop)
    xmax = length(qmax_pop_mall_2018)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_mall_2018, lwd = 3, col = colore)
  
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



####plot confronto anni#####
ymax = 100
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "% of mallards")
lines(mean_pop_mall_2016, lwd = 3, col = "#9E0142")
lines(mean_pop_mall_2017, lwd = 3, col = "#F46D43")
lines(mean_pop_mall_2018, lwd = 3, col = "#66C2A5")

axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("topright",
       legend = c("2016","2017","2018"),
       col = c("#9E0142","#F46D43","#66C2A5"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)



###confronto AC-SN----
#AC----
what_plot = "seroprevalence_avian_community"
#2016
anno_inizio = 2016
anno_fine = 2016
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Germani/Simulazioni/"
FileDynName = "dynamics_2spec_Germani_"
FolderPlotOut = "Output_WNV/Germani/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Germani_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Mallards"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#9e0142"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#9e0142"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#9e0142"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#9e0142"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#9e0142"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#9e0142"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#9e0142"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#9e0142"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#9e0142"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#D53E4F"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#D53E4F"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#D53E4F"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#D53E4F"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#D53E4F"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#D53E4F"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#D53E4F"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of mallards"
      colore = "#9e0142"
      main = paste("mallards",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S mallards"
      colore = "#9e0142"
      main = paste("Susceptible mallards",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E mallards"
      colore = "#9e0142"
      main = paste("Exposed mallards",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I mallards"
      colore = "#9e0142"
      main = paste("Infected mallards",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R mallards"
      colore = "#9e0142"
      main = paste("Recovered mallards",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#9e0142"
      main = paste("Mallards prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#9e0142"
      main = paste("Mallards seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#9e0142"
      main = anno
    }
    
    
    qmax_pop_ac_mall_2016 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_ac_mall_2016 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_ac_mall_2016 = apply(pop, MARGIN = 2, mean)
    qmax_pop_ac_mall_2016[which(is.na(qmax_pop_ac_mall_2016))] = 0
    qmin_pop_ac_mall_2016[which(is.na(qmin_pop_ac_mall_2016))] = 0
    mean_pop_ac_mall_2016[which(is.na(mean_pop_ac_mall_2016))] = 0
    ymax = max(mean_pop_ac_mall_2016) #max(qmax_pop)
    xmax = length(qmax_pop_ac_mall_2016)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_ac_mall_2016, lwd = 3, col = colore)
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

#2017
anno_inizio = 2017
anno_fine = 2017
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Germani/Simulazioni/"
FileDynName = "dynamics_2spec_Germani_"
FolderPlotOut = "Output_WNV/Germani/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Germani_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Mallards"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#f46d43"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#f46d43"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#f46d43"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#f46d43"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#f46d43"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#f46d43"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#f46d43"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#f46d43"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#f46d43"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#FDAE61"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#FDAE61"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#FDAE61"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#FDAE61"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#FDAE61"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#FDAE61"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#FDAE61"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of mallards"
      colore = "#f46d43"
      main = paste("mallards",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S mallards"
      colore = "#f46d43"
      main = paste("Susceptible mallards",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E mallards"
      colore = "#f46d43"
      main = paste("Exposed mallards",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I mallards"
      colore = "#f46d43"
      main = paste("Infected mallards",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R mallards"
      colore = "#f46d43"
      main = paste("Recovered mallards",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#f46d43"
      main = paste("Mallards prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#f46d43"
      main = paste("Mallards seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#f46d43"
      main = anno
    }
    
    
    qmax_pop_ac_mall_2017 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_ac_mall_2017 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_ac_mall_2017 = apply(pop, MARGIN = 2, mean)
    qmax_pop_ac_mall_2017[which(is.na(qmax_pop_ac_mall_2017))] = 0
    qmin_pop_ac_mall_2017[which(is.na(qmin_pop_ac_mall_2017))] = 0
    mean_pop_ac_mall_2017[which(is.na(mean_pop_ac_mall_2017))] = 0
    ymax = max(mean_pop_ac_mall_2017) #max(qmax_pop)
    xmax = length(qmax_pop_ac_mall_2017)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_ac_mall_2017, lwd = 3, col = colore)
  
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

#2018
anno_inizio = 2018
anno_fine = 2018
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Germani/Simulazioni/"
FileDynName = "dynamics_2spec_Germani_"
FolderPlotOut = "Output_WNV/Germani/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Germani_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Mallards"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#66c2a5"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#66c2a5"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#66c2a5"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#66c2a5"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#66c2a5"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#66c2a5"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#66c2a5"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#66c2a5"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#66c2a5"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#ABDDA4"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#ABDDA4"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#ABDDA4"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#ABDDA4"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#ABDDA4"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#ABDDA4"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#ABDDA4"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of mallards"
      colore = "#66c2a5"
      main = paste("Mallards",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S mallards"
      colore = "#66c2a5"
      main = paste("Susceptible mallards",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E mallards"
      colore = "#66c2a5"
      main = paste("Exposed mallards",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I mallards"
      colore = "#66c2a5"
      main = paste("Infected mallards",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R mallards"
      colore = "#66c2a5"
      main = paste("Recovered mallards",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Mallards prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Mallards seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#66c2a5"
      main = anno
    }
    
    
    qmax_pop_ac_mall_2018 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_ac_mall_2018 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_ac_mall_2018 = apply(pop, MARGIN = 2, mean)
    qmax_pop_ac_mall_2018[which(is.na(qmax_pop_ac_mall_2018))] = 0
    qmin_pop_ac_mall_2018[which(is.na(qmin_pop_ac_mall_2018))] = 0
    mean_pop_ac_mall_2018[which(is.na(mean_pop_ac_mall_2018))] = 0
    ymax = max(mean_pop_ac_mall_2018) #max(qmax_pop)
    xmax = length(qmax_pop_ac_mall_2018)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_ac_mall_2018, lwd = 3, col = colore)
  
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

#SN----
what_plot = "seroprev_known_bird_population"
#2016
anno_inizio = 2016
anno_fine = 2016
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Germani/Simulazioni/"
FileDynName = "dynamics_2spec_Germani_"
FolderPlotOut = "Output_WNV/Germani/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Germani_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Mallards"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#9e0142"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#9e0142"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#9e0142"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#9e0142"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#9e0142"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#9e0142"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#9e0142"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#9e0142"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#9e0142"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#9e0142"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#D53E4F"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#D53E4F"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#D53E4F"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#D53E4F"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#D53E4F"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#D53E4F"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#D53E4F"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of mallards"
      colore = "#9e0142"
      main = paste("Mallards",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S mallards"
      colore = "#9e0142"
      main = paste("Susceptible mallards",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E mallards"
      colore = "#9e0142"
      main = paste("Exposed mallards",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I mallards"
      colore = "#9e0142"
      main = paste("Infected mallards",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R mallards"
      colore = "#9e0142"
      main = paste("Recovered mallards",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#9e0142"
      main = paste("Mallards prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#9e0142"
      main = paste("Mallards seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#9e0142"
      main = anno
    }
    
    
    qmax_pop_sn_mall_2016 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_sn_mall_2016 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_sn_mall_2016 = apply(pop, MARGIN = 2, mean)
    qmax_pop_sn_mall_2016[which(is.na(qmax_pop_sn_mall_2016))] = 0
    qmin_pop_sn_mall_2016[which(is.na(qmin_pop_sn_mall_2016))] = 0
    mean_pop_sn_mall_2016[which(is.na(mean_pop_sn_mall_2016))] = 0
    ymax = max(mean_pop_sn_mall_2016) #max(qmax_pop)
    xmax = length(qmax_pop_sn_mall_2016)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_sn_mall_2016, lwd = 3, col = colore)

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

#2017
anno_inizio = 2017
anno_fine = 2017
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Germani/Simulazioni/"
FileDynName = "dynamics_2spec_Germani_"
FolderPlotOut = "Output_WNV/Germani/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Germani_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Mallards"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#f46d43"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#f46d43"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#f46d43"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#f46d43"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#f46d43"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#f46d43"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#f46d43"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#f46d43"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#f46d43"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#f46d43"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#FDAE61"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#FDAE61"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#FDAE61"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#FDAE61"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#FDAE61"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#FDAE61"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#FDAE61"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of mallards"
      colore = "#f46d43"
      main = paste("Mallards",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S mallards"
      colore = "#f46d43"
      main = paste("Susceptible mallards",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E mallards"
      colore = "#f46d43"
      main = paste("Exposed mallards",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I mallards"
      colore = "#f46d43"
      main = paste("Infected mallards",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R mallards"
      colore = "#f46d43"
      main = paste("Recovered mallards",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#f46d43"
      main = paste("Mallards prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#f46d43"
      main = paste("Mallards seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#f46d43"
      main = anno
    }
    
    
    qmax_pop_sn_mall_2017 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_sn_mall_2017 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_sn_mall_2017 = apply(pop, MARGIN = 2, mean)
    qmax_pop_sn_mall_2017[which(is.na(qmax_pop_sn_mall_2017))] = 0
    qmin_pop_sn_mall_2017[which(is.na(qmin_pop_sn_mall_2017))] = 0
    mean_pop_sn_mall_2017[which(is.na(mean_pop_sn_mall_2017))] = 0
    ymax = max(mean_pop_sn_mall_2017) #max(qmax_pop)
    xmax = length(qmax_pop_sn_mall_2017)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_sn_mall_2017, lwd = 3, col = colore)

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

#2018
anno_inizio = 2018
anno_fine = 2018
numero_cluster = 1
con_cosa_inizio = 1 

FolderSimu = "Output_WNV/Germani/Simulazioni/"
FileDynName = "dynamics_2spec_Germani_"
FolderPlotOut = "Output_WNV/Germani/Simulazioni/Plots/"
PlotName = "Dynamics_MosqPop_2spec_Germani_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

quale_specie = "Mallards"
quale_specie_2 = "Avian community"
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8

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
    all_bird_BS = BS+BS2
    all_bird_BE = BE+BE2
    all_bird_BI = BI+BI2
    all_bird_BR = BR+BR2
    known_bird_population = BS + BE + BI + BR
    prev_known_bird_population = (BI/known_bird_population) *100
    seroprev_known_bird_population = (BR/known_bird_population) *100
    avian_community = BS2 + BE2 + BI2 + BR2
    prevalence_avian_community = (BI2/avian_community) *100
    seroprevalence_avian_community = (BR2/avian_community) *100
    known_bird_pop_rate = (known_bird_population/all_bird_population)*100
    
    if (what_plot == "mosquito_population") {
      pop = mosquito_population
      ytext = "Number of mosquitoes"
      colore = "#66c2a5"
      main = paste("Mosquito population",anno)
    }
    
    if (what_plot == "MS") {
      pop = MS
      ytext = "Number of S mosquitoes"
      colore = "#66c2a5"
      main = paste("Susceptible mosquitoes",anno)
    }
    
    if (what_plot == "ME") {
      pop = ME
      ytext = "Number of E mosquitoes"
      colore = "#66c2a5"
      main = paste("Exposed mosquitoes",anno)
    }
    
    if (what_plot == "MI") {
      pop = MI
      ytext = "Number of I mosquitoes"
      colore = "#66c2a5"
      main = paste("Infected mosquitoes",anno)
    }
    
    if (what_plot == "mosquito_prevalence") {
      pop = mosquito_prevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Mosquito prevalence",anno)
    }
    
    if (what_plot == "all_bird_population") {
      pop = all_bird_population
      ytext = "number of birds"
      colore = "#66c2a5"
      main = paste("Birds",anno)
    }
    
    if (what_plot == "all_bird_BS") {
      pop = all_bird_BS
      ytext = "number of susceptible birds"
      colore = "#66c2a5"
      main = paste("Susceptible birds",anno)
    }
    
    if (what_plot == "all_bird_BE") {
      pop = all_bird_BE
      ytext = "number of exposed birds"
      colore = "#66c2a5"
      main = paste("Exposed birds",anno)
    }
    
    if (what_plot == "all_bird_BI") {
      pop = all_bird_BI
      ytext = "number of infected birds"
      colore = "#66c2a5"
      main = paste("Infected birds",anno)
    }
    
    if (what_plot == "all_bird_BR") {
      pop = all_bird_BR
      ytext = "number of recovered birds"
      colore = "#66c2a5"
      main = paste("Recovered birds",anno)
    }
    
    if (what_plot == "all_bird_prevalence") {
      pop = all_bird_prevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Bird prevalence",anno)
    }
    
    if (what_plot == "all_bird_seroprevalence") {
      pop = all_bird_seroprevalence
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Bird seroprevalence",anno)
    }
    
    if (what_plot == "avian_community") {
      pop = avian_community
      ytext = "Number of avian community birds"
      colore = "#ABDDA4"
      main = paste("Avian community",anno)
    }
    
    if (what_plot == "avian_community_BS") {
      pop = BS2
      ytext = "Number of S birds"
      colore = "#ABDDA4"
      main = paste("Susceptible avian community",anno)
    }
    
    if (what_plot == "avian_community_BE") {
      pop = BE2
      ytext = "Number of E birds"
      colore = "#ABDDA4"
      main = paste("Exposed avian community",anno)
    }
    
    if (what_plot == "avian_community_BI") {
      pop = BI2
      ytext = "Number of I birds"
      colore = "#ABDDA4"
      main = paste("Infected avian community",anno)
    }
    
    if (what_plot == "avian_community_BR") {
      pop = BR2
      ytext = "Number of R birds"
      colore = "#ABDDA4"
      main = paste("Recovered avian community",anno)
    }
    
    if (what_plot == "prevalence_avian_community") {
      pop = prevalence_avian_community
      ytext = "%"
      colore = "#ABDDA4"
      main = paste("Avian community prevalence",anno)
    }
    
    if (what_plot == "seroprevalence_avian_community") {
      pop = seroprevalence_avian_community
      ytext = "%"
      colore = "#ABDDA4"
      main = paste("Avian community seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_population") {
      pop = known_bird_population
      ytext = "Number of mallards"
      colore = "#66c2a5"
      main = paste("Mallards",anno)
    } 
    
    if (what_plot == "known_BS") {
      pop = BS
      ytext = "Number of S mallards"
      colore = "#66c2a5"
      main = paste("Susceptible mallards",anno)
    } 
    
    if (what_plot == "known_BE") {
      pop = BE
      ytext = "Number of E mallards"
      colore = "#66c2a5"
      main = paste("Exposed mallards",anno)
    } 
    
    if (what_plot == "known_BI") {
      pop = BI
      ytext = "Number of I mallards"
      colore = "#66c2a5"
      main = paste("Infected mallards",anno)
    } 
    
    if (what_plot == "known_BR") {
      pop = BR
      ytext = "Number of R mallards"
      colore = "#66c2a5"
      main = paste("Recovered mallards",anno)
    } 
    
    if (what_plot == "prev_known_bird_population") {
      pop = prev_known_bird_population
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Mallards prevalence",anno)
    } 
    
    if (what_plot == "seroprev_known_bird_population") {
      pop = seroprev_known_bird_population
      ytext = "%"
      colore = "#66c2a5"
      main = paste("Mallards seroprevalence",anno)
    } 
    
    if (what_plot == "known_bird_pop_rate") {
      pop = known_bird_pop_rate
      ytext = paste("% of", quale_specie)
      colore = "#66c2a5"
      main = anno
    }
    
    
    qmax_pop_sn_mall_2018 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.975, na.rm = T)
    })
    qmin_pop_sn_mall_2018 = apply(pop, MARGIN = 2, function(p) {
      quantile(p, probs = 0.025, na.rm = T)
    })
    mean_pop_sn_mall_2018 = apply(pop, MARGIN = 2, mean)
    qmax_pop_sn_mall_2018[which(is.na(qmax_pop_sn_mall_2018))] = 0
    qmin_pop_sn_mall_2018[which(is.na(qmin_pop_sn_mall_2018))] = 0
    mean_pop_sn_mall_2018[which(is.na(mean_pop_sn_mall_2018))] = 0
    ymax = max(mean_pop_sn_mall_2018) #max(qmax_pop)
    xmax = length(qmax_pop_sn_mall_2018)
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = main)
    lines(mean_pop_sn_mall_2018, lwd = 3, col = colore)
  
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

###plot----
#2016
ymax = 20.7
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Bird seroprevalence")
lines(mean_pop_ac_mall_2016, lwd = 3, col = "#D53E4F")
lines(mean_pop_sn_mall_2016, lwd = 3, col = "#9E0142")

axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("topleft",
       legend = c("avian community 2016","mallards 2016"),
       col = c("#D53E4F","#9E0142"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)

#2017
ymax = 10.7
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Bird seroprevalence")
lines(mean_pop_ac_mall_2017, lwd = 3, col = "#FDAE61")
lines(mean_pop_sn_mall_2017, lwd = 3, col = "#F46D43")

axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("topleft",
       legend = c("avian community 2017","mallards 2017"),
       col = c("#FDAE61","#F46D43"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)

#2018
ymax = 8
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Bird seroprevalence")
lines(mean_pop_ac_mall_2018, lwd = 3, col = "#ABDDA4")
lines(mean_pop_sn_mall_2018, lwd = 3, col = "#66C2A5")

axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend(0.5,8.5,
       legend = c("avian community 2018","mallards 2018"),
       col = c("#ABDDA4","#66C2A5"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)


#2016-2017-2018
ymax = 20.7
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Bird seroprevalence")

lines(mean_pop_ac_mall_2016, lwd = 3, col = "#D53E4F")
lines(mean_pop_sn_mall_2016, lwd = 3, col = "#9E0142")
lines(mean_pop_ac_mall_2017, lwd = 3, col = "#FDAE61")
lines(mean_pop_sn_mall_2017, lwd = 3, col = "#F46D43")
lines(mean_pop_ac_mall_2018, lwd = 3, col = "#ABDDA4")
lines(mean_pop_sn_mall_2018, lwd = 3, col = "#66C2A5")

axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("topleft",
       legend = c("avian community 2016","mallards 2016","avian community 2017","mallards 2017","avian community 2018","mallards 2018"),
       col = c("#D53E4F","#9E0142","#FDAE61","#F46D43","#ABDDA4","#66C2A5"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)


















#####plot confronto 3specie####
#2016
ymax = 26.2
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Avian community seroprevalence 2016")
lines(mean_pop_ac_black_2016, lwd = 3, col = "#D53E4F")
lines(mean_pop_ac_mag_2016, lwd = 3, col = "#FDAE61")
lines(mean_pop_ac_mall_2016, lwd = 3, col = "#ABDDA4")


axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("topleft",
       legend = c("avian community blackbirds","avian community magpies","avian community mallards"),
       col = c("#D53E4F","#FDAE61","#ABDDA4"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)

ymax = 100
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "% of birds 2018")
lines(mean_pop_black_2018, lwd = 3, col = "#9E0142")
lines(mean_pop_mag_2018, lwd = 3, col = "#F46D43")
lines(mean_pop_mall_2018, lwd = 3, col = "#66C2A5")


axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("topright",
       legend = c("blackbirds","magpies","mallards"),
       col = c("#9E0142","#F46D43","#66C2A5"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)


ymax = 26.2
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Bird seroprevalence 2016")
lines(mean_pop_sn_black_2016, lwd = 3, col = "#9E0142")
lines(mean_pop_ac_black_2016, lwd = 3, col = "#D53E4F")
lines(mean_pop_sn_mag_2016, lwd = 3, col = "#F46D43")
lines(mean_pop_ac_mag_2016, lwd = 3, col = "#FDAE61")
lines(mean_pop_sn_mall_2016, lwd = 3, col = "#66C2A5")
lines(mean_pop_ac_mall_2016, lwd = 3, col = "#ABDDA4")


axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("topleft",
       legend = c("blackbirds","ac blackbirds","magpies","ac magpies","mallards","ac mallards"),
       col = c("#9E0142","#D53E4F","#F46D43","#FDAE61","#66C2A5","#ABDDA4"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)


#2017
ymax = 21.4
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Avian community seroprevalence 2017")
lines(mean_pop_ac_black_2017, lwd = 3, col = "#D53E4F")
lines(mean_pop_ac_mag_2017, lwd = 3, col = "#FDAE61")
lines(mean_pop_ac_mall_2017, lwd = 3, col = "#ABDDA4")


axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("topleft",
       legend = c("avian community blackbirds","avian community magpies","avian community mallards"),
       col = c("#D53E4F","#FDAE61","#ABDDA4"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)

ymax = 17.7
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Known bird seroprevalence 2017")
lines(mean_pop_sn_black_2017, lwd = 3, col = "#9E0142")
lines(mean_pop_sn_mag_2017, lwd = 3, col = "#F46D43")
lines(mean_pop_sn_mall_2017, lwd = 3, col = "#66C2A5")


axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("top",
       legend = c("blackbirds","magpies","mallards"),
       col = c("#9E0142","#F46D43","#66C2A5"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)


ymax = 21.4
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Bird seroprevalence 2017")
lines(mean_pop_sn_black_2017, lwd = 3, col = "#9E0142")
lines(mean_pop_ac_black_2017, lwd = 3, col = "#D53E4F")
lines(mean_pop_sn_mag_2017, lwd = 3, col = "#F46D43")
lines(mean_pop_ac_mag_2017, lwd = 3, col = "#FDAE61")
lines(mean_pop_sn_mall_2017, lwd = 3, col = "#66C2A5")
lines(mean_pop_ac_mall_2017, lwd = 3, col = "#ABDDA4")


axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("top",
       legend = c("blackbirds","ac blackbirds","magpies","ac magpies","mallards","ac mallards"),
       col = c("#9E0142","#D53E4F","#F46D43","#FDAE61","#66C2A5","#ABDDA4"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)

#2018
ymax = 8.1
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Avian community seroprevalence 2018")
lines(mean_pop_ac_black_2018, lwd = 3, col = "#D53E4F")
lines(mean_pop_ac_mag_2018, lwd = 3, col = "#FDAE61")
lines(mean_pop_ac_mall_2018, lwd = 3, col = "#ABDDA4")


axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("topleft",
       legend = c("avian community blackbirds","avian community magpies","avian community mallards"),
       col = c("#D53E4F","#FDAE61","#ABDDA4"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)

ymax = 15.6
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Known bird seroprevalence 2018")
lines(mean_pop_sn_black_2018, lwd = 3, col = "#9E0142")
lines(mean_pop_sn_mag_2018, lwd = 3, col = "#F46D43")
lines(mean_pop_sn_mall_2018, lwd = 3, col = "#66C2A5")


axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("topleft",
       legend = c("blackbirds","magpies","mallards"),
       col = c("#9E0142","#F46D43","#66C2A5"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)


ymax = 15.6
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Bird seroprevalence 2018")
lines(mean_pop_sn_black_2018, lwd = 3, col = "#9E0142")
lines(mean_pop_ac_black_2018, lwd = 3, col = "#D53E4F")
lines(mean_pop_sn_mag_2018, lwd = 3, col = "#F46D43")
lines(mean_pop_ac_mag_2018, lwd = 3, col = "#FDAE61")
lines(mean_pop_sn_mall_2018, lwd = 3, col = "#66C2A5")
lines(mean_pop_ac_mall_2018, lwd = 3, col = "#ABDDA4")


axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend(5,16,
       legend = c("blackbirds","ac blackbirds","magpies","ac magpies","mallards","ac mallards"),
       col = c("#9E0142","#D53E4F","#F46D43","#FDAE61","#66C2A5","#ABDDA4"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)

#2016-2017-2018
ymax = 20.7
plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                   ymax), xlab = "", ylab = "", axes = F, main = "Bird seroprevalence")

lines(mean_pop_ac_2016, lwd = 3, col = "#D53E4F")
lines(mean_pop_sn_2016, lwd = 3, col = "#9E0142")
lines(mean_pop_ac_2017, lwd = 3, col = "#FDAE61")
lines(mean_pop_sn_2017, lwd = 3, col = "#F46D43")
lines(mean_pop_ac_2018, lwd = 3, col = "#ABDDA4")
lines(mean_pop_sn_2018, lwd = 3, col = "#66C2A5")

axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)
scientific_num = ifelse((ymax < 0.01 | ymax > 1e+05), 
                        T, F)
axis(2, at = seq(0, ymax, length.out = 4), las = 2, 
     labels = format(seq(0, ymax, length.out = 4), 
                     digits = 2, scientific = scientific_num), cex.axis = cex_axes)
mtext("%", side = 2, line = 5, cex = cex_axes)

legend("topleft",
       legend = c("avian community 2016","mallards 2016","avian community 2017","mallards 2017","avian community 2018","mallards 2018"),
       col = c("#D53E4F","#9E0142","#FDAE61","#F46D43","#ABDDA4","#66C2A5"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)


######################################CONTRIBUTION##################################

####MERLI----
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8


par(mfrow = c(1, 1), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
#### contributo 2016 ####
FolderSimu = "Output_WNV/Merli/Simulazioni/"
FileDynName = "dynamics_2spec_Merli_"
anno = 2016  #cambia l'anno di volta in volta
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

contr_known_black_2016 = (num/den)*100
contr_ac_black_2016 = (num2/den)*100

####contribution 2017####
FolderSimu = "Output_WNV/Merli/Simulazioni/"
FileDynName = "dynamics_2spec_Merli_"
anno = 2017  #cambia l'anno di volta in volta
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

contr_known_black_2017 = (num/den)*100
contr_ac_black_2017 = (num2/den)*100

####contribution 2018####
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

contr_known_black_2018 = (num/den)*100
contr_ac_black_2018 = (num2/den)*100

####plot####
#SN
plot(0, col = "white", xlim = c(0, 180), ylim = c(0, 100),
     xlab = "", ylab = "", axes = F, main = "Blackbirds contribution 2018", type = 'l')

lines(contr_known_black_2018, lwd = 3, col = "#66C2A5")


axis(1, at = c(1, 1+30, 1+30 + 31, 1+30 + 31 + 30, 1+30 + 31 + 30 + 
                 31, 1+30 + 31 + 30 + 31 + 31, 1+30 + 31 + 30 + 31 + 
                 31 + 30),
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept","Oct"), 
     cex.axis = cex_axes)
axis(2, at = NULL, las =2) 
mtext("Contribution %", side = 2, line = 5, adj = 0.5, cex = cex_axes)


##confronto anni
plot(0, col = "white", xlim = c(0, 180), ylim = c(0, 100),
     xlab = "", ylab = "", axes = F, main = "Blackbirds contribution", type = 'l')

lines(contr_known_black_2016, lwd = 3, col = "#9E0142")
lines(contr_known_black_2017, lwd = 3, col = "#F46D43")
lines(contr_known_black_2018, lwd = 3, col = "#66C2A5")


axis(1, at = c(1, 1+30, 1+30 + 31, 1+30 + 31 + 30, 1+30 + 31 + 30 + 
                 31, 1+30 + 31 + 30 + 31 + 31, 1+30 + 31 + 30 + 31 + 
                 31 + 30),
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept","Oct"), 
     cex.axis = cex_axes)
axis(2, at = NULL, las =2) 
mtext("Contribution %", side = 2, line = 5, adj = 0.5, cex = cex_axes)

legend("topleft",
       legend = c("2016", "2017","2018"),
       col = c("#9E0142","#F46D43","#66C2A5"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)




####GAZZE----
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8


par(mfrow = c(1, 1), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
#### contributo 2016 ####
FolderSimu = "Output_WNV/Gazze/Simulazioni/"
FileDynName = "dynamics_2spec_Gazze_"
anno = 2016  #cambia l'anno di volta in volta
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

contr_known_mag_2016 = (num/den)*100
contr_ac_mag_2016 = (num2/den)*100

####contribution 2017####
FolderSimu = "Output_WNV/Gazze/Simulazioni/"
FileDynName = "dynamics_2spec_Gazze_"
anno = 2017  #cambia l'anno di volta in volta
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

contr_known_mag_2017 = (num/den)*100
contr_ac_mag_2017 = (num2/den)*100

####contribution 2018####
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

contr_known_mag_2018 = (num/den)*100
contr_ac_mag_2018 = (num2/den)*100

####plot####
#SN
plot(0, col = "white", xlim = c(0, 180), ylim = c(0, 35),
     xlab = "", ylab = "", axes = F, main = "Magpies contribution 2017", type = 'l')

lines(contr_known_mag_2016, lwd = 3, col = "#9E0142")
#oppure
lines(contr_known_mag_2017, lwd = 3, col = "#F46D43")
#oppure
lines(contr_known_mag_2018, lwd = 3, col = "#66C2A5")


axis(1, at = c(1, 1+30, 1+30 + 31, 1+30 + 31 + 30, 1+30 + 31 + 30 + 
                 31, 1+30 + 31 + 30 + 31 + 31, 1+30 + 31 + 30 + 31 + 
                 31 + 30),
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept","Oct"), 
     cex.axis = cex_axes)
axis(2, at = NULL, las =2) 
mtext("Contribution %", side = 2, line = 5, adj = 0.5, cex = cex_axes)


##confronto anni
plot(0, col = "white", xlim = c(0, 180), ylim = c(0, 35),
     xlab = "", ylab = "", axes = F, main = "Magpies contribution", type = 'l')

lines(contr_known_mag_2016, lwd = 3, col = "#9E0142")
lines(contr_known_mag_2017, lwd = 3, col = "#F46D43")
lines(contr_known_mag_2018, lwd = 3, col = "#66C2A5")


axis(1, at = c(1, 1+30, 1+30 + 31, 1+30 + 31 + 30, 1+30 + 31 + 30 + 
                 31, 1+30 + 31 + 30 + 31 + 31, 1+30 + 31 + 30 + 31 + 
                 31 + 30),
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept","Oct"), 
     cex.axis = cex_axes)
axis(2, at = NULL, las =2) 
mtext("Contribution %", side = 2, line = 5, adj = 0.5, cex = cex_axes)

legend("topleft",
       legend = c("2016", "2017","2018"),
       col = c("#9E0142","#F46D43","#66C2A5"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)






####GERMANI----
cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8


par(mfrow = c(1, 1), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
#### contributo 2016 ####
FolderSimu = "Output_WNV/Germani/Simulazioni/"
FileDynName = "dynamics_2spec_Germani_"
anno = 2016  #cambia l'anno di volta in volta
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

b11 = 0.1 #usa biting rate specie nota
b1 = mean(output_mcmc[, 'b1_ca']) #usa bititng rate calcolato da MCMC (per avian community)

pop_tot = apply((BS + BE + BI + BR), MARGIN = 2, mean)
pop_tot2 = apply((BS2 + BE2 + BI2 + BR2), MARGIN = 2, mean)

num = b11*(apply(BI, MARGIN = 2, mean)/pop_tot)
num2 = b1*(apply(BI2, MARGIN = 2, mean)/pop_tot2)
den = b11*(apply(BI, MARGIN = 2, mean)/pop_tot) + b1*(apply(BI2, MARGIN = 2, mean)/pop_tot2)

contr_known_mall_2016 = (num/den)*100
contr_ac_mall_2016 = (num2/den)*100

####contribution 2017####
FolderSimu = "Output_WNV/Germani/Simulazioni/"
FileDynName = "dynamics_2spec_Germani_"
anno = 2017  #cambia l'anno di volta in volta
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

b11 = 0.1 #usa biting rate specie nota
b1 = mean(output_mcmc[, 'b1_ca']) #usa bititng rate calcolato da MCMC (per avian community)

pop_tot = apply((BS + BE + BI + BR), MARGIN = 2, mean)
pop_tot2 = apply((BS2 + BE2 + BI2 + BR2), MARGIN = 2, mean)

num = b11*(apply(BI, MARGIN = 2, mean)/pop_tot)
num2 = b1*(apply(BI2, MARGIN = 2, mean)/pop_tot2)
den = b11*(apply(BI, MARGIN = 2, mean)/pop_tot) + b1*(apply(BI2, MARGIN = 2, mean)/pop_tot2)

contr_known_mall_2017 = (num/den)*100
contr_ac_mall_2017 = (num2/den)*100

####contribution 2018####
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

b11 = 0.1 #usa biting rate specie nota
b1 = mean(output_mcmc[, 'b1_ca']) #usa bititng rate calcolato da MCMC (per avian community)

pop_tot = apply((BS + BE + BI + BR), MARGIN = 2, mean)
pop_tot2 = apply((BS2 + BE2 + BI2 + BR2), MARGIN = 2, mean)

num = b11*(apply(BI, MARGIN = 2, mean)/pop_tot)
num2 = b1*(apply(BI2, MARGIN = 2, mean)/pop_tot2)
den = b11*(apply(BI, MARGIN = 2, mean)/pop_tot) + b1*(apply(BI2, MARGIN = 2, mean)/pop_tot2)

contr_known_mall_2018 = (num/den)*100
contr_ac_mall_2018 = (num2/den)*100

####plot####
#SN
plot(0, col = "white", xlim = c(0, 180), ylim = c(0, 5),
     xlab = "", ylab = "", axes = F, main = "Mallards contribution 2016", type = 'l')

lines(contr_known_mall_2016, lwd = 3, col = "#9E0142")
#oppure
lines(contr_known_mall_2017, lwd = 3, col = "#F46D43")
#oppure
lines(contr_known_mall_2018, lwd = 3, col = "#66C2A5")


axis(1, at = c(1, 1+30, 1+30 + 31, 1+30 + 31 + 30, 1+30 + 31 + 30 + 
                 31, 1+30 + 31 + 30 + 31 + 31, 1+30 + 31 + 30 + 31 + 
                 31 + 30),
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept","Oct"), 
     cex.axis = cex_axes)
axis(2, at = NULL, las =2) 
mtext("Contribution %", side = 2, line = 5, adj = 0.5, cex = cex_axes)


##confronto anni
plot(0, col = "white", xlim = c(0, 180), ylim = c(0, 5),
     xlab = "", ylab = "", axes = F, main = "Mallards contribution", type = 'l')

lines(contr_known_mall_2016, lwd = 3, col = "#9E0142")
lines(contr_known_mall_2017, lwd = 3, col = "#F46D43")
lines(contr_known_mall_2018, lwd = 3, col = "#66C2A5")


axis(1, at = c(1, 1+30, 1+30 + 31, 1+30 + 31 + 30, 1+30 + 31 + 30 + 
                 31, 1+30 + 31 + 30 + 31 + 31, 1+30 + 31 + 30 + 31 + 
                 31 + 30),
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept","Oct"), 
     cex.axis = cex_axes)
axis(2, at = NULL, las =2) 
mtext("Contribution %", side = 2, line = 5, adj = 0.5, cex = cex_axes)

legend("topleft",
       legend = c("2016", "2017","2018"),
       col = c("#9E0142","#F46D43","#66C2A5"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)

#####plot confronto 3 specie
plot(0, col = "white", xlim = c(0, 180), ylim = c(0, 100),
     xlab = "", ylab = "", axes = F, main = "Contribution 2018", type = 'l')

lines(contr_known_black_2018, lwd = 3, col = "#9E0142")
lines(contr_known_mag_2018, lwd = 3, col = "#F46D43")
lines(contr_known_mall_2018, lwd = 3, col = "#66C2A5")


axis(1, at = c(1, 1+30, 1+30 + 31, 1+30 + 31 + 30, 1+30 + 31 + 30 + 
                 31, 1+30 + 31 + 30 + 31 + 31, 1+30 + 31 + 30 + 31 + 
                 31 + 30),
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept","Oct"), 
     cex.axis = cex_axes)
axis(2, at = NULL, las =2) 
mtext("Contribution %", side = 2, line = 5, adj = 0.5, cex = cex_axes)

legend("topleft",
       legend = c("blackbirds", "magpies","mallards"),
       col = c("#9E0142","#F46D43","#66C2A5"), lwd = 2, lty=1,
       bty = "n",
       cex = 1)






