##################################### 02_Epidemiological_1sp ########################################
FolderSimu = "Output_WNV/Simulazioni/"
FileDynName = "dynamics_"
FolderPlotOut = "Output_WNV/Simulazioni/Plots/"
PlotName = "ModelFit_"
file_numero_pool_name = "per_mcmc/totale_pool_cluster_"
file_numero_pool_positivi_name = "per_mcmc/totale_pool_WNVpos_cluster_"
file_pool_size_name = "per_mcmc/mean_pool_size_cluster_"
vettore_date_catture = "per_mcmc/giorni_cattura.txt"

anno_inizio = 2016
anno_fine = 2018
numero_cluster = 1
con_cosa_inizio = 1
max_iter_MCMC = 10000
tmax = 180

cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8
ymax_plot = 17


#### model fit plot ####
settimane = scan(vettore_date_catture)
colori = brewer.pal(11,"Spectral")
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
                                                        wd = 0.2, col = "#5E4FA2")#colori[quale_cluster])
    points(1:xmax, numero_pool_positivi[quale_cluster, 
                                        quando_ho_pool][1:xmax], pch = 19, col = "#FDAE61", 
           cex = 2)
    date = format(as.Date(as.numeric(settimane[quando_ho_pool[1:xmax]]), 
                          origin = paste("01-01-", anno, sep = ""), format = "%d-%m-%Y"), 
                  "%d-%m")
    axis(1, at = c(1:xmax), labels = date, las = 2)
    axis(2, las = 2, at = seq(0, ymax, 5))
    mtext("WNV positive pools", side = 2, line = 3, adj = 0.5, cex = 1.5)
    legend( x = "topright",
            legend = c("Observations","Predictions"),
            col = c("#FDAE61","#5E4FA2"), lwd = 2, lty = c(0,0),
            pch = c(19,15),
            bty = "n",
            cex = 1.2,
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


#### boxplot plot #### 
OutLoc = "Output_WNV/MCMC"
FileAllParmsName = "parametri_"
FileParmsPerSimuName = "per_simulazione_"

nomi_parametri = c("p", "B0", "pR", "b1", "muB", "s", "phi", 
                   "niB", "recB")
nomi_parametri = c("recB") #da cambiare a seconda del box che si vuole plottare

#2016----
anno_inizio = 2016
anno_fine = 2016
numero_cluster = 1
con_cosa_inizio = 1

for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
  for (quale_cluster in 1:numero_cluster) {
    if (con_cosa_inizio == 0) 
      nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                   "B_", anno, "_", quale_cluster, ".txt")
    if (con_cosa_inizio == 1) 
      nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                   "M_", anno, "_", quale_cluster, ".txt")
    output_mcmc_2016 = read.table(nome_file_parametri)
    output_mcmc_2016 = output_mcmc_2016[,c(9,10)] ##CAMBIA A SECONDA DELLA COLONNA CORRISPONDENTE AL PARAMETRO DA PLOTTARE
    burnin = nrow(output_mcmc_2016) * 0.1
    if (ncol(output_mcmc_2016) <= 1) 
      output_mcmc_2016 = matrix(0, ncol = length(nomi_parametri) + 
                             1, nrow = max_iter_MCMC)
    for (j in 1:(ncol(output_mcmc_2016) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_2016[-c(1:burnin), j])
  }
  if (con_cosa_inizio == 0) 
    nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                       "B_", anno, "_", quale_cluster, ".pdf")
  if (con_cosa_inizio == 1) 
    nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                       "M_", anno, "_", quale_cluster, ".pdf")
  

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
  }
    
#2017----
anno_inizio = 2017
anno_fine = 2017
numero_cluster = 1
con_cosa_inizio = 1


for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
  for (quale_cluster in 1:numero_cluster) {
    if (con_cosa_inizio == 0) 
      nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                   "B_", anno, "_", quale_cluster, ".txt")
    if (con_cosa_inizio == 1) 
      nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                   "M_", anno, "_", quale_cluster, ".txt")
    output_mcmc_2017 = read.table(nome_file_parametri)
    output_mcmc_2017 = output_mcmc_2017[,c(9,10)]##CAMBIA A SECONDA DELLA COLONNA CORRISPONDENTE AL PARAMETRO DA PLOTTARE
    burnin = nrow(output_mcmc_2017) * 0.1
    if (ncol(output_mcmc_2017) <= 1) 
      output_mcmc_2017 = matrix(0, ncol = length(nomi_parametri) + 
                                  1, nrow = max_iter_MCMC)
    for (j in 1:(ncol(output_mcmc_2017) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_2017[-c(1:burnin), j])
  }
  if (con_cosa_inizio == 0) 
    nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                       "B_", anno, "_", quale_cluster, ".pdf")
  if (con_cosa_inizio == 1) 
    nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                       "M_", anno, "_", quale_cluster, ".pdf")
  
  
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
}
#2018----
anno_inizio = 2018
anno_fine = 2018
numero_cluster = 1
con_cosa_inizio = 1


for (anno in anno_inizio:anno_fine) {
  parametri_stimati = vector("list", length(nomi_parametri))
  for (quale_cluster in 1:numero_cluster) {
    if (con_cosa_inizio == 0) 
      nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                   "B_", anno, "_", quale_cluster, ".txt")
    if (con_cosa_inizio == 1) 
      nome_file_parametri = paste0(OutLoc, "/", FileAllParmsName, 
                                   "M_", anno, "_", quale_cluster, ".txt")
    output_mcmc_2018 = read.table(nome_file_parametri)
    output_mcmc_2018 = output_mcmc_2018[,c(9,10)]##CAMBIA A SECONDA DELLA COLONNA CORRISPONDENTE AL PARAMETRO DA PLOTTARE
    burnin = nrow(output_mcmc_2018) * 0.1
    if (ncol(output_mcmc_2018) <= 1) 
      output_mcmc_2018 = matrix(0, ncol = length(nomi_parametri) + 
                                  1, nrow = max_iter_MCMC)
    for (j in 1:(ncol(output_mcmc_2018) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                             output_mcmc_2018[-c(1:burnin), j])
  }
  if (con_cosa_inizio == 0) 
    nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                       "B_", anno, "_", quale_cluster, ".pdf")
  if (con_cosa_inizio == 1) 
    nome_file = paste0(OutLoc, "/Plots/", FileAllParmsName, 
                       "M_", anno, "_", quale_cluster, ".pdf")
  
  
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
}

#plol----
recB <- as.data.frame(cbind(tab_2016, tab_2017, tab_2018))
colnames(recB) <- c(2016,2017,2018)

par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l", las = 1)
boxplot(recB, ylim= c(0.1,0.21), bty = "l", ylab = "",
        xlab = "years", main = "Recovery rate", col =c("#9E0142","#F46D43","#66C2A5"))


col = c("#9E0142","#F46D43","#66C2A5")
col = "grey"



#### dinamiche ####
anno_inizio = 2018
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
what_plot = "vector_host"
colore = "#66C2A5" ##da cambiare a seconda dell'anno che plotto (vedi sotto)
#2016 = "#9E0142"
#2017 = "#F46D43"
#2018 = "#66C2A5"



settimane = scan(vettore_date_catture)

for (anno in anno_inizio:anno_fine) {
  print(anno)
  nome_file_plot = paste0(FolderPlotOut, PlotName, anno, 
                          ".pdf")
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
    sel_MS_2018 = seq(1, nrow(output_dynamics), numero_classi)
    sel_ME_2018 = seq(2, nrow(output_dynamics), numero_classi)
    sel_MI_2018 = seq(3, nrow(output_dynamics), numero_classi)
    sel_BS_2018 = seq(4, nrow(output_dynamics), numero_classi)
    sel_BE_2018 = seq(5, nrow(output_dynamics), numero_classi)
    sel_BI_2018 = seq(6, nrow(output_dynamics), numero_classi)
    sel_BR_2018 = seq(7, nrow(output_dynamics), numero_classi)
    MS_2018 = output_dynamics[sel_MS_2018, ]
    ME_2018 = output_dynamics[sel_ME_2018, ]
    MI_2018 = output_dynamics[sel_MI_2018, ]
    BS_2018 = output_dynamics[sel_BS_2018, ]
    BE_2018 = output_dynamics[sel_BE_2018, ]
    BI_2018 = output_dynamics[sel_BI_2018, ]
    BR_2018 = output_dynamics[sel_BR_2018, ]
    mosquito_population_2018 = MS_2018 + ME_2018 + MI_2018
    mosquito_prevalence_2018 = MI_2018/mosquito_population_2018
    bird_population_2018 = BS_2018 + BE_2018 + BI_2018 + BR_2018
    bird_prevalence_2018 = BI_2018/bird_population_2018
    vector_host_2018 = mosquito_population_2018/bird_population_2018
    if (what_plot == "mosquito_prevalence") 
      pop_2018 = mosquito_prevalence_2018
    if (what_plot == "MS") 
      pop_2018 = MS_2018
    if (what_plot == "ME") 
      pop_2018 = ME_2018
    if (what_plot == "MI") 
      pop_2018 = MI_2018
    if (what_plot == "mosquito_population") 
      pop_2018 = mosquito_population_2018
    if (what_plot == "bird_prevalence") 
      pop_2018 = bird_prevalence_2018
    if (what_plot == "bird_population") 
      pop_2018 = bird_population_2018
    if (what_plot == "BS") 
      pop_2018 = BS_2018
    if (what_plot == "BE") 
      pop_2018 = BE_2018
    if (what_plot == "BI") 
      pop_2018 = BI_2018
    if (what_plot == "BR") 
      pop = BR_2018
    if (what_plot == "vector_host") 
      pop_2018 = vector_host_2018
   
    qmax_pop_2018 = c()
    qmin_pop_2018 = c()
    mean_pop_2018 = c()
    for (j in 1:(ncol(pop_2018))) {
      qmax_pop_2018 = c(qmax_pop_2018, quantile(pop_2018[, j], probs = 0.975, 
                                      na.rm = T))
      qmin_pop_2018 = c(qmin_pop_2018, quantile(pop_2018[, j], probs = 0.025, 
                                      na.rm = T))
      mean_pop_2018 = c(mean_pop_2018, mean(pop_2018[, j], na.rm = T))
    }
    qmax_pop_2018[which(is.na(qmax_pop_2018))] = 0
    qmin_pop_2018[which(is.na(qmin_pop_2018))] = 0
    mean_pop_2018[which(is.na(mean_pop_2018))] = 0
    ymax = max(mean_pop_2018)
    xmax = length(qmax_pop_2018)
    
    cex_points = 1.2
    cex_axes = 1.6
    cex_lab_axes = 1.6
    margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
    cex_main = 1.8
    
    
    par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
        cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")
    plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, 
                                                       ymax), xlab = "", ylab = "", axes = F, main = anno)
    lines(mean_pop_2018, lwd = 3, col = colore)

    axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 
                     30, 1 + 30 + 31 + 30 + 31, 1 + 30 + 31 + 30 + 
                     31 + 31, 1 + 30 + 31 + 30 + 
                     31 + 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                               "Aug", "Sept", "Oct"), cex.axis = 1.3, las = 2)

    axis(2, at = seq(0, ymax, length.out = 5), las = 2, 
         labels = format(seq(0, ymax, length.out = 5), 
                         digits = 2, scientific = F), cex.axis = 1.3)
    if (what_plot == "mosquito_prevalence") 
      text = "Prevalence in mosquitoes"
    if (what_plot == "mosquito_population") 
      text = "Mosquito population"
    if (what_plot == "bird_prevalence") 
      text = "Prevalence in birds"
    if (what_plot == "bird_population") 
      text = "Bird population"
    if (what_plot == "BS") 
      text = "number of susceptible birds"
    if (what_plot == "BE") 
      text = "number of exposed birds"
    if (what_plot == "BI") 
      text = "number of infected birds"
    if (what_plot == "BR") 
      text = "number of recovered birds"
    if (what_plot == "vector_host") 
      text = "vector host ratio"
    if (what_plot == "MS") 
      text = "number of susceptible mosquitoes"
    if (what_plot == "ME") 
      text = "number of exposed mosquitoes"
    if (what_plot == "MI") 
      text = "number of infected mosquitoes"
    mtext(text, side = 2, line = 6, adj = 0.5, cex = 1.5)
  }
  #dev.off()
}

#plot----
par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6)
ymax = 570 #max(qmax, na.rm = T)
ymin = 0
xmin = 0
xmax = length(qmax_pop_2018)
plot(0, axes = F, col = "white", ylim = c(ymin, ymax), 
     xlim = c(xmin, xmax), ylab = "", xlab = "", main = "Vector/Host ratio", cex.main = 1.5)

lines(mean_pop_2016, lwd = 2.5, col ="#9E0142")
lines(mean_pop_2017, lwd = 2.5, col ="#F46D43")
lines(mean_pop_2018, lwd = 2.5, col ="#66C2A5")

axis(1, at = seq(xmin, xmax, 30), labels = month.abb[seq(4,10,1)])
axis(2, at = seq(ymin, ymax, length.out = 5), 
     las = 2, labels = format(seq(ymin, ymax, length.out = 5), scientific = F, digits = 1))
mtext(side = 2, text = "vector/host", line = 5, 
      cex = 1.5)
legend("topright",legend=c("2016","2017","2018"), lty = 1, lwd = 2.5,
       col = c("#9E0142","#F46D43","#66C2A5"), box.lwd = F)






#### birth pulse plot ####
library(WNModFEMRL)
setwd("/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/WNModFEMRL")
parms = "Output_WNV/MCMC/parametri_M_2018_1.txt"
main = 2018
tmin = 91
tmax = 274
quante_specie = 1
numero_cluster = 1

cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8


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
                                                    0.1), xlab = "", ylab = "", axes = F, main = main, bty="l")
  
  #lines(mean_pop, lwd = 3, col = colore)
  abline(v = c(tmin, tmax), lwd = 2, col = "#FDAE61")
  lines(mean, lwd = 3, col = "black")
  #poligono = cbind(x = c(1:length(mean), length(mean):1), 
  #                 y = c(qmin, rev(qmax)))
  #polygon(poligono, col = adjustcolor("#66C2A5", alpha = 0.2), 
  #        border = NA)
  axis(1, at = c(1, 1 + 31,
                 1 + 31 + 28,
                 1 + 31 + 28 + 31,
                 1 + 31 + 28 + 31 + 30,
                 1 + 31 + 28 + 31 + 30 + 31,
                 1 + 31 + 28 + 31 + 30 + 31 + 30,
                 1 + 31 + 28 + 31 + 30 + 31 + 30 + 31,
                 1 + 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31, 
                 1 + 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30,
                 1 + 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31,
                 1 + 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 , 
                 1 + 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + 31), 
       labels = c("Gen", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "Gen"),
       cex.axis = 1.3, las = 2)
  
  axis(2, at = seq(0, 0.1, 0.05), las = 2, 
       labels = format(seq(0, 0.1, 0.05), 
                       digits = 2, scientific = F), cex.axis = 1.3)
  mtext("Birth pulse", side = 2, line = 5, adj = 0.5, cex = 1.5)  
}
#dev.off()

##confronto 3 anni####
#2016----
setwd("/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/WNModFEMRL")
parms_2016 = "Output_WNV/MCMC/parametri_M_2016_1.txt"
main = "Birth pulse"
tmin = 91
tmax = 274
quante_specie = 1
numero_cluster = 1

cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8


par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")



if (is.character(parms_2016)) {
  if (quante_specie == 1) 
    nomi_parametri = c("p", "B0", "pR", "b1", "muB", 
                       "s", "phi", "niB", "recB")

  parametri_stimati = vector("list", length(nomi_parametri))
  output_mcmc_2016 = read.table(parms_2016)
  colnames(output_mcmc_2016) = c(nomi_parametri, "lik")
  burnin = nrow(output_mcmc_2016) * 0.1
  output_mcmc_2016 = output_mcmc_2016[-c(1:burnin), ]
  if (quante_specie == 1) {
    birth_pulse_2016 = apply(output_mcmc_2016, MARGIN = 1, function(a) {
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
  mean_2016 = apply(birth_pulse_2016, MARGIN = 1, function(a) {
    mean(a, na.rm = T)
  })
  qmin_2016 = apply(birth_pulse_2016, MARGIN = 1, function(a) {
    quantile(a, probs = 0.025, na.rm = T)
  })
  qmax_2016 = apply(birth_pulse_2016, MARGIN = 1, function(a) {
    quantile(a, probs = 0.975, na.rm = T)
  })

  ymax = max(qmax_2016, qmin_2016, mean_2016)
}

#2017----
setwd("/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/WNModFEMRL")
parms_2017 = "Output_WNV/MCMC/parametri_M_2017_1.txt"
main = "Birth pulse"
tmin = 91
tmax = 274
quante_specie = 1
numero_cluster = 1

cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8


par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")



if (is.character(parms_2017)) {
  if (quante_specie == 1) 
    nomi_parametri = c("p", "B0", "pR", "b1", "muB", 
                       "s", "phi", "niB", "recB")
  
  parametri_stimati = vector("list", length(nomi_parametri))
  output_mcmc_2017 = read.table(parms_2017)
  colnames(output_mcmc_2017) = c(nomi_parametri, "lik")
  burnin = nrow(output_mcmc_2017) * 0.1
  output_mcmc_2017 = output_mcmc_2017[-c(1:burnin), ]
  if (quante_specie == 1) {
    birth_pulse_2017 = apply(output_mcmc_2017, MARGIN = 1, function(a) {
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
  mean_2017 = apply(birth_pulse_2017, MARGIN = 1, function(a) {
    mean(a, na.rm = T)
  })
  qmin_2017 = apply(birth_pulse_2017, MARGIN = 1, function(a) {
    quantile(a, probs = 0.025, na.rm = T)
  })
  qmax_2017 = apply(birth_pulse_2017, MARGIN = 1, function(a) {
    quantile(a, probs = 0.975, na.rm = T)
  })
  
  ymax = max(qmax_2017, qmin_2017, mean_2017)
}

#2018----
setwd("/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/WNModFEMRL")
parms_2018 = "Output_WNV/MCMC/parametri_M_2018_1.txt"
main = "Birth pulse"
tmin = 91
tmax = 274
quante_specie = 1
numero_cluster = 1

cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8


par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")



if (is.character(parms_2018)) {
  if (quante_specie == 1) 
    nomi_parametri = c("p", "B0", "pR", "b1", "muB", 
                       "s", "phi", "niB", "recB")
  
  parametri_stimati = vector("list", length(nomi_parametri))
  output_mcmc_2018 = read.table(parms_2018)
  colnames(output_mcmc_2018) = c(nomi_parametri, "lik")
  burnin = nrow(output_mcmc_2018) * 0.1
  output_mcmc_2018 = output_mcmc_2018[-c(1:burnin), ]
  if (quante_specie == 1) {
    birth_pulse_2018 = apply(output_mcmc_2018, MARGIN = 1, function(a) {
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
  mean_2018 = apply(birth_pulse_2018, MARGIN = 1, function(a) {
    mean(a, na.rm = T)
  })
  qmin_2018 = apply(birth_pulse_2018, MARGIN = 1, function(a) {
    quantile(a, probs = 0.025, na.rm = T)
  })
  qmax_2018 = apply(birth_pulse_2018, MARGIN = 1, function(a) {
    quantile(a, probs = 0.975, na.rm = T)
  })
  
  ymax = max(qmax_2018, qmin_2018, mean_2018)
}

#plot ----
  plot(0, col = "white", xlim = c(0, 365), ylim = c(0, 
                                                    0.15), xlab = "", ylab = "", axes = F, main = main, bty="l")

  abline(v = c(tmin, tmax), lwd = 2, col = "black")
  lines(mean_2016, lwd = 3, col = "#9E0142")
  poligono = cbind(x = c(1:length(mean_2016), length(mean_2016):1), 
                   y = c(qmin_2016, rev(qmax_2016)))
  polygon(poligono, col = adjustcolor("#9E0142", alpha = 0.2), 
          border = NA)
  lines(mean_2017, lwd = 3, col = "#F46D43")
  poligono = cbind(x = c(1:length(mean_2017), length(mean_2017):1), 
                   y = c(qmin_2017, rev(qmax_2017)))
  polygon(poligono, col = adjustcolor("#F46D43", alpha = 0.2), 
          border = NA)
  lines(mean_2018, lwd = 3, col = "#66C2A5")
  poligono = cbind(x = c(1:length(mean_2018), length(mean_2018):1), 
                   y = c(qmin_2018, rev(qmax_2018)))
  polygon(poligono, col = adjustcolor("#66C2A5", alpha = 0.2), 
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

  legend("topright",legend=c("2016","2017","2028"), lty = 1, lwd = 2.5,
         col = c("#9E0142","#F46D43","#66C2A5"), box.lwd = F)
#dev.off()

