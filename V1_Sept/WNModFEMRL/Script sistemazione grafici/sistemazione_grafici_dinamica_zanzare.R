library(dplyr)
library(tidyr)
library(ggplot2)
library(WNModFEMRL)
library(RColorBrewer)
display.brewer.pal(11,"Spectral") # Spectral sono 11 colori

####### plot dinamica popolazione zanzare ----
setwd("/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/WNModFEMRL/")
anno_inizio = 2016
anno_fine = 2018
numero_cluster = 1

OutLoc = "Output/Simulazioni"
OutZanzMedieLoc = "per_mcmc/zanzare"

FileDynName = "dynamics_"
FileZanzMedieName = "adulti_medi_"
PlotDynName = "dinamica_adulti_"

cex_points = 1.2
cex_axes = 1.5
cex_lab_axes = 1.5
margins_plot = c(5, 5, 2, 2)

message("adults dynamics")
numero_classi_popolazione = 4
classe_adulti = 4
nome_classi = c("E", "L", "P", "A")

for (anno in anno_inizio:anno_fine) {
  print(paste("Anno", anno))
  nome_file = paste0(OutLoc, "/Plots/", PlotDynName, anno, ".jpg")
  #jpeg(nome_file, width = numero_cluster * 1200, height = 1000, res = 200)
  par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
      cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6)
  for (quale_cluster in 1:numero_cluster) {
    nome_file_output_dynamics = paste0(OutLoc, "/", FileDynName, anno, "_", quale_cluster, ".txt")
    dinamica_simulata = read.table(nome_file_output_dynamics)
    classe = classe_adulti = 4
    simu_sel = dinamica_simulata[seq(classe, nrow(dinamica_simulata), numero_classi_popolazione), 1:180]
    qmin = rep(NA, ncol(simu_sel))
    qmax = rep(NA, ncol(simu_sel))
    media = rep(NA, ncol(simu_sel))
    for (j in 1:ncol(simu_sel)) {
      qmin[j] = quantile(simu_sel[, j], probs = 0.025)
      qmax[j] = quantile(simu_sel[, j], probs = 0.975)
      media[j] = mean(simu_sel[, j])
    }
    ymax = 26100 #max(qmax, na.rm = T)
    ymin = 0
    xmin = 0
    xmax = length(media)
    plot(0, axes = F, col = "white", ylim = c(ymin, ymax), 
         xlim = c(xmin, xmax), ylab = "", xlab = "", main = anno, cex.main = 1.8)
    poligono = cbind(x = c(1:length(qmin), length(qmin):1), 
                     y = c(qmin, rev(qmax)))
    #polygon(poligono, col = adjustcolor("gray", alpha = 0.5),border = NA)
    lines(media, lwd = 2.5)
    axis(1, at = seq(xmin, xmax, 30), labels = month.abb[seq(4,10,1)])
    axis(2, at = round(seq(ymin, ymax, length.out = 5)), 
         las = 2, labels = format(round(seq(ymin, ymax, length.out = 5)), scientific = F, digits = 1))
    mtext(side = 2, text = "Number of Cx. pipiens females", line = 6, 
          cex = 1.5)
  }
 # dev.off()
}


##### plot confronto 3 anni ----
####2016####
anno_inizio = 2016
anno_fine = 2016
numero_cluster = 1

OutLoc = "Output/Simulazioni"
OutZanzMedieLoc = "per_mcmc/zanzare"

FileDynName = "dynamics_"
FileZanzMedieName = "adulti_medi_"
PlotDynName = "dinamica_adulti_"

cex_points = 1.2
cex_axes = 1.5
cex_lab_axes = 1.5
margins_plot = c(5, 5, 2, 2)

message("adults dynamics")
numero_classi_popolazione = 4
classe_adulti = 4
nome_classi = c("E", "L", "P", "A")

for (anno in anno_inizio:anno_fine) {
  print(paste("Anno", anno))
  nome_file = paste0(OutLoc, "/Plots/", PlotDynName, anno, ".jpg")
  #jpeg(nome_file, width = numero_cluster * 1200, height = 1000, res = 200)
  par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
      cex.lab = 1.5, cex.main = 2, cex.axis = 1.5)
  for (quale_cluster in 1:numero_cluster) {
    nome_file_output_dynamics = paste0(OutLoc, "/", FileDynName, anno, "_", quale_cluster, ".txt")
    dinamica_simulata = read.table(nome_file_output_dynamics)
    classe = classe_adulti = 4
    simu_sel = dinamica_simulata[seq(classe, nrow(dinamica_simulata), numero_classi_popolazione), 1:180]
    qmin = rep(NA, ncol(simu_sel))
    qmax = rep(NA, ncol(simu_sel))
    media_2016 = rep(NA, ncol(simu_sel))
    for (j in 1:ncol(simu_sel)) {
      qmin[j] = quantile(simu_sel[, j], probs = 0.025)
      qmax[j] = quantile(simu_sel[, j], probs = 0.975)
      media_2016[j] = mean(simu_sel[, j])
    }
  }
  ymax = 26100 #max(qmax, na.rm = T)
  ymin = 0
  xmin = 0
  xmax = length(media)
  plot(0, axes = F, col = "white", ylim = c(ymin, ymax), 
       xlim = c(xmin, xmax), ylab = "", xlab = "", main = anno, cex.main = 1.8)
  poligono = cbind(x = c(1:length(qmin), length(qmin):1), 
                   y = c(qmin, rev(qmax)))
  #polygon(poligono, col = adjustcolor("gray", alpha = 0.5),border = NA)
  lines(media_2016, lwd = 2.5, col = "#9E0142")
  axis(1, at = seq(xmin, xmax, 30), labels = month.abb[seq(4,10,1)])
  axis(2, at = round(seq(ymin, ymax, length.out = 5)), 
       las = 2, labels = format(round(seq(ymin, ymax, length.out = 5)), scientific = F, digits = 1))
  mtext(side = 2, text = "Number of Cx. pipiens females", line = 6, 
        cex = 1.5)
}
####2017####
anno_inizio = 2017
anno_fine = 2017
numero_cluster = 1

OutLoc = "Output/Simulazioni"
OutZanzMedieLoc = "per_mcmc/zanzare"

FileDynName = "dynamics_"
FileZanzMedieName = "adulti_medi_"
PlotDynName = "dinamica_adulti_"

cex_points = 1.2
cex_axes = 1.5
cex_lab_axes = 1.5
margins_plot = c(5, 5, 2, 2)

message("adults dynamics")
numero_classi_popolazione = 4
classe_adulti = 4
nome_classi = c("E", "L", "P", "A")

for (anno in anno_inizio:anno_fine) {
  print(paste("Anno", anno))
  nome_file = paste0(OutLoc, "/Plots/", PlotDynName, anno, ".jpg")
  #jpeg(nome_file, width = numero_cluster * 1200, height = 1000, res = 200)
  par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
      cex.lab = 1.5, cex.main = 2, cex.axis = 1.5)
  for (quale_cluster in 1:numero_cluster) {
    nome_file_output_dynamics = paste0(OutLoc, "/", FileDynName, anno, "_", quale_cluster, ".txt")
    dinamica_simulata = read.table(nome_file_output_dynamics)
    classe = classe_adulti = 4
    simu_sel = dinamica_simulata[seq(classe, nrow(dinamica_simulata), numero_classi_popolazione), 1:180]
    qmin = rep(NA, ncol(simu_sel))
    qmax = rep(NA, ncol(simu_sel))
    media_2017 = rep(NA, ncol(simu_sel))
    for (j in 1:ncol(simu_sel)) {
      qmin[j] = quantile(simu_sel[, j], probs = 0.025)
      qmax[j] = quantile(simu_sel[, j], probs = 0.975)
      media_2017[j] = mean(simu_sel[, j])
    }
  }
  ymax = 26100 #max(qmax, na.rm = T)
  ymin = 0
  xmin = 0
  xmax = length(media)
  plot(0, axes = F, col = "white", ylim = c(ymin, ymax), 
       xlim = c(xmin, xmax), ylab = "", xlab = "", main = anno, cex.main = 1.8)
  poligono = cbind(x = c(1:length(qmin), length(qmin):1), 
                   y = c(qmin, rev(qmax)))
  #polygon(poligono, col = adjustcolor("gray", alpha = 0.5),border = NA)
  lines(media_2017, lwd = 2.5, col = "#F46D43")
  axis(1, at = seq(xmin, xmax, 30), labels = month.abb[seq(4,10,1)])
  axis(2, at = round(seq(ymin, ymax, length.out = 5)), 
       las = 2, labels = format(round(seq(ymin, ymax, length.out = 5)), scientific = F, digits = 1))
  mtext(side = 2, text = "Number of Cx. pipiens females", line = 6, 
        cex = 1.5)
}

####2018####
anno_inizio = 2018
anno_fine = 2018
numero_cluster = 1

OutLoc = "Output/Simulazioni"
OutZanzMedieLoc = "per_mcmc/zanzare"

FileDynName = "dynamics_"
FileZanzMedieName = "adulti_medi_"
PlotDynName = "dinamica_adulti_"

cex_points = 1.2
cex_axes = 1.5
cex_lab_axes = 1.5
margins_plot = c(5, 5, 2, 2)

message("adults dynamics")
numero_classi_popolazione = 4
classe_adulti = 4
nome_classi = c("E", "L", "P", "A")

for (anno in anno_inizio:anno_fine) {
  print(paste("Anno", anno))
  nome_file = paste0(OutLoc, "/Plots/", PlotDynName, anno, ".jpg")
  #jpeg(nome_file, width = numero_cluster * 1200, height = 1000, res = 200)
  par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
      cex.lab = 1.5, cex.main = 2, cex.axis = 1.5)
  for (quale_cluster in 1:numero_cluster) {
    nome_file_output_dynamics = paste0(OutLoc, "/", FileDynName, anno, "_", quale_cluster, ".txt")
    dinamica_simulata = read.table(nome_file_output_dynamics)
    classe = classe_adulti = 4
    simu_sel = dinamica_simulata[seq(classe, nrow(dinamica_simulata), numero_classi_popolazione), 1:180]
    qmin = rep(NA, ncol(simu_sel))
    qmax = rep(NA, ncol(simu_sel))
    media_2018 = rep(NA, ncol(simu_sel))
    for (j in 1:ncol(simu_sel)) {
      qmin[j] = quantile(simu_sel[, j], probs = 0.025)
      qmax[j] = quantile(simu_sel[, j], probs = 0.975)
      media_2018[j] = mean(simu_sel[, j])
    }
  }
  ymax = 26100 #max(qmax, na.rm = T)
  ymin = 0
  xmin = 0
  xmax = length(media)
  plot(0, axes = F, col = "white", ylim = c(ymin, ymax), 
       xlim = c(xmin, xmax), ylab = "", xlab = "", main = anno, cex.main = 1.8)
  poligono = cbind(x = c(1:length(qmin), length(qmin):1), 
                   y = c(qmin, rev(qmax)))
  #polygon(poligono, col = adjustcolor("gray", alpha = 0.5),border = NA)
  lines(media_2018, lwd = 2.5, col = "#66C2A5")
  axis(1, at = seq(xmin, xmax, 30), labels = month.abb[seq(4,10,1)])
  axis(2, at = round(seq(ymin, ymax, length.out = 5)), 
       las = 2, labels = format(round(seq(ymin, ymax, length.out = 5)), scientific = F, digits = 1))
  mtext(side = 2, text = "Number of Cx. pipiens females", line = 6, 
        cex = 1.5)
}

#plot----
par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6)
    ymax = 26100 #max(qmax, na.rm = T)
    ymin = 0
    xmin = 0
    xmax = length(media)
    plot(0, axes = F, col = "white", ylim = c(ymin, ymax), 
         xlim = c(xmin, xmax), ylab = "", xlab = "", main = "Mosquito Dynamics", cex.main = 1.5)
    poligono = cbind(x = c(1:length(qmin), length(qmin):1), 
                     y = c(qmin, rev(qmax)))
    #polygon(poligono, col = adjustcolor("gray", alpha = 0.5),border = NA)
    lines(media_2016, lwd = 2.5, col ="#9E0142")
    lines(media_2017, lwd = 2.5, col ="#F46D43")
    lines(media_2018, lwd = 2.5, col ="#66C2A5")
    
    axis(1, at = seq(xmin, xmax, 30), labels = month.abb[seq(4,10,1)])
    axis(2, at = round(seq(ymin, ymax, length.out = 5)), 
         las = 2, labels = format(round(seq(ymin, ymax, length.out = 5)), scientific = F, digits = 1))
    mtext(side = 2, text = "Number of Cx. pipiens females", line = 6, 
          cex = 1.5)
    legend("topright",legend=c("2016","2017","2028"), lty = 1, lwd = 2.5,
           col = c("#9E0142","#F46D43","#66C2A5"), box.lwd = F)
  
  # dev.off()



#### plot temperature medie ----
####2016####
temp_2016 <- t(read.table("/home/nicolaferrari/Scrivania/GitHub/WestNile_project/per_mcmc/temperatura_media_2016"))
anno = 2016

Apr <- mean(temp_2016[92:121,])
May <- mean(temp_2016[122:152,])
Jun <- mean(temp_2016[153:182,])
Jul <- mean(temp_2016[183:213,])
Aug <- mean(temp_2016[214:244,])
Sep <- mean(temp_2016[245:274,])

summer_2016 <- temp_2016[92:274,]
mean_2016 <- c(Apr, May, Jun, Jul, Aug, Sep)

xmin = 0
xmax = length(media)

plot(0, col = "white", type = "l", lwd = 2, xlim = c(0, 182), ylim = c(0,30), xlab = "", ylab = "", axes = F, main = anno, cex.main = 1.5)
lines(summer_2016, lwd = 2, col = "#9E0142")


axis(1, at = seq(xmin, xmax, 30), labels = month.abb[seq(4,10,1)])


#axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
 #                31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
#                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
 #                                     "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)

axis(2, at = seq(0, 30, 5), las = 2, 
     labels = format(seq(0, 30, 5), 
                     digits = 2, scientific = F), cex.axis = cex_axes)
mtext("°C", side = 2, line = 3.5, cex = 1.5, las = 1)


xmin = 0
xmax = 6
plot(0, col = "white", type = "l", lwd = 2, xlim = c(0, xmax), ylim = c(0,30), xlab = "", ylab = "", axes = F, main = anno, cex.main = 1.5)
lines(mean_2016, lwd = 2, col = "#9E0142", type = "b", pch = 19)


axis(1, at = seq(xmin, xmax, 1), labels = month.abb[seq(4,10,1)])


#axis(1, at = c(1, 
  #             1 + 30 + 31, 
  #             1 + 30 + 31 + 30, 
  #             1 + 30 + 31 + 30 + 31, 
   #            1 + 30 + 31 + 30 + 31 + 31,
  #             1 + 30 + 31 + 30 + 31 +31 + 30),
  #   labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
  #   cex.axis = cex_axes, las = 2)

axis(2, at = seq(0, 30, 5), las = 2, 
     labels = format(seq(0, 30, 5), 
                     digits = 2, scientific = F), cex.axis = cex_axes)
mtext("°C", side = 2, line = 3.5, cex = 1.5, las = 1)
#####2017####
temp_2017 <- t(read.table("/home/nicolaferrari/Scrivania/GitHub/WestNile_project/per_mcmc/temperatura_media_2017"))
anno = 2017

Apr <- temp_2017[91:120,]
May <- temp_2017[121:151,]
Jun <- temp_2017[152:181,]
Jul <- temp_2017[182:212,]
Aug <- temp_2017[213:243,]
Sep <- temp_2017[244:273,]

summer_2017 <- temp_2017[91:273,]
mean_2017 <- c(Apr, May, Jun, Jul, Aug, Sep)


plot(0, col = "white", xlim = c(0, 182), ylim = c(0,30), xlab = "", ylab = "", axes = F, main = anno)
lines(summer_2017, lwd = 2, col = "#F46D43")

axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)

axis(2, at = seq(0, 30, 5), las = 2, 
     labels = format(seq(0, 30, 5), 
                     digits = 2, scientific = F), cex.axis = cex_axes)
mtext("°C", side = 2, line = 3.5, cex = cex_axes + 0.3, las = 2)

xmin = 0
xmax = 6
plot(0, col = "white", type = "l", lwd = 2, xlim = c(0, xmax), ylim = c(0,30), xlab = "", ylab = "", axes = F, main = anno, cex.main = 1.5)
lines(mean_2017, lwd = 2, col = "#F46D43", type = "b", pch = 19)


axis(1, at = seq(xmin, xmax, 1), labels = month.abb[seq(4,10,1)])


#axis(1, at = c(1, 
#             1 + 30 + 31, 
#             1 + 30 + 31 + 30, 
#             1 + 30 + 31 + 30 + 31, 
#            1 + 30 + 31 + 30 + 31 + 31,
#             1 + 30 + 31 + 30 + 31 +31 + 30),
#   labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept"), 
#   cex.axis = cex_axes, las = 2)

axis(2, at = seq(0, 30, 5), las = 2, 
     labels = format(seq(0, 30, 5), 
                     digits = 2, scientific = F), cex.axis = cex_axes)
mtext("°C", side = 2, line = 3.5, cex = 1.5, las = 1)

####2018####
temp_2018 <- t(read.table("/home/nicolaferrari/Scrivania/GitHub/WestNile_project/per_mcmc/temperatura_media_2018"))
anno = 2018

Apr <- mean(temp_2018[91:120,])
May <- mean(temp_2018[121:151,])
Jun <- mean(temp_2018[152:181,])
Jul <- mean(temp_2018[182:212,])
Aug <- mean(temp_2018[213:243,])
Sep <- mean(temp_2018[244:273,])

summer_2018 <- temp_2018[91:273,]
mean_2018 <- c(Apr, May, Jun, Jul, Aug, Sep)


plot(0, col = "white", xlim = c(0, 182), ylim = c(0,30), xlab = "", ylab = "", axes = F, main = anno)
lines(summer_2018, lwd = 2, col = "#66C2A5")

axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)

axis(2, at = seq(0, 30, 5), las = 2, 
     labels = format(seq(0, 30, 5), 
                     digits = 2, scientific = F), cex.axis = cex_axes)
mtext("°C", side = 2, line = 3.5, cex = cex_axes + 0.3, las = 2)


xmin = 0
xmax = 6
plot(0, col = "white", type = "l", lwd = 2, xlim = c(0, xmax), ylim = c(0,30), xlab = "", ylab = "", axes = F, main = anno, cex.main = 1.5)
lines(mean, lwd = 2, col = "#66C2A5", type = "b", pch = 19)


axis(1, at = seq(xmin, xmax, 1), labels = month.abb[seq(4,10,1)])
axis(2, at = seq(0, 30, 5), las = 2, 
     labels = format(seq(0, 30, 5), 
                     digits = 2, scientific = F), cex.axis = cex_axes)
mtext("°C", side = 2, line = 3.5, cex = cex_axes + 0.3, las = 2)

#plot confronto temperature medie 3 anni####
par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6)

xmin = 0
xmax = length(media)
plot(0, col = "white", xlim = c(0, 182), ylim = c(0,30), xlab = "", ylab = "", axes = F, main = "Temperatures")
lines(summer_2016, lwd = 2, col ="#9E0142")
lines(summer_2017,lwd = 2, col ="#F46D43")
lines(summer_2018, lwd = 2, col ="#66C2A5")

axis(1, at = c(1, 1 + 30, 1 + 30 + 31, 1 + 30 + 31 + 30, 1 + 30 + 31 + 30 + 
                 31, 1 + 30 + 31 + 30 + 31 + 31, 1 + 30 + 31 + 30 + 31 + 
                 31 + 30), labels = c("Apr", "May", "Jun", "Jul", 
                                      "Aug", "Sept", "Oct"), cex.axis = cex_axes, las = 2)

axis(2, at = seq(0, 30, 5), las = 2, 
     labels = format(seq(0, 30, 5), 
                     digits = 2, scientific = F), cex.axis = cex_axes)
mtext("°C", side = 2, line = 3.5, cex = cex_axes + 0.3, las = 2)
legend(-0.11,30,legend=c("2016","2017","2028"), lty = 1, lwd = 2.5,
       col = c("#9E0142","#F46D43","#66C2A5"), box.lwd = F)


ymax = 30 #max(qmax, na.rm = T)
ymin = 0
xmin = 0
xmax = 6
plot(0, axes = F, col = "white", ylim = c(ymin, ymax), 
     xlim = c(xmin, xmax), ylab = "", xlab = "", main = "Mean Temperatures", cex.main = 1.5)

lines(mean_2016, lwd = 2.5, col ="#9E0142", type = "b", pch = 19)
lines(mean_2017, lwd = 2.5, col ="#F46D43", type = "b", pch = 19)
lines(mean_2018, lwd = 2.5, col ="#66C2A5", type = "b", pch = 19)

axis(1, at = seq(xmin, xmax, 1), labels = month.abb[seq(4,10,1)])
axis(2, at = round(seq(ymin, ymax, length.out = 5)), 
     las = 2, labels = format(round(seq(ymin, ymax, length.out = 5)), scientific = F, digits = 1))
mtext("°C", side = 2, line = 3.5, cex = cex_axes + 0.3, las = 2)
legend(-0.11,30,legend=c("2016","2017","2028"), pch = 19, lwd = 2.5,
       col = c("#9E0142","#F46D43","#66C2A5"), box.lwd = F)
