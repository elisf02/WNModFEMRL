#plot contributo known population
# Merli ----
FolderSimu = "Output_WNV/Merli/Simulazioni/"
FileDynName = "dynamics_2spec_Merli_"
anno = 2016  #cambia l'anno di volta in volta
quale_cluster = 1
numero_classi = 11

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

b11 = 0.35 # usa biting rate specie nota
b1 = mean(output_mcmc[, 'b1_ca']) #usa bititng rate calcolato da MCMC (per avian community)

pop_tot = apply((BS + BE + BI + BR), MARGIN = 2, mean)
pop_tot2 = apply((BS2 + BE2 + BI2 + BR2), MARGIN = 2, mean)

num = b11*(apply(BI, MARGIN = 2, mean)/pop_tot)
den = b11*(apply(BI, MARGIN = 2, mean)/pop_tot) + b1*(apply(BI2, MARGIN = 2, mean)/pop_tot2)
contr_known_bird = (num/den)*100

par(mfrow = c(1,1))

plot(contr_known_bird, col = "red", xlim = c(0, 180), ylim = c(0, 100),
     xlab = "", ylab = "", axes = F, main = paste("% of Blackbird contribution", anno), type = 'l')

axis(1, at = c(1, 1+30, 1+30 + 31, 1+30 + 31 + 30, 1+30 + 31 + 30 +
                 31, 1+30 + 31 + 30 + 31 + 31, 1+30 + 31 + 30 + 31 +
                 31 + 30),
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept","Oct"),
     cex.axis = 1, las = 2)
axis(2, at = NULL)


#plot(contr_known_bird, col='red', main = "Blackbirds contribution 2016", type = "l")


#Gazze ----
FolderSimu = "Output_WNV/Gazze/Simulazioni/"
FileDynName = "dynamics_2spec_Gazze_"
anno = 2018
quale_cluster = 1
numero_classi = 11

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

plot(contr_known_bird, col = "red", xlim = c(0, 180), ylim = c(0,40),
     xlab = "", ylab = "", axes = F, main = paste("% of Magpie contribution", anno), type = 'l')

axis(1, at = c(1, 1+30, 1+30 + 31, 1+30 + 31 + 30, 1+30 + 31 + 30 +
                 31, 1+30 + 31 + 30 + 31 + 31, 1+30 + 31 + 30 + 31 +
                 31 + 30),
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept","Oct"),
     cex.axis = 1, las = 2)
axis(2, at = NULL)
#plot(contr_known_bird, col='red', main = "contrib Gazze 2018 (%)", type = "l") #sistema

#Germani ----
FolderSimu = "Output_WNV/Germani/Simulazioni/"
FileDynName = "dynamics_2spec_Germani_"
anno = 2018
quale_cluster = 1
numero_classi = 11

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

plot(contr_known_bird, col = "red", xlim = c(0, 180), ylim = c(0, 0.3),
     xlab = "", ylab = "", axes = F, main = paste("% of Mallard contribution", anno), type = 'l')

axis(1, at = c(1, 1+30, 1+30 + 31, 1+30 + 31 + 30, 1+30 + 31 + 30 +
                 31, 1+30 + 31 + 30 + 31 + 31, 1+30 + 31 + 30 + 31 +
                 31 + 30),
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept","Oct"),
     cex.axis = 1, las = 2)
axis(2, at = NULL)
#plot(contr_known_bird, col='red', main = "contrib Germani 2018 (%)", type = "l") #sistema

#Cornacchie ----
FolderSimu = "Output_WNV/Cornacchie/Simulazioni/"
FileDynName = "dynamics_2spec_Cornacchie_"
anno = 2018  #cambia l'anno di volta in volta
quale_cluster = 1
numero_classi = 11

nome_file_output_dynamics = paste0(FolderSimu,
                                   FileDynName, "M_", anno, "_", quale_cluster,
                                   ".txt")

nome_file_parametri = paste0("Output_WNV/Cornacchie/MCMC/parametri_2spec_Cornacchie_M_", anno,
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

b11 = 0.009 #usa biting rate specie nota
b1 = mean(output_mcmc[, 'b1_ca']) #usa bititng rate calcolato da MCMC (per avian community)

pop_tot = apply((BS + BE + BI + BR), MARGIN = 2, mean)
pop_tot2 = apply((BS2 + BE2 + BI2 + BR2), MARGIN = 2, mean)

num = b11*(apply(BI, MARGIN = 2, mean)/pop_tot)
den = b11*(apply(BI, MARGIN = 2, mean)/pop_tot) + b1*(apply(BI2, MARGIN = 2, mean)/pop_tot2)
contr_known_bird = (num/den)*100

par(mfrow = c(1,1))

plot(contr_known_bird, col = "red", xlim = c(0, 180), ylim = c(0, 0.3),
     xlab = "", ylab = "", axes = F, main = paste("% of Crow contribution", anno), type = 'l')

axis(1, at = c(1, 1+30, 1+30 + 31, 1+30 + 31 + 30, 1+30 + 31 + 30 +
                 31, 1+30 + 31 + 30 + 31 + 31, 1+30 + 31 + 30 + 31 +
                 31 + 30),
     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept","Oct"),
     cex.axis = 1, las = 2)
axis(2, at = NULL)

#plot(contr_known_bird, col='red', main = "contrib Cornacchie 2018 (%)", type = "l") #sistema

