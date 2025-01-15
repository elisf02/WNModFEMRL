# plot sensitivity ----
############################## muB ----
changing_par = 'muB'

nome_file_parametri = paste0("Output_WNV/MCMC/parametri_sens_MeadiaAllYears_",
                                   changing_par, 
                                   ".txt")
parametri_sensitivity = read.table(nome_file_parametri)
colnames(parametri_sensitivity) = c("p", "B0", "pR", "b1", "muB", "s", "phi", 
                                                   "niB", "recB")

#par(mfrow = c(2,2))
# birth pulse ----
birth_pulse = apply(parametri_sensitivity, MARGIN = 1, function(a) {
  with(as.list(a), muB * exp(-s * sin(pi * (seq(0, 
                                                365)/365 - phi))^2)/besselI(s/2, 0, T))
})
ymax = max(birth_pulse)

plot(0, col = "white", xlim = c(0, 365), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("birth pulse")) #, quale_cluster))
colori = rainbow(100)
for (i in 1:ncol(birth_pulse)) {
  lines(as.numeric(birth_pulse[,i]), col = colori[i])
}
#apply(pop, MARGIN = 1, lines)
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
min = 0/365
max = 8/365
var_par =  seq(min, max, length.out = 100)

legend('topleft', legend = round(var_par,4)[seq(1,100, 9)]*365 , cex = 0.8, 
       lwd = 4, col = colori[seq(1,100, 9)])
# zanzare infette 2016 ----
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

pop = MI 
ymax = max(pop)

plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito Infected", anno)) #, quale_cluster))
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
# zanzare infette 2017 ----
anno = 2017
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

pop = MI 
ymax = max(pop)

plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito Infected", anno)) #, quale_cluster))
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
# zanzare infette 2018 ----
anno = 2018
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

pop = MI 
ymax = max(pop)

plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito Infected", anno)) #, quale_cluster))
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


############################## s ----
changing_par = 's'

nome_file_parametri = paste0("Output_WNV/MCMC/parametri_sens_MeadiaAllYears_",
                             changing_par, 
                             ".txt")
parametri_sensitivity = read.table(nome_file_parametri)
colnames(parametri_sensitivity) = c("p", "B0", "pR", "b1", "muB", "s", "phi", 
                                    "niB", "recB")

#par(mfrow = c(2,2))
# birth pulse ----
birth_pulse = apply(parametri_sensitivity, MARGIN = 1, function(a) {
  with(as.list(a), muB * exp(-s * sin(pi * (seq(0, 
                                                365)/365 - phi))^2)/besselI(s/2, 0, T))
})
ymax = max(birth_pulse)

plot(0, col = "white", xlim = c(0, 365), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("birth pulse")) #, quale_cluster))
colori = rainbow(100)
for (i in 1:ncol(birth_pulse)) {
  lines(as.numeric(birth_pulse[,i]), col = colori[i])
}
#apply(pop, MARGIN = 1, lines)
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
min = 5
max = 20
var_par =  seq(min, max, length.out = 100)
legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.7, 
       lwd = 4, col = colori[seq(1,100, 9)])
# zanzare infette 2016 ----
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

pop = MI 
ymax = max(pop)

plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito Infected", anno)) #, quale_cluster))
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
# zanzare infette 2017 ----
anno = 2017
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

pop = MI 
ymax = max(pop)

plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito Infected", anno)) #, quale_cluster))
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
# zanzare infette 2018 ----
anno = 2018
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

pop = MI 
ymax = max(pop)

plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito Infected", anno)) #, quale_cluster))
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


############################## phi ----
changing_par = 'phi'

nome_file_parametri = paste0("Output_WNV/MCMC/parametri_sens_MeadiaAllYears_",
                             changing_par, 
                             ".txt")
parametri_sensitivity = read.table(nome_file_parametri)
colnames(parametri_sensitivity) = c("p", "B0", "pR", "b1", "muB", "s", "phi", 
                                    "niB", "recB")

#par(mfrow = c(2,2))
# birth pulse ----
birth_pulse = apply(parametri_sensitivity, MARGIN = 1, function(a) {
  with(as.list(a), muB * exp(-s * sin(pi * (seq(0, 
                                                365)/365 - phi))^2)/besselI(s/2, 0, T))
})
ymax = max(birth_pulse)

plot(0, col = "white", xlim = c(0, 365), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("birth pulse")) #, quale_cluster))
colori = rainbow(100)
for (i in 1:ncol(birth_pulse)) {
  lines(as.numeric(birth_pulse[,i]), col = colori[i])
}
#apply(pop, MARGIN = 1, lines)
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

min = 0.33
max = 0.66
var_par =  seq(min, max, length.out = 100) # importarlo dal file di simulazioni

legend('topleft', legend = round(var_par,4)[seq(1,100, 9)] , cex = 0.7, 
       lwd = 4, col = colori[seq(1,100, 9)])

# zanzare infette 2016 ----
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

pop = MI 
ymax = max(pop)

plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito Infected", anno)) #, quale_cluster))
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
# zanzare infette 2017 ----
anno = 2017
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

pop = MI 
ymax = max(pop)

plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito Infected", anno)) #, quale_cluster))
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
# zanzare infette 2018 ----
anno = 2018
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

pop = MI 
ymax = max(pop)

plot(0, col = "white", xlim = c(0, xmax), ylim = c(0, ymax), 
     xlab = "", ylab = "", axes = F, main = paste("Mosquito Infected", anno)) #, quale_cluster))
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
