#####################05_Epidemiological_model_casestudy#######################
setwd("/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/WNModFEMRL")
####Boxplot_MCMC####
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







#####Confronto boxplot####
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
