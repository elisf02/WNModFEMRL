library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(readxl) #to import dataset from excel

library("ggbreak") # cite S Xu, M Chen, T Feng, L Zhan, L Zhou, G Yu. Use ggbreak to effectively utilize plotting space to deal with large datasets and outliers. Frontiers in Genetics. 2021, 12:774846. doi: 10.3389/fgene.2021.774846
library("ggbreak") # cite S Xu, M Chen, T Feng, L Zhan, L Zhou, G Yu. Use ggbreak to effectively utilize plotting space to deal with large datasets and outliers. Frontiers in Genetics. 2021, 12:774846. doi: 10.3389/fgene.2021.774846

# colori e varie ----
set.seed(0)
palette('Set1')

okabe_ito_palette <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
  "#D55E00", "#CC79A7", "#000000", "#999999", "#117733"
)
'#CC7722': Ochre
'#D2691E': Chocolate
'#FF8C00': Dark Orange
'#FFA500': Orange
'#FF7F50': Coral


redorange_palette <- colorRampPalette(c("#FFCC00", "#FF9900", "#CC3300"))
orange_palette <- colorRampPalette(c("#FFCC00", "#FF9900", "#CC7722"))
green_palette <- colorRampPalette(c("#E5F5E0", "#A1D99B", "#31A354"))
blue_palette <- colorRampPalette(c("#DEEBF7", "#9ECAE1", "#3182BD"))
purple_palette <- colorRampPalette(c("#F2F0F7", "#CBC9E2", "#756BB1"))
dark_green_palette <- colorRampPalette(c("#A1D99B", "#31A354", "#006D2C"))
dark_blue_palette <- colorRampPalette(c("#9ECAE1", "#3182BD", "#08519C"))
dark_purple_palette <- colorRampPalette(c("#CBC9E2", "#756BB1", "#54278F"))

custom_colors <- c("p1" = "grey","p2" = "grey", "p3" = "grey", "p4" = "grey", "p5" = "grey",
                   "p6" = "grey", "p7" = "grey", "p8" = "grey", "p9" = "grey", "p10" = "grey",
                   "p11" = "grey","p12" = "grey", "p13" = "grey", "p14" = "grey", "p15" = "grey",
                   "p16" = "grey", "p17" = "grey", "p18" = "grey", "p19" = "grey", "p20" = "grey",
                   "p21" = "grey", "p22" = "grey", "p23" = "grey", "p24" = "grey", "p25" = "grey",
                   "p26" = "grey", "p27" = "grey", "p28" = "grey", "p29" = "grey", "p30" = "grey",
                   "p31" = "grey","p32" = "grey", "p33" = "grey", "p34" = "grey", "p35" = "grey",
                   "p36" = "grey", "p37" = "grey", "p38" = "grey", "p39" = "grey", "p40" = "grey",
                   "p41" = "grey","p42" = "grey", "p43" = "grey", "p44" = "grey", "p45" = "grey",
                   "p46" = "grey", "p47" = "grey", "p48" = "grey", "p49" = "grey", "p50" = "grey",
                   "p51" = "grey","p52" = "grey", "p53" = "grey", "p54" = "grey", "p55" = "grey",
                   "p56" = "grey", "p57" = "grey", "p58" = "grey", "p59" = "grey", "p60" = "grey",
                   "p61" = "grey","p62" = "grey", "p63" = "grey", "p64" = "grey", "p65" = "grey",
                   "p66" = "grey", "p67" = "grey", "p68" = "grey", "p69" = "grey", "p70" = "grey",
                   "p71" = "grey","p72" = "grey", "p73" = "grey", "p74" = "grey", "p75" = "grey",
                   "p76" = "grey", "p77" = "grey", "p78" = "grey", "p79" = "grey", "p80" = "grey",
                   "p81" = "grey","p82" = "grey", "p83" = "grey", "p84" = "grey", "p85" = "grey",
                   "p86" = "grey", "p87" = "grey", "p88" = "grey", "p89" = "grey", "p90" = "grey",
                   "p91" = "grey","p92" = "grey", "p93" = "grey", "p94" = "grey", "p95" = "grey",
                   "p96" = "grey", "p97" = "grey", "p98" = "grey", "p99" = "grey", "p100" = "grey")

# Generate 10 gradient colors
# gradient_colors <- dark_purple_palette(10)
gradient_colors <- orange_palette(10)
custom_colors2 <- c("p1" = gradient_colors[1],"p2" = gradient_colors[2], "p3" = gradient_colors[3],
                    "p4" = gradient_colors[4], "p5" = gradient_colors[5], "p6" = gradient_colors[6],
                    "p7" = gradient_colors[7], "p8" = gradient_colors[8], "p9" = gradient_colors[9],
                    "p10" = gradient_colors[10])

# birth pulse per capire phi ----
muB = 5/365 # 0.007874999
s = 30 # 11.83775
phi = 0.66 # 0.4 # 0.5878114
ntdf = data.frame(time = seq(1, 365)) %>%
  mutate(nt = muB * exp(-s * sin(pi * (time/365 - phi))^2)/besselI(s/2, 0, T))

ggplot() +
  geom_line(data = ntdf, aes(x = time, y = nt), linewidth =1.2, color = '#4DAF4A') +
  geom_vline(xintercept = 241)


# Valori parametri usati (da MCMC) ----
UnknParms = read.table('V1_Sept/WNModFEMRL/Output_WNV/MCMC/parametri_MediaAllYears.txt')
colnames(UnknParms) = c("p", "B0", "pR", "b1", "muB",
                        "s", "phi", "niB", "recB")
UnknParms

# sensitivity ----
muBBase = 0.007874999
sBase = 11.83775
phiBase = 0.5878114
ntdf = data.frame(yday = seq(1, 365)) %>%
  mutate(nt = muBBase * exp(-sBase * sin(pi * (yday/365 - phiBase))^2)/besselI(sBase/2, 0, T))

Sens = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/MCMC/parametri_sens_MeadiaAllYears_muB.txt'))
muB = Sens$V5
muBshort = Sens$V5[seq(1,100, 10)]

min(muB)
max(muB)

min(muB)*365
max(muB)*365

# plot
par = muB
tmp = data.frame(sapply(muB, FUN = function(a) {
  a * exp(-sBase * sin(pi * (seq(1,365)/365 - phiBase))^2)/besselI(sBase/2, 0, T)
}))
colnames(tmp) = paste0('p', seq(1,100))
tmp = tmp %>%
  mutate(yday = seq(1, 365)) %>%
  pivot_longer(1:100)

tmpshort= data.frame(sapply(muBshort, FUN = function(a) {
  a * exp(-sBase * sin(pi * (seq(1,365)/365 - phiBase))^2)/besselI(sBase/2, 0, T)
}))
colnames(tmpshort) = paste0('p', seq(1,10))
tmpshort = tmpshort %>%
  mutate(yday = seq(1, 365)) %>%
  pivot_longer(1:10)

p1 = ggplot() +
  geom_line(data = tmp, aes(x = yday, y = value, col = name), linewidth = 1, alpha = 0.5) +
  geom_line(data = ntdf, aes(x = yday, y = nt), linewidth = 1.2) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = expression("n"[t])         # Custom y-axis label
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(0, 365),
                     breaks = c(0,
                                28,
                                28+31,
                                31+28+30,
                                31+28+30+31,
                                31+28+30+31+30,
                                31+28+30+31+30+31,
                                31+28+30+31+30+31+31,
                                28+31+30+31+30+31+31+30,
                                28+31+30+31+30+31+31+30+31,
                                28+31+30+31+30+31+31+30+31+30,
                                28+31+30+31+30+31+31+30+31+30+31),
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
  ggtitle(expression(paste("A)           ", mu[B])))
p1
p1short = ggplot() +
  geom_line(data = tmpshort, aes(x = yday, y = value, col = name), linewidth = 1) +
  scale_color_manual(values = custom_colors2) +  # Apply custom colors
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = expression("n"[t])         # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(0, 365),
                     breaks = c(0,
                                28,
                                28+31,
                                31+28+30,
                                31+28+30+31,
                                31+28+30+31+30,
                                31+28+30+31+30+31,
                                31+28+30+31+30+31+31,
                                28+31+30+31+30+31+31+30,
                                28+31+30+31+30+31+31+30+31,
                                28+31+30+31+30+31+31+30+31+30,
                                28+31+30+31+30+31+31+30+31+30+31),
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
  ggtitle(expression(paste("A)               ", mu[B])))
p1short

Sens = read.table('V1_Sept/WNModFEMRL/Output_WNV/MCMC/parametri_sens_MeadiaAllYears_s.txt')
s = Sens$V6
sshort = Sens$V6[seq(1,100, 10)]

min(s)
max(s)

par = s
tmp = data.frame(sapply(par, FUN = function(a) {
  muBBase * exp(-a * sin(pi * (seq(1,365)/365 - phiBase))^2)/besselI(a/2, 0, T)
}))
colnames(tmp) = paste0('p', seq(1,100))
tmp = tmp %>%
  mutate(yday = seq(1, 365)) %>%
  pivot_longer(1:100)

tmpshort= data.frame(sapply(sshort, FUN = function(a) {
  muBBase * exp(-a * sin(pi * (seq(1,365)/365 - phiBase))^2)/besselI(a/2, 0, T)
}))
colnames(tmpshort) = paste0('p', seq(1,10))
tmpshort = tmpshort %>%
  mutate(yday = seq(1, 365)) %>%
  pivot_longer(1:10)

p2 = ggplot() +
  geom_line(data = tmp, aes(x = yday, y = value, col = name), linewidth = 1, alpha = 0.5) +
  geom_line(data = ntdf, aes(x = yday, y = nt), linewidth = 1.2) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = "", #expression("n"[t])         # Custom y-axis label
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(0, 365),
                     breaks = c(0,
                                28,
                                28+31,
                                31+28+30,
                                31+28+30+31,
                                31+28+30+31+30,
                                31+28+30+31+30+31,
                                31+28+30+31+30+31+31,
                                28+31+30+31+30+31+31+30,
                                28+31+30+31+30+31+31+30+31,
                                28+31+30+31+30+31+31+30+31+30,
                                28+31+30+31+30+31+31+30+31+30+31),
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
  ggtitle(expression(paste("B)           ", s)))
p2

p2short = ggplot() +
  geom_line(data = tmpshort, aes(x = yday, y = value, col = name), linewidth = 1) +
  scale_color_manual(values = custom_colors2) +  # Apply custom colors
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = "", #expression("n"[t])         # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(0, 365),
                     breaks = c(0,
                                28,
                                28+31,
                                31+28+30,
                                31+28+30+31,
                                31+28+30+31+30,
                                31+28+30+31+30+31,
                                31+28+30+31+30+31+31,
                                28+31+30+31+30+31+31+30,
                                28+31+30+31+30+31+31+30+31,
                                28+31+30+31+30+31+31+30+31+30,
                                28+31+30+31+30+31+31+30+31+30+31),
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
  ggtitle(expression(paste("B)               ", s)))
p2short


Sens = read.table('V1_Sept/WNModFEMRL/Output_WNV/MCMC/parametri_sens_MeadiaAllYears_phi.txt')
phi = Sens$V7
phishort = Sens$V7[seq(1,100, 10)]

phi =  phi  %% 1
min(phi)
max(phi)
phishort = phishort %% 1
par = phi
tmp = data.frame(sapply(par, FUN = function(a) {
  muBBase * exp(-sBase * sin(pi * (seq(1,365)/365 - a))^2)/besselI(sBase/2, 0, T)
}))
colnames(tmp) = paste0('p', seq(1,100))
tmp = tmp %>%
  mutate(yday = seq(1, 365)) %>%
  pivot_longer(1:100)

tmpshort= data.frame(sapply(phishort, FUN = function(a) {
  muBBase * exp(-sBase * sin(pi * (seq(1,365)/365 - a))^2)/besselI(sBase/2, 0, T)
}))
colnames(tmpshort) = paste0('p', seq(1,10))
tmpshort = tmpshort %>%
  mutate(yday = seq(1, 365)) %>%
  pivot_longer(1:10)

p3 = ggplot() +
  geom_line(data = tmp, aes(x = yday, y = value, col = name), linewidth = 1, alpha = 0.5) +
  geom_line(data = ntdf, aes(x = yday, y = nt), linewidth = 1.2) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = "", #expression("n"[t])         # Custom y-axis label
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(0, 365),
                     breaks = c(0,
                                28,
                                28+31,
                                31+28+30,
                                31+28+30+31,
                                31+28+30+31+30,
                                31+28+30+31+30+31,
                                31+28+30+31+30+31+31,
                                28+31+30+31+30+31+31+30,
                                28+31+30+31+30+31+31+30+31,
                                28+31+30+31+30+31+31+30+31+30,
                                28+31+30+31+30+31+31+30+31+30+31),
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
  ggtitle(expression(paste("C)           ", phi)))

p3short = ggplot() +
  geom_line(data = tmpshort, aes(x = yday, y = value, col = name), linewidth = 1) +
  scale_color_manual(values = custom_colors2) +  # Apply custom colors
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = "", #expression("n"[t])         # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(0, 365),
                     breaks = c(0,
                                28,
                                28+31,
                                31+28+30,
                                31+28+30+31,
                                31+28+30+31+30,
                                31+28+30+31+30+31,
                                31+28+30+31+30+31+31,
                                28+31+30+31+30+31+31+30,
                                28+31+30+31+30+31+31+30+31,
                                28+31+30+31+30+31+31+30+31+30,
                                28+31+30+31+30+31+31+30+31+30+31),
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
  ggtitle(expression(paste("C)               ", phi)))



ggarrange(p1, p2, p3,  common.legend = F, ncol = 3, labels = NULL)
ggarrange(p1short, p2short, p3short,  common.legend = F, ncol = 3, labels = NULL)
# 1300x400

# plot of the birth pulse function example ----
muB = 2/365 # 0.007874999
s = 10 # 11.83775
phi = 0.4 # 0.5878114
ntdf1 = data.frame(time = seq(1, 365)) %>%
  mutate(nt = muB * exp(-s * sin(pi * (time/365 - phi))^2)/besselI(s/2, 0, T))
muB = 5/365 # 0.007874999
s = 30 # 11.83775
phi = 0.47 # 0.4 # 0.5878114
ntdf2 = data.frame(time = seq(1, 365)) %>%
  mutate(nt = muB * exp(-s * sin(pi * (time/365 - phi))^2)/besselI(s/2, 0, T))

ggplot() +
  geom_line(data = ntdf1, aes(x = time, y = nt), linewidth =1.2, color = '#4DAF4A') + #'#66C2A5')
  geom_line(data = ntdf2, aes(x = time, y = nt), linewidth =1.2, color = '#FF7F00') + #'#FC8D62')
  theme_minimal() +
  labs(x = "",
       y = expression("n"[t])) +
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.1)),# rel(1.4)),
        axis.text = element_text(size = rel(1)),#rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(0, 365),
                     breaks = c(0,
                                28,
                                28+31,
                                31+28+30,
                                31+28+30+31,
                                31+28+30+31+30,
                                31+28+30+31+30+31,
                                31+28+30+31+30+31+31,
                                28+31+30+31+30+31+31+30,
                                28+31+30+31+30+31+31+30+31,
                                28+31+30+31+30+31+31+30+31+30,
                                28+31+30+31+30+31+31+30+31+30+31),
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
# 500x300

# plot of the birth pulse function MCMC ----
set.seed(0)
nomi_parametri = c("p", "B0", "pR", "b1", "muB",
                   "s", "phi", "niB", "recB")
ParmsMCMC_2016 = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/MCMC/parametri_M_2016_1.txt'))
burnin = nrow(ParmsMCMC_2016) * 0.1
ParmsMCMC_2016 = ParmsMCMC_2016[-c(1:burnin1),-(length(nomi_parametri)+1)]

ParmsMCMC_2017 = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/MCMC/parametri_M_2017_1.txt'))
burnin = nrow(ParmsMCMC_2017) * 0.1
ParmsMCMC_2017 = ParmsMCMC_2017[-c(1:burnin1),-(length(nomi_parametri)+1)]

ParmsMCMC_2018 = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/MCMC/parametri_M_2018_1.txt'))
burnin = nrow(ParmsMCMC_2018) * 0.1
ParmsMCMC_2018 = ParmsMCMC_2018[-c(1:burnin1),-(length(nomi_parametri)+1)]

colnames(ParmsMCMC_2016) = c("p", "B0", "pR", "b1", "muB",
                             "s", "phi", "niB", "recB")
colnames(ParmsMCMC_2017) = c("p", "B0", "pR", "b1", "muB",
                             "s", "phi", "niB", "recB")
colnames(ParmsMCMC_2018) = c("p", "B0", "pR", "b1", "muB",
                             "s", "phi", "niB", "recB")

nt_2016 = t(data.frame(apply(ParmsMCMC_2016, MARGIN = 1, FUN = function(a){
  as.numeric(a['muB']) * exp(-as.numeric(a['s']) * sin(pi * (seq(1,365)/365 - as.numeric(a['phi'])))^2)/besselI(as.numeric(a['s'])/2, 0, T)
})))
nt_2017 = t(data.frame(apply(ParmsMCMC_2017, MARGIN = 1, FUN = function(a){
  as.numeric(a['muB']) * exp(-as.numeric(a['s']) * sin(pi * (seq(1,365)/365 - as.numeric(a['phi'])))^2)/besselI(as.numeric(a['s'])/2, 0, T)
})))
nt_2018 = t(data.frame(apply(ParmsMCMC_2018, MARGIN = 1, FUN = function(a){
  as.numeric(a['muB']) * exp(-as.numeric(a['s']) * sin(pi * (seq(1,365)/365 - as.numeric(a['phi'])))^2)/besselI(as.numeric(a['s'])/2, 0, T)
})))

colnames(nt_2016) = seq(1,365)
colnames(nt_2017) = seq(1,365)
colnames(nt_2018) = seq(1,365)

df2016 = data.frame(time = seq(1, 365)) %>%
  mutate(mean = apply(nt_2016, MARGIN = 2, FUN = mean),
         qmin = apply(nt_2016, MARGIN = 2, FUN = function(a){
           quantile(a, probs = 0.025)
         }),
         qmax = apply(nt_2016, MARGIN = 2, FUN = function(a){
           quantile(a, probs = 0.975)
         })
  )
df2017 = data.frame(time = seq(1, 365)) %>%
  mutate(mean = apply(nt_2017, MARGIN = 2, FUN = mean),
         qmin = apply(nt_2017, MARGIN = 2, FUN = function(a){
           quantile(a, probs = 0.025)
         }),
         qmax = apply(nt_2017, MARGIN = 2, FUN = function(a){
           quantile(a, probs = 0.975)
         })
  )
df2018 = data.frame(time = seq(1, 365)) %>%
  mutate(mean = apply(nt_2018, MARGIN = 2, FUN = mean),
         qmin = apply(nt_2018, MARGIN = 2, FUN = function(a){
           quantile(a, probs = 0.025)
         }),
         qmax = apply(nt_2018, MARGIN = 2, FUN = function(a){
           quantile(a, probs = 0.975)
         })
  )


p1 = df2016 %>% ggplot() +
  geom_rect(aes(xmin = 90, xmax = 300, ymin = 0, ymax = 0.13),
            color = "#FFDB33", linewidth = 1.2, fill = NA, alpha = 0.002) +  # Colored rectangle
  geom_line(aes(x = time, y = mean), color = "black") +  # Line for mean
  geom_ribbon(aes(x = time,
                  ymin = qmin, ymax = qmax), alpha = 0.2) +
  theme_minimal() +
  labs(x = "",
       y = expression("n"[t])) +
  ggtitle(2016) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.1)),# rel(1.4)),
        axis.text = element_text(size = rel(1)),#rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(0, 365),
                     breaks = c(0,
                                28,
                                28+31,
                                31+28+30,
                                31+28+30+31,
                                31+28+30+31+30,
                                31+28+30+31+30+31,
                                31+28+30+31+30+31+31,
                                28+31+30+31+30+31+31+30,
                                28+31+30+31+30+31+31+30+31,
                                28+31+30+31+30+31+31+30+31+30,
                                28+31+30+31+30+31+31+30+31+30+31),
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
  ylim(0,0.13)
p2 = df2017 %>% ggplot() +
  geom_rect(aes(xmin = 90, xmax = 300, ymin = 0, ymax = 0.13),
            color = "#FFDB33", linewidth = 1.2, fill = NA, alpha = 0.002) +  # Colored rectangle
  geom_line(aes(x = time, y = mean), color = "black") +  # Line for mean
  geom_ribbon(aes(x = time,
                  ymin = qmin, ymax = qmax), alpha = 0.2) +
  theme_minimal() +
  labs(x = "",
       y = '') +
  ggtitle(2017) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.1)),# rel(1.4)),
        axis.text = element_text(size = rel(1)),#rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(0, 365),
                     breaks = c(0,
                                28,
                                28+31,
                                31+28+30,
                                31+28+30+31,
                                31+28+30+31+30,
                                31+28+30+31+30+31,
                                31+28+30+31+30+31+31,
                                28+31+30+31+30+31+31+30,
                                28+31+30+31+30+31+31+30+31,
                                28+31+30+31+30+31+31+30+31+30,
                                28+31+30+31+30+31+31+30+31+30+31),
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
  ylim(0,0.13)


p3 = df2018 %>% ggplot() +
  geom_rect(aes(xmin = 90, xmax = 300, ymin = 0, ymax = 0.13),
            color = "#FFDB33", linewidth = 1.2, fill = NA, alpha = 0.002) +  # Colored rectangle
  geom_line(aes(x = time, y = mean), color = "black") +  # Line for mean
  geom_ribbon(aes(x = time,
                  ymin = qmin, ymax = qmax), alpha = 0.2) +
  theme_minimal() +
  labs(x = "",
       y = '') +
  ggtitle(2018) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.1)),# rel(1.4)),
        axis.text = element_text(size = rel(1)),#rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(0, 365),
                     breaks = c(0,
                                28,
                                28+31,
                                31+28+30,
                                31+28+30+31,
                                31+28+30+31+30,
                                31+28+30+31+30+31,
                                31+28+30+31+30+31+31,
                                28+31+30+31+30+31+31+30,
                                28+31+30+31+30+31+31+30+31,
                                28+31+30+31+30+31+31+30+31+30,
                                28+31+30+31+30+31+31+30+31+30+31),
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
  ylim(0,0.13)

ggarrange(p1, p2, p3,  common.legend = F, ncol = 3, labels = NULL)
# 1300x400

max(df2016$mean)
max(df2017$mean)
max(df2018$mean)

plot(df2016$mean, type = 'l')
abline(v = 236)
236/30

plot(df2017$mean, type = 'l')
abline(v = 6.9*30)

plot(df2016$mean, type = 'l')
abline(v = 8*30)

# plot of the dynamics ----
simu_2016 = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/Simulazioni/dynamics_ParmsMedi_MediaAllYears_2016_1.txt'))
simu_2017 = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/Simulazioni/dynamics_ParmsMedi_MediaAllYears_2017_1.txt'))
simu_2018 = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/Simulazioni/dynamics_ParmsMedi_MediaAllYears_2018_1.txt'))

dim(simu_2016)

df2016 = data.frame(time = seq(1,180),
                    MS = as.numeric(simu_2016[1,]),
                    ME = as.numeric(simu_2016[2,]),
                    MI = as.numeric(simu_2016[3,]),
                    BS = as.numeric(simu_2016[4,]),
                    BE = as.numeric(simu_2016[5,]),
                    BI = as.numeric(simu_2016[6,]),
                    BR = as.numeric(simu_2016[7,])) %>%
  mutate(Mprev = MI/(MS+ME+MI),
         Bprev = BI/(BS+BE+BI+BR))

df2017 = data.frame(time = seq(1,180),
                    MS = as.numeric(simu_2017[1,]),
                    ME = as.numeric(simu_2017[2,]),
                    MI = as.numeric(simu_2017[3,]),
                    BS = as.numeric(simu_2017[4,]),
                    BE = as.numeric(simu_2017[5,]),
                    BI = as.numeric(simu_2017[6,]),
                    BR = as.numeric(simu_2017[7,])) %>%
  mutate(Mprev = MI/(MS+ME+MI),
         Bprev = BI/(BS+BE+BI+BR))

df2018 = data.frame(time = seq(1,180),
                    MS = as.numeric(simu_2018[1,]),
                    ME = as.numeric(simu_2018[2,]),
                    MI = as.numeric(simu_2018[3,]),
                    BS = as.numeric(simu_2018[4,]),
                    BE = as.numeric(simu_2018[5,]),
                    BI = as.numeric(simu_2018[6,]),
                    BR = as.numeric(simu_2018[7,])) %>%
  mutate(Mprev = MI/(MS+ME+MI),
         Bprev = BI/(BS+BE+BI+BR))

p1 = df2016 %>% ggplot() +
  geom_line(aes(x = time, y = Mprev*100), color = "#E69F00", linewidth = rel(1.2)) +   # Line for mean
  # geom_line(aes(x = time, y = Bprev*100), color = "#FF7F00", linewidth = rel(1.2)) +   # Line for mean
  theme_minimal() +
  labs(x = "",
       y = "Prevalence (%)") +
  ggtitle(2016) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),# rel(1.4)),
        axis.text = element_text(size = rel(1)),#rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
   scale_x_continuous(limits = c(-1,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
 ylim(0,1) # ylim(0,10)

p2 = df2017 %>% ggplot() +
  geom_line(aes(x = time, y = Mprev*100), color = "#E69F00", linewidth = rel(1.2)) +   # Line for mean
  # geom_line(aes(x = time, y = Bprev*100), color = "#FF7F00", linewidth = rel(1.2)) +   # Line for mean
  theme_minimal() +
  labs(x = "",
       y = "") +
  ggtitle(2017) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),# rel(1.4)),
        axis.text = element_text(size = rel(1)),#rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(-1,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
  ylim(0,1) # ylim(0,10)

p3 = df2018 %>% ggplot() +
  geom_line(aes(x = time, y = Mprev*100), color = "#E69F00", linewidth = rel(1.2)) +   # Line for mean
  # geom_line(aes(x = time, y = Bprev*100), color = "#FF7F00", linewidth = rel(1.2)) +   # Line for mean
  theme_minimal() +
  labs(x = "",
       y = "") +
  ggtitle(2018) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),# rel(1.4)),
        axis.text = element_text(size = rel(1)),#rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(-1,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
  ylim(0,1) # ylim(0,10)

ggarrange(p1, p2, p3,  common.legend = F, ncol = 3, labels = NULL)
# 1300x400

# prevalence
p1 = df2016 %>% ggplot() +
  geom_line(aes(x = time, y = MI), color = "#E69F00", linewidth = rel(1.2)) +   # Line for mean
  theme_minimal() +
  labs(x = "",
       y = expression("M"[I]) ) +        # Custom y-axis label
       ggtitle(2016) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),# rel(1.4)),
        axis.text = element_text(size = rel(1)),#rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(-1,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
  ylim(0,50)

p2 = df2017 %>% ggplot() +
  geom_line(aes(x = time, y = MI), color = "#E69F00", linewidth = rel(1.2)) +   # Line for mean
  theme_minimal() +
  labs(x = "",
       y = "") +
  ggtitle(2017) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),# rel(1.4)),
        axis.text = element_text(size = rel(1)),#rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(-1,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
  ylim(0,50)

p3 = df2018 %>% ggplot() +
  geom_line(aes(x = time, y = MI), color = "#E69F00", linewidth = rel(1.2)) +   # Line for mean
  theme_minimal() +
  labs(x = "",
       y = "") +
  ggtitle(2018) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),# rel(1.4)),
        axis.text = element_text(size = rel(1)),#rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(-1,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
  ylim(0,50)

ggarrange(p1, p2, p3,  common.legend = F, ncol = 3, labels = NULL)
# 1300x400

max(df2016$MI)
max(df2017$MI)
max(df2018$MI)

# plot of the dynamics (check) ----
simu_2016 = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/Simulazioni/dynamics_ParmsMedi_MediaAllYears_2016_1.txt'))
simu_2017 = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/Simulazioni/dynamics_ParmsMedi_MediaAllYears_2017_1.txt'))
simu_2018 = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/Simulazioni/dynamics_ParmsMedi_MediaAllYears_2018_1.txt'))

df2016 = data.frame(time = seq(1,180),
                    MS = as.numeric(simu_2016[1,]),
                    ME = as.numeric(simu_2016[2,]),
                    MI = as.numeric(simu_2016[3,]),
                    BS = as.numeric(simu_2016[4,]),
                    BE = as.numeric(simu_2016[5,]),
                    BI = as.numeric(simu_2016[6,]),
                    BR = as.numeric(simu_2016[7,])) %>%
  mutate(Mprev = MI/(MS+ME+MI),
         Bprev = BI/(BS+BE+BI+BR),
         Bseroprev = BR/(BS+BE+BI+BR))


df2017 = data.frame(time = seq(1,180),
                    MS = as.numeric(simu_2017[1,]),
                    ME = as.numeric(simu_2017[2,]),
                    MI = as.numeric(simu_2017[3,]),
                    BS = as.numeric(simu_2017[4,]),
                    BE = as.numeric(simu_2017[5,]),
                    BI = as.numeric(simu_2017[6,]),
                    BR = as.numeric(simu_2017[7,])) %>%
  mutate(Mprev = MI/(MS+ME+MI),
         Bprev = BI/(BS+BE+BI+BR),
         Bseroprev = BR/(BS+BE+BI+BR))

df2018 = data.frame(time = seq(1,180),
                    MS = as.numeric(simu_2018[1,]),
                    ME = as.numeric(simu_2018[2,]),
                    MI = as.numeric(simu_2018[3,]),
                    BS = as.numeric(simu_2018[4,]),
                    BE = as.numeric(simu_2018[5,]),
                    BI = as.numeric(simu_2018[6,]),
                    BR = as.numeric(simu_2018[7,])) %>%
  mutate(Mprev = MI/(MS+ME+MI),
         Bprev = BI/(BS+BE+BI+BR),
         Bseroprev = BR/(BS+BE+BI+BR))

p1 = df2016 %>% ggplot() +
  geom_line(aes(x = time, y = Bseroprev*100), color = "#FF7F00", linewidth = rel(1.2)) +   # Line for mean
  # geom_line(aes(x = time, y = Bprev*100), color = "#FF7F00", linewidth = rel(1.2)) +   # Line for mean
  theme_minimal() +
  labs(x = "",
       y = "Prevalence (%)") +
  ggtitle(2016) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),# rel(1.4)),
        axis.text = element_text(size = rel(1)),#rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(1, 31+30+30+31+30+31),
                     breaks = c(1,
                                30,
                                30+31,
                                31+30+30,
                                31+30+30+31,
                                31+30+30+31+30),
                     labels = c('May', 'Jun',
                                'Jul', 'Aug', 'Sep', 'Oct'))
p1

p2 = df2017 %>% ggplot() +
  geom_line(aes(x = time, y = Bseroprev*100), color = "#FF7F00", linewidth = rel(1.2)) +   # Line for mean
  theme_minimal() +
  labs(x = "",
       y = "") +
  ggtitle(2017) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),# rel(1.4)),
        axis.text = element_text(size = rel(1)),#rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(1, 31+30+30+31+30+31),
                     breaks = c(1,
                                30,
                                30+31,
                                31+30+30,
                                31+30+30+31,
                                31+30+30+31+30),
                     labels = c('May', 'Jun',
                                'Jul', 'Aug', 'Sep', 'Oct'))
p2

p3 = df2018 %>% ggplot() +
  geom_line(aes(x = time, y = Bseroprev*100), color = "#FF7F00", linewidth = rel(1.2)) +   # Line for mean
  # geom_line(aes(x = time, y = Bprev*100), color = "#FF7F00", linewidth = rel(1.2)) +   # Line for mean
  theme_minimal() +
  labs(x = "",
       y = "") +
  ggtitle(2018) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),# rel(1.4)),
        axis.text = element_text(size = rel(1)),#rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(1, 31+30+30+31+30+31),
                     breaks = c(1,
                                30,
                                30+31,
                                31+30+30,
                                31+30+30+31,
                                31+30+30+31+30),
                     labels = c('May', 'Jun',
                                'Jul', 'Aug', 'Sep', 'Oct'))
p3
ggarrange(p1, p2, p3,  common.legend = F, ncol = 3, labels = NULL)
# 1300x400

# plot senstitivity ----
par_muB = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/MCMC/parametri_sens_MeadiaAllYears_muB.txt'))
par_s = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/MCMC/parametri_sens_MeadiaAllYears_s.txt'))
par_phi= data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/MCMC/parametri_sens_MeadiaAllYears_phi.txt'))

muB = par_muB$V5
s = par_s$V6
phi = par_phi$V7

listSimu = list(simu_mu2016 = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/Simulazioni/dynamics_sens_MeadiaAllYears_muB_2016_1.txt')),
             simu_mu2017 = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/Simulazioni/dynamics_sens_MeadiaAllYears_muB_2017_1.txt')),
             simu_mu2018 = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/Simulazioni/dynamics_sens_MeadiaAllYears_muB_2018_1.txt')),

             simu_s2016 = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/Simulazioni/dynamics_sens_MeadiaAllYears_s_2016_1.txt')),
             simu_s2017 = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/Simulazioni/dynamics_sens_MeadiaAllYears_s_2017_1.txt')),
             simu_s2018 = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/Simulazioni/dynamics_sens_MeadiaAllYears_s_2018_1.txt')),

             simu_phi2016 = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/Simulazioni/dynamics_sens_MeadiaAllYears_phi_2016_1.txt')),
             simu_phi2017 = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/Simulazioni/dynamics_sens_MeadiaAllYears_phi_2017_1.txt')),
             simu_phi2018 = data.frame(read.table('V1_Sept/WNModFEMRL/Output_WNV/Simulazioni/dynamics_sens_MeadiaAllYears_phi_2018_1.txt')))

dfplot = data.frame()
for (i in 1:length(listSimu)) {
  if(i %in% c(1,4,7))
    anno = 2016
  if(i %in% c(2,5,8))
    anno = 2017
  if(i %in% c(3,6,9))
    anno = 2018
  if(i %in% c(1,2,3)) {
    par = 'muB'
    parVal = muB
  }
  if(i %in% c(4,5,6)) {
    par = 's'
    parVal = s
  }
  if(i %in% c(7,8,9)) {
    par = 'phi'
    parVal = phi
  }
  print(anno)

  tmp = listSimu[[i]]

  MI = data.frame(tmp[seq(3,nrow(tmp),7),])[seq(1,100,length.out = 10),]
  colnames(MI) = seq(1, 180)

  parVal = parVal[seq(1,100,length.out = 10)]

 MItmp = MI %>%
    mutate(ParVal = parVal,
           Par = par,
           Anno = anno) %>%
    pivot_longer(cols = 1:180,
                 values_to = 'MI',
                 names_to = 'yday')


  prev = data.frame((tmp[seq(3,nrow(tmp),7),])/(tmp[seq(1,nrow(tmp),7),]+tmp[seq(2,nrow(tmp),7),]+tmp[seq(3,nrow(tmp),7),]))[seq(1,100,length.out = 10),]
  colnames(prev) = seq(1, 180)
  prevtmp = prev %>%
    pivot_longer(cols = 1:180,
                 values_to = 'prev',
                 names_to = 'yday')

  dfplot = data.frame(rbind(dfplot, cbind(MItmp, prev = prevtmp$prev)))

 }
dfplot = dfplot %>%
  mutate(Par = as.factor(Par),
         yday = as.numeric(yday),
         Anno = as.factor(Anno)) %>%
  select(yday, MI, prev, Par, ParVal, Anno)

# MI----
# mu
plot_muB2016 =dfplot %>%
  filter(Anno == '2016', Par == 'muB') %>%
  ggplot() +
  geom_line(aes(x = yday, y = MI, col = as.factor(ParVal)), linewidth = 1) +
  scale_color_manual(values = gradient_colors) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = expression("M"[I])         # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(-1,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
  ggtitle('2016')

plot_muB2017 = dfplot %>%
  filter(Anno == '2017', Par == 'muB') %>%
  ggplot() +
  geom_line(aes(x = yday, y = MI, col = as.factor(ParVal)), linewidth = 1) +
  scale_color_manual(values = gradient_colors) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = ''      # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(-1,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
  ggtitle('2017')


plot_muB2018 = dfplot %>%
  filter(Anno == '2018', Par == 'muB') %>%
  ggplot() +
  geom_line(aes(x = yday, y = MI, col = as.factor(ParVal)), linewidth = 1) +
  scale_color_manual(values = gradient_colors) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = ''   # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(-1,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
  ggtitle('2018')


ggarrange(plot_muB2016, plot_muB2017, plot_muB2018,  common.legend = T, ncol = 3, labels = NULL, legend = 'right')
# 1500x400

# s
plot_s2016 = dfplot %>%
  filter(Anno == '2016', Par == 's') %>%
  ggplot() +
  geom_line(aes(x = yday, y = MI, col = as.factor(ParVal)), linewidth = 1) +
  scale_color_manual(values = gradient_colors) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = expression("M"[I])         # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm"))  +
  scale_x_continuous(limits = c(-1,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
  ggtitle('2016')

plot_s2017 = dfplot %>%
  filter(Anno == '2017', Par == 's') %>%
  ggplot() +
  geom_line(aes(x = yday, y = MI, col = as.factor(ParVal)), linewidth = 1) +
  scale_color_manual(values = gradient_colors) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = ''      # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm"))  +
  scale_x_continuous(limits = c(-1,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
  ggtitle('2017')

plot_s2018 = dfplot %>%
  filter(Anno == '2018', Par == 's') %>%
  ggplot() +
  geom_line(aes(x = yday, y = MI, col = as.factor(ParVal)), linewidth = 1) +
  scale_color_manual(values = gradient_colors) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = ''   # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm"))  +
  scale_x_continuous(limits = c(-1,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
  ggtitle('2018')

ggarrange(plot_s2016, plot_s2017, plot_s2018,  common.legend = T, ncol = 3, labels = NULL, legend = 'right')
# 1500x400

# phi
plot_phi2016 = dfplot %>%
  filter(Anno == '2016', Par == 'phi') %>%
  ggplot() +
  geom_line(aes(x = yday, y = MI, col = as.factor(ParVal)), linewidth = 1) +
  scale_color_manual(values = gradient_colors) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = expression("M"[I])         # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm"))  +
  scale_x_continuous(limits = c(-1,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
  ggtitle('2016')

plot_phi2017 = dfplot %>%
  filter(Anno == '2017', Par == 'phi') %>%
  ggplot() +
  geom_line(aes(x = yday, y = MI, col = as.factor(ParVal)), linewidth = 1) +
  scale_color_manual(values = gradient_colors) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = ''      # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(-1,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
  ggtitle('2017')

plot_phi2018 = dfplot %>%
  filter(Anno == '2018', Par == 'phi') %>%
  ggplot() +
  geom_line(aes(x = yday, y = MI, col = as.factor(ParVal)), linewidth = 1) +
  scale_color_manual(values = gradient_colors) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = ''   # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(-1,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
  ggtitle('2018')

ggarrange(plot_phi2016, plot_phi2017, plot_phi2018,  common.legend = T, ncol = 3, labels = NULL, legend = 'right')
# 1500x400

# prevalenze ----
# mu
plot_muB2016 = dfplot %>%
  filter(Anno == '2016', Par == 'muB') %>%
  ggplot() +
  geom_line(aes(x = yday, y = prev*100, col = as.factor(ParVal)), linewidth = 1) +
  scale_color_manual(values = gradient_colors) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = 'Prevalence (%)'        # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm"))

plot_muB2017 = dfplot %>%
  filter(Anno == '2017', Par == 'muB') %>%
  ggplot() +
  geom_line(aes(x = yday, y = prev*100, col = as.factor(ParVal)), linewidth = 1) +
  scale_color_manual(values = gradient_colors) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = ''      # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm"))

plot_muB2018 = dfplot %>%
  filter(Anno == '2018', Par == 'muB') %>%
  ggplot() +
  geom_line(aes(x = yday, y = prev*100, col = as.factor(ParVal)), linewidth = 1) +
  scale_color_manual(values = gradient_colors) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = ''   # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm"))

ggarrange(plot_muB2016, plot_muB2017, plot_muB2018,  common.legend = T, ncol = 3, labels = NULL, legend = 'right')
# 1500x400

# s
plot_s2016 = dfplot %>%
  filter(Anno == '2016', Par == 's') %>%
  ggplot() +
  geom_line(aes(x = yday, y = prev*100, col = as.factor(ParVal)), linewidth = 1) +
  scale_color_manual(values = gradient_colors) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = 'Prevalence (%)'        # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm"))

plot_s2017 = dfplot %>%
  filter(Anno == '2017', Par == 's') %>%
  ggplot() +
  geom_line(aes(x = yday, y = prev*100, col = as.factor(ParVal)), linewidth = 1) +
  scale_color_manual(values = gradient_colors) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = ''      # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm"))

plot_s2018 = dfplot %>%
  filter(Anno == '2018', Par == 's') %>%
  ggplot() +
  geom_line(aes(x = yday, y = prev*100, col = as.factor(ParVal)), linewidth = 1) +
  scale_color_manual(values = gradient_colors) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = ''   # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm"))

ggarrange(plot_s2016, plot_s2017, plot_s2018,  common.legend = T, ncol = 3, labels = NULL, legend = 'right')
# 1500x400

# phi
plot_phi2016 = dfplot %>%
  filter(Anno == '2016', Par == 'phi') %>%
  ggplot() +
  geom_line(aes(x = yday, y = prev*100, col = as.factor(ParVal)), linewidth = 1) +
  scale_color_manual(values = gradient_colors) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = 'Prevalence (%)'        # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm"))

plot_phi2017 = dfplot %>%
  filter(Anno == '2017', Par == 'phi') %>%
  ggplot() +
  geom_line(aes(x = yday, y = prev*100, col = as.factor(ParVal)), linewidth = 1) +
  scale_color_manual(values = gradient_colors) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = ''      # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm"))

plot_phi2018 = dfplot %>%
  filter(Anno == '2018', Par == 'phi') %>%
  ggplot() +
  geom_line(aes(x = yday, y = prev*100, col = as.factor(ParVal)), linewidth = 1) +
  scale_color_manual(values = gradient_colors) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = ''   # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm"))

ggarrange(plot_phi2016, plot_phi2017, plot_phi2018,  common.legend = T, ncol = 3, labels = NULL, legend = 'right')
# 1500x400


# species contribution ----
# cluster ----
#import the dataset containing birds' demographic parameters
uccelli <- read_excel('V1_Sept/Birds_data/birds_demographic_dataset.xlsx', #"/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/Birds/birds_demographic_dataset.xlsx",
                      sheet = "Demo")

#eliminate the first column of the dataset containing bird species names
mydata <- uccelli[,c(-1)]
uccelli_names = c('Turdus merula',
                  'Passer montanus',
                  'Pica pica',
                  'Corvus cornix',
                  'Garrulus glandarius',
                  'Parus major',
                  'Sylvia articapilla',
                  'Streptopelia decaocto',
                  'Columba palumbus',
                  'Anas platyrhynchos',
                  'Vanellus vanellus',
                  'Dendrocopos major',
                  'Ardea cinirea')

df <- scale(mydata)

set.seed(123)
final <- kmeans(df, 3, nstart = 25)
print(final)

fviz_cluster(final, data = df, # geom = 'point',
             shape = 20,
             palette = "Set2", ggtheme = theme_minimal()) +
  # Add custom text annotation
  annotate("text", x = - 3, y = 0.45, label = uccelli_names[1], size = 3.5, color = "#FC8D62") +
  annotate("text", x = - 3.8, y = -1.15, label = uccelli_names[2], size = 3.5, color = "#FC8D62") +
  annotate("text", x = - 2.8, y = -0.65, label = uccelli_names[7], size = 3.5, color = "#FC8D62") +
  annotate("text", x = 0.1, y = 2.6, label = uccelli_names[9], size = 3.5, color = "#FC8D62") +
  annotate("text", x = -3.5, y = 3.55, label = uccelli_names[8], size = 3.5, color = "#FC8D62") +
  annotate("text", x = -0.3, y = 0.35, label = uccelli_names[4], size = 3.5, color = "#66C2A5") +
  annotate("text", x = 1.85, y = 0.2, label = uccelli_names[5], size = 3.5, color = "#66C2A5") +
  annotate("text", x = 2, y = -0.42, label = uccelli_names[3], size = 3.5, color = "#66C2A5") +
  annotate("text", x = -1.15, y = -2.05, label = uccelli_names[12], size = 3.5, color = "#66C2A5") +
  annotate("text", x = -0.8, y = -3.65, label = uccelli_names[6], size = 3.5, color = "#66C2A5") +
  annotate("text", x = 3, y = 2.05, label = uccelli_names[11], size = 3.5, color = "#8DA0CB") +
  annotate("text", x = 5, y = 2.6, label = uccelli_names[13], size = 3.5, color = "#8DA0CB") +
  annotate("text", x = 4.2, y = -3.4, label = uccelli_names[10], size = 3.5, color = "#8DA0CB")

fviz_cluster(final, data = df, geom = 'point',
             shape = 20,
             palette = "Set2", ggtheme = theme_minimal(), main = '') +
  # Add custom text annotation
  annotate("text", x = - 3, y = 0.45, label = uccelli_names[1], size = 3.5, color = "#FC8D62") +
  annotate("text", x = - 3.8, y = -1.15, label = uccelli_names[2], size = 3.5, color = "#FC8D62") +
  annotate("text", x = - 2.8, y = -0.65, label = uccelli_names[7], size = 3.5, color = "#FC8D62") +
  annotate("text", x = 0.1, y = 2.6, label = uccelli_names[9], size = 3.5, color = "#FC8D62") +
  annotate("text", x = -3.5, y = 3.55, label = uccelli_names[8], size = 3.5, color = "#FC8D62") +
  annotate("text", x = -0.3, y = 0.35, label = uccelli_names[4], size = 3.5, color = "#66C2A5") +
  annotate("text", x = 1.85, y = 0.2, label = uccelli_names[5], size = 3.5, color = "#66C2A5") +
  annotate("text", x = 2, y = -0.42, label = uccelli_names[3], size = 3.5, color = "#66C2A5") +
  annotate("text", x = -1.15, y = -2.05, label = uccelli_names[12], size = 3.5, color = "#66C2A5") +
  annotate("text", x = -0.8, y = -3.65, label = uccelli_names[6], size = 3.5, color = "#66C2A5") +
  annotate("text", x = 3, y = 2.05, label = uccelli_names[11], size = 3.5, color = "#8DA0CB") +
  annotate("text", x = 5, y = 2.6, label = uccelli_names[13], size = 3.5, color = "#8DA0CB") +
  annotate("text", x = 4.2, y = -3.4, label = uccelli_names[10], size = 3.5, color = "#8DA0CB")
# set2 cols
# Light Green: '#66C2A5'
#  Light Orange: '#FC8D62'
#  Light Purple: '#8DA0CB'
#  Pinkish Red: '#E78AC3'
#  Yellow: '#A6D854'
#  Beige: '#FFD92F'
#  Gray: '#E5C494'
#  Light Blue: '#B3B3B3'

# Fit modello ----
set.seed(0)
# 2016
anno = 2016

settimane=scan('V1_Sept/WNModFEMRL/per_mcmc/giorni_cattura.txt')-120

simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Simulazioni/dynamics_M_', anno,
                                    '_1.txt')))
MS=simu[seq(1,nrow(simu),7),]
ME=simu[seq(2,nrow(simu),7),]
MI=simu[seq(3,nrow(simu),7),]

mosquito_prevalence=MI/(MS+ME+MI)

numero_pool=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_cluster_',anno))
numero_pool_positivi=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_WNVpos_cluster_',anno))
pool_size=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/mean_pool_size_cluster_',anno))

quando_ho_pool=which(pool_size>0)

pool_positivi_simulati=c()
pool_positivi_real = c()
yday = c()
for(index_giorno_pool in quando_ho_pool){
  #ora devi sottrarre al giorno di cattura un numero in modo tale che le date coincidano.
  # 120=il modello parte da 1 maggio
  giorno_pool=settimane[index_giorno_pool]
  if(giorno_pool < 180){
    prob_pool_positivo=1-(1-mosquito_prevalence[,giorno_pool])^as.numeric(pool_size[1,index_giorno_pool])
    pool_analizzati=numero_pool[1, index_giorno_pool]
    pool_positivi=numero_pool_positivi[1, index_giorno_pool]
    tmp=c()
    tmp_sel=sample(1:length(prob_pool_positivo),size=100)
    for(quale in tmp_sel)
      tmp=c(tmp,rbinom(n=1,size=pool_analizzati,prob=prob_pool_positivo[quale]))
    pool_positivi_simulati=cbind(pool_positivi_simulati,tmp)
    pool_positivi_real = c(pool_positivi_real, pool_positivi)
    yday = c(yday, giorno_pool)
  }
}
pool_pos = data.frame(cbind(t(pool_positivi_simulati)),
                      yday = yday,
                      RealPoolPos = as.numeric(pool_positivi_real))

p1 = ggplot() +
  stat_boxplot(data = pool_pos %>%
                 select(-RealPoolPos) %>%
                 pivot_longer(1:100), aes(x = yday, y = value, group = yday),
               geom = "boxplot",
               width = 3,
               fill = '#8DA0CB',
               outlier.shape = '*') +
  geom_point(data = pool_pos, aes(yday, RealPoolPos), color = 'orange', size = rel(3)) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = 'Positive Pools'        # Custom y-axis label
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.1)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  # ggtitle(anno) +
  scale_x_continuous(limits = c(min(pool_pos_germani$yday)-2, max(pool_pos_germani$yday)+2),
                     breaks = pool_pos_germani$yday,
                     labels = format(as.Date(pool_pos_germani$yday+120, origin = "2016-01-01"),
                                     "%b %d"))
p1
# 2017
anno = 2017

settimane=scan('V1_Sept/WNModFEMRL/per_mcmc/giorni_cattura.txt')-120

simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Simulazioni/dynamics_M_', anno,
                                    '_1.txt')))
MS=simu[seq(1,nrow(simu),7),]
ME=simu[seq(2,nrow(simu),7),]
MI=simu[seq(3,nrow(simu),7),]

mosquito_prevalence=MI/(MS+ME+MI)

numero_pool=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_cluster_',anno))
numero_pool_positivi=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_WNVpos_cluster_',anno))
pool_size=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/mean_pool_size_cluster_',anno))

quando_ho_pool=which(pool_size>0)

pool_positivi_simulati=c()
pool_positivi_real = c()
yday = c()
for(index_giorno_pool in quando_ho_pool){
  #ora devi sottrarre al giorno di cattura un numero in modo tale che le date coincidano.
  # 120=il modello parte da 1 maggio
  giorno_pool=settimane[index_giorno_pool]
  if(giorno_pool < 180){
    prob_pool_positivo=1-(1-mosquito_prevalence[,giorno_pool])^as.numeric(pool_size[1,index_giorno_pool])
    pool_analizzati=numero_pool[1, index_giorno_pool]
    pool_positivi=numero_pool_positivi[1, index_giorno_pool]
    tmp=c()
    tmp_sel=sample(1:length(prob_pool_positivo),size=100)
    for(quale in tmp_sel)
      tmp=c(tmp,rbinom(n=1,size=pool_analizzati,prob=prob_pool_positivo[quale]))
    pool_positivi_simulati=cbind(pool_positivi_simulati,tmp)
    pool_positivi_real = c(pool_positivi_real, pool_positivi)
    yday = c(yday, giorno_pool)
  }
}
pool_pos = data.frame(cbind(t(pool_positivi_simulati)),
                      yday = yday,
                      RealPoolPos = as.numeric(pool_positivi_real))

p2 = ggplot() +
  stat_boxplot(data = pool_pos %>%
                 select(-RealPoolPos) %>%
                 pivot_longer(1:100), aes(x = yday, y = value, group = yday),
               geom = "boxplot",
               width = 3,
               fill = '#8DA0CB',
               outlier.shape = '*') +
  geom_point(data = pool_pos, aes(yday, RealPoolPos), color = 'orange', size = rel(3)) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = 'Positive Pools'        # Custom y-axis label
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.1)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  ggtitle(anno) +
  scale_x_continuous(limits = c(min(pool_pos_germani$yday)-2, max(pool_pos_germani$yday)+2),
                     breaks = pool_pos_germani$yday,
                     labels = format(as.Date(pool_pos_germani$yday+120, origin = "2016-01-01"),
                                     "%b %d"))
# 2018
anno = 2018

settimane=scan('V1_Sept/WNModFEMRL/per_mcmc/giorni_cattura.txt')-120

simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Simulazioni/dynamics_M_', anno,
                                    '_1.txt')))
MS=simu[seq(1,nrow(simu),7),]
ME=simu[seq(2,nrow(simu),7),]
MI=simu[seq(3,nrow(simu),7),]

mosquito_prevalence=MI/(MS+ME+MI)

numero_pool=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_cluster_',anno))
numero_pool_positivi=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_WNVpos_cluster_',anno))
pool_size=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/mean_pool_size_cluster_',anno))

quando_ho_pool=which(pool_size>0)

pool_positivi_simulati=c()
pool_positivi_real = c()
yday = c()
for(index_giorno_pool in quando_ho_pool){
  #ora devi sottrarre al giorno di cattura un numero in modo tale che le date coincidano.
  # 120=il modello parte da 1 maggio
  giorno_pool=settimane[index_giorno_pool]
  if(giorno_pool < 180){
    prob_pool_positivo=1-(1-mosquito_prevalence[,giorno_pool])^as.numeric(pool_size[1,index_giorno_pool])
    pool_analizzati=numero_pool[1, index_giorno_pool]
    pool_positivi=numero_pool_positivi[1, index_giorno_pool]
    tmp=c()
    tmp_sel=sample(1:length(prob_pool_positivo),size=100)
    for(quale in tmp_sel)
      tmp=c(tmp,rbinom(n=1,size=pool_analizzati,prob=prob_pool_positivo[quale]))
    pool_positivi_simulati=cbind(pool_positivi_simulati,tmp)
    pool_positivi_real = c(pool_positivi_real, pool_positivi)
    yday = c(yday, giorno_pool)
  }
}
pool_pos = data.frame(cbind(t(pool_positivi_simulati)),
                      yday = yday,
                      RealPoolPos = as.numeric(pool_positivi_real))

p3 = ggplot() +
  stat_boxplot(data = pool_pos %>%
                 select(-RealPoolPos) %>%
                 pivot_longer(1:100), aes(x = yday, y = value, group = yday),
               geom = "boxplot",
               width = 3,
               fill = '#8DA0CB',
               outlier.shape = '*') +
  geom_point(data = pool_pos, aes(yday, RealPoolPos), color = 'orange', size = rel(3)) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = 'Positive Pools'        # Custom y-axis label
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.1)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  ggtitle(anno) +
  scale_x_continuous(limits = c(min(pool_pos_germani$yday)-2, max(pool_pos_germani$yday)+2),
                     breaks = pool_pos_germani$yday,
                     labels = format(as.Date(pool_pos_germani$yday+120, origin = "2016-01-01"),
                                     "%b %d"))

ggarrange(p1, p2, p3,  common.legend = F, ncol = 3, labels = NULL)
# 1200x400


# fit modello 2 spec -----
# Gazze
set.seed(0)

# 2016
anno = 2016

settimane=scan('V1_Sept/WNModFEMRL/per_mcmc/giorni_cattura.txt')-120

simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Gazze/Simulazioni/dynamics_2spec_Gazze_M_', anno,
                                    '_1.txt')))
MS=simu[seq(1,nrow(simu),11),]
ME=simu[seq(2,nrow(simu),11),]
MI=simu[seq(3,nrow(simu),11),]

mosquito_prevalence=MI/(MS+ME+MI)

numero_pool=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_cluster_',anno))
numero_pool_positivi=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_WNVpos_cluster_',anno))
pool_size=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/mean_pool_size_cluster_',anno))

quando_ho_pool=which(pool_size>0)

pool_positivi_simulati=c()
pool_positivi_real = c()
yday = c()
for(index_giorno_pool in quando_ho_pool){
  #ora devi sottrarre al giorno di cattura un numero in modo tale che le date coincidano.
  # 120=il modello parte da 1 maggio
  giorno_pool=settimane[index_giorno_pool]
  if(giorno_pool < 180){
    prob_pool_positivo=1-(1-mosquito_prevalence[,giorno_pool])^as.numeric(pool_size[1,index_giorno_pool])
    pool_analizzati=numero_pool[1, index_giorno_pool]
    pool_positivi=numero_pool_positivi[1, index_giorno_pool]
    tmp=c()
    tmp_sel=sample(1:length(prob_pool_positivo),size=100)
    for(quale in tmp_sel)
      tmp=c(tmp,rbinom(n=1,size=pool_analizzati,prob=prob_pool_positivo[quale]))
    pool_positivi_simulati=cbind(pool_positivi_simulati,tmp)
    pool_positivi_real = c(pool_positivi_real, pool_positivi)
    yday = c(yday, giorno_pool)
  }
}
pool_pos_gazze = data.frame(cbind(t(pool_positivi_simulati)),
                            yday = yday,
                            RealPoolPos = as.numeric(pool_positivi_real))

simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Merli/Simulazioni/dynamics_2spec_Merli_M_', anno,
                                    '_1.txt')))
MS=simu[seq(1,nrow(simu),11),]
ME=simu[seq(2,nrow(simu),11),]
MI=simu[seq(3,nrow(simu),11),]

mosquito_prevalence=MI/(MS+ME+MI)

numero_pool=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_cluster_',anno))
numero_pool_positivi=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_WNVpos_cluster_',anno))
pool_size=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/mean_pool_size_cluster_',anno))

quando_ho_pool=which(pool_size>0)

pool_positivi_simulati=c()
pool_positivi_real = c()
yday = c()
for(index_giorno_pool in quando_ho_pool){
  #ora devi sottrarre al giorno di cattura un numero in modo tale che le date coincidano.
  # 120=il modello parte da 1 maggio
  giorno_pool=settimane[index_giorno_pool]
  if(giorno_pool < 180){
    prob_pool_positivo=1-(1-mosquito_prevalence[,giorno_pool])^as.numeric(pool_size[1,index_giorno_pool])
    pool_analizzati=numero_pool[1, index_giorno_pool]
    pool_positivi=numero_pool_positivi[1, index_giorno_pool]
    tmp=c()
    tmp_sel=sample(1:length(prob_pool_positivo),size=100)
    for(quale in tmp_sel)
      tmp=c(tmp,rbinom(n=1,size=pool_analizzati,prob=prob_pool_positivo[quale]))
    pool_positivi_simulati=cbind(pool_positivi_simulati,tmp)
    pool_positivi_real = c(pool_positivi_real, pool_positivi)
    yday = c(yday, giorno_pool)
  }
}
pool_pos_merli = data.frame(cbind(t(pool_positivi_simulati)),
                            yday = yday,
                            RealPoolPos = as.numeric(pool_positivi_real))

simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Germani/Simulazioni/dynamics_2spec_Germani_M_', anno,
                                    '_1.txt')))
MS=simu[seq(1,nrow(simu),11),]
ME=simu[seq(2,nrow(simu),11),]
MI=simu[seq(3,nrow(simu),11),]

mosquito_prevalence=MI/(MS+ME+MI)

numero_pool=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_cluster_',anno))
numero_pool_positivi=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_WNVpos_cluster_',anno))
pool_size=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/mean_pool_size_cluster_',anno))

quando_ho_pool=which(pool_size>0)

pool_positivi_simulati=c()
pool_positivi_real = c()
yday = c()
for(index_giorno_pool in quando_ho_pool){
  #ora devi sottrarre al giorno di cattura un numero in modo tale che le date coincidano.
  # 120=il modello parte da 1 maggio
  giorno_pool=settimane[index_giorno_pool]
  if(giorno_pool < 180){
    prob_pool_positivo=1-(1-mosquito_prevalence[,giorno_pool])^as.numeric(pool_size[1,index_giorno_pool])
    pool_analizzati=numero_pool[1, index_giorno_pool]
    pool_positivi=numero_pool_positivi[1, index_giorno_pool]
    tmp=c()
    tmp_sel=sample(1:length(prob_pool_positivo),size=100)
    for(quale in tmp_sel)
      tmp=c(tmp,rbinom(n=1,size=pool_analizzati,prob=prob_pool_positivo[quale]))
    pool_positivi_simulati=cbind(pool_positivi_simulati,tmp)
    pool_positivi_real = c(pool_positivi_real, pool_positivi)
    yday = c(yday, giorno_pool)
  }
}
pool_pos_germani = data.frame(cbind(t(pool_positivi_simulati)),
                            yday = yday,
                            RealPoolPos = as.numeric(pool_positivi_real))



ggplot() +
  stat_boxplot(data = pool_pos_merli %>%
                 select(-RealPoolPos) %>%
                 pivot_longer(1:100), aes(x = yday, y = value, group = yday),
               geom = "boxplot",
               width = 1,
               fill = '#FC8D62',
               outlier.shape = '*',
               position = position_nudge(x = -1.3)) +
  stat_boxplot(data = pool_pos_gazze %>%
                 select(-RealPoolPos) %>%
                 pivot_longer(1:100), aes(x = yday, y = value, group = yday),
               geom = "boxplot",
               width = 1,
               fill = '#66C2A5',
               outlier.shape = '*',
               position = position_nudge(x = 0))  +
  stat_boxplot(data = pool_pos_germani %>%
                 select(-RealPoolPos) %>%
                 pivot_longer(1:100), aes(x = yday, y = value, group = yday),
               geom = "boxplot",
               width = 1,
               fill = '#8DA0CB',
               outlier.shape = '*',
               position = position_nudge(x = 1.3)) +
   geom_point(data = pool_pos_gazze, aes(yday, RealPoolPos), color = 'orange', size = rel(3)) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = 'Positive Pools'        # Custom y-axis label
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  ggtitle(anno) +
scale_x_continuous(limits = c(min(pool_pos_germani$yday)-2, max(pool_pos_germani$yday)+2),
                     breaks = pool_pos_germani$yday,
                   labels = format(as.Date(pool_pos_germani$yday+120, origin = "2016-01-01"),
                                   "%b %d"))
# 1200x400

# 2017
# Gazze
anno = 2017

settimane=scan('V1_Sept/WNModFEMRL/per_mcmc/giorni_cattura.txt')-120

simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Gazze/Simulazioni/dynamics_2spec_Gazze_M_', anno,
                                    '_1.txt')))
MS=simu[seq(1,nrow(simu),11),]
ME=simu[seq(2,nrow(simu),11),]
MI=simu[seq(3,nrow(simu),11),]

mosquito_prevalence=MI/(MS+ME+MI)

numero_pool=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_cluster_',anno))
numero_pool_positivi=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_WNVpos_cluster_',anno))
pool_size=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/mean_pool_size_cluster_',anno))

quando_ho_pool=which(pool_size>0)

pool_positivi_simulati=c()
pool_positivi_real = c()
yday = c()
for(index_giorno_pool in quando_ho_pool){
  #ora devi sottrarre al giorno di cattura un numero in modo tale che le date coincidano.
  # 120=il modello parte da 1 maggio
  giorno_pool=settimane[index_giorno_pool]
  if(giorno_pool < 180){
    prob_pool_positivo=1-(1-mosquito_prevalence[,giorno_pool])^as.numeric(pool_size[1,index_giorno_pool])
    pool_analizzati=numero_pool[1, index_giorno_pool]
    pool_positivi=numero_pool_positivi[1, index_giorno_pool]
    tmp=c()
    tmp_sel=sample(1:length(prob_pool_positivo),size=100)
    for(quale in tmp_sel)
      tmp=c(tmp,rbinom(n=1,size=pool_analizzati,prob=prob_pool_positivo[quale]))
    pool_positivi_simulati=cbind(pool_positivi_simulati,tmp)
    pool_positivi_real = c(pool_positivi_real, pool_positivi)
    yday = c(yday, giorno_pool)
  }
}
pool_pos_gazze = data.frame(cbind(t(pool_positivi_simulati)),
                            yday = yday,
                            RealPoolPos = as.numeric(pool_positivi_real))

simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Merli/Simulazioni/dynamics_2spec_Merli_M_', anno,
                                    '_1.txt')))
MS=simu[seq(1,nrow(simu),11),]
ME=simu[seq(2,nrow(simu),11),]
MI=simu[seq(3,nrow(simu),11),]

mosquito_prevalence=MI/(MS+ME+MI)

numero_pool=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_cluster_',anno))
numero_pool_positivi=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_WNVpos_cluster_',anno))
pool_size=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/mean_pool_size_cluster_',anno))

quando_ho_pool=which(pool_size>0)

pool_positivi_simulati=c()
pool_positivi_real = c()
yday = c()
for(index_giorno_pool in quando_ho_pool){
  #ora devi sottrarre al giorno di cattura un numero in modo tale che le date coincidano.
  # 120=il modello parte da 1 maggio
  giorno_pool=settimane[index_giorno_pool]
  if(giorno_pool < 180){
    prob_pool_positivo=1-(1-mosquito_prevalence[,giorno_pool])^as.numeric(pool_size[1,index_giorno_pool])
    pool_analizzati=numero_pool[1, index_giorno_pool]
    pool_positivi=numero_pool_positivi[1, index_giorno_pool]
    tmp=c()
    tmp_sel=sample(1:length(prob_pool_positivo),size=100)
    for(quale in tmp_sel)
      tmp=c(tmp,rbinom(n=1,size=pool_analizzati,prob=prob_pool_positivo[quale]))
    pool_positivi_simulati=cbind(pool_positivi_simulati,tmp)
    pool_positivi_real = c(pool_positivi_real, pool_positivi)
    yday = c(yday, giorno_pool)
  }
}
pool_pos_merli = data.frame(cbind(t(pool_positivi_simulati)),
                            yday = yday,
                            RealPoolPos = as.numeric(pool_positivi_real))

simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Germani/Simulazioni/dynamics_2spec_Germani_M_', anno,
                                    '_1.txt')))
MS=simu[seq(1,nrow(simu),11),]
ME=simu[seq(2,nrow(simu),11),]
MI=simu[seq(3,nrow(simu),11),]

mosquito_prevalence=MI/(MS+ME+MI)

numero_pool=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_cluster_',anno))
numero_pool_positivi=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_WNVpos_cluster_',anno))
pool_size=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/mean_pool_size_cluster_',anno))

quando_ho_pool=which(pool_size>0)

pool_positivi_simulati=c()
pool_positivi_real = c()
yday = c()
for(index_giorno_pool in quando_ho_pool){
  #ora devi sottrarre al giorno di cattura un numero in modo tale che le date coincidano.
  # 120=il modello parte da 1 maggio
  giorno_pool=settimane[index_giorno_pool]
  if(giorno_pool < 180){
    prob_pool_positivo=1-(1-mosquito_prevalence[,giorno_pool])^as.numeric(pool_size[1,index_giorno_pool])
    pool_analizzati=numero_pool[1, index_giorno_pool]
    pool_positivi=numero_pool_positivi[1, index_giorno_pool]
    tmp=c()
    tmp_sel=sample(1:length(prob_pool_positivo),size=100)
    for(quale in tmp_sel)
      tmp=c(tmp,rbinom(n=1,size=pool_analizzati,prob=prob_pool_positivo[quale]))
    pool_positivi_simulati=cbind(pool_positivi_simulati,tmp)
    pool_positivi_real = c(pool_positivi_real, pool_positivi)
    yday = c(yday, giorno_pool)
  }
}
pool_pos_germani = data.frame(cbind(t(pool_positivi_simulati)),
                              yday = yday,
                              RealPoolPos = as.numeric(pool_positivi_real))



ggplot() +
  stat_boxplot(data = pool_pos_merli %>%
                 select(-RealPoolPos) %>%
                 pivot_longer(1:100), aes(x = yday, y = value, group = yday),
               geom = "boxplot",
               width = 1,
               fill = '#FC8D62',
               outlier.shape = '*',
               position = position_nudge(x = -1.3)) +
  stat_boxplot(data = pool_pos_gazze %>%
                 select(-RealPoolPos) %>%
                 pivot_longer(1:100), aes(x = yday, y = value, group = yday),
               geom = "boxplot",
               width = 1,
               fill = '#66C2A5',
               outlier.shape = '*',
               position = position_nudge(x = 0))  +
  stat_boxplot(data = pool_pos_germani %>%
                 select(-RealPoolPos) %>%
                 pivot_longer(1:100), aes(x = yday, y = value, group = yday),
               geom = "boxplot",
               width = 1,
               fill = '#8DA0CB',
               outlier.shape = '*',
               position = position_nudge(x = 1.3)) +
  geom_point(data = pool_pos_gazze, aes(yday, RealPoolPos), color = 'orange', size = rel(3)) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = 'Positive Pools'        # Custom y-axis label
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  ggtitle(anno) +
  scale_x_continuous(limits = c(min(pool_pos_germani$yday)-2, max(pool_pos_germani$yday)+2),
                     breaks = pool_pos_germani$yday,
                     labels = format(as.Date(pool_pos_germani$yday+120, origin = "2016-01-01"),
                                     "%b %d"))
# 1200x400
# 2018
# Gazze
anno = 2018

settimane=scan('V1_Sept/WNModFEMRL/per_mcmc/giorni_cattura.txt')-120

simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Gazze/Simulazioni/dynamics_2spec_Gazze_M_', anno,
                                    '_1.txt')))
MS=simu[seq(1,nrow(simu),11),]
ME=simu[seq(2,nrow(simu),11),]
MI=simu[seq(3,nrow(simu),11),]

mosquito_prevalence=MI/(MS+ME+MI)

numero_pool=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_cluster_',anno))
numero_pool_positivi=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_WNVpos_cluster_',anno))
pool_size=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/mean_pool_size_cluster_',anno))

quando_ho_pool=which(pool_size>0)

pool_positivi_simulati=c()
pool_positivi_real = c()
yday = c()
for(index_giorno_pool in quando_ho_pool){
  #ora devi sottrarre al giorno di cattura un numero in modo tale che le date coincidano.
  # 120=il modello parte da 1 maggio
  giorno_pool=settimane[index_giorno_pool]
  if(giorno_pool < 180){
    prob_pool_positivo=1-(1-mosquito_prevalence[,giorno_pool])^as.numeric(pool_size[1,index_giorno_pool])
    pool_analizzati=numero_pool[1, index_giorno_pool]
    pool_positivi=numero_pool_positivi[1, index_giorno_pool]
    tmp=c()
    tmp_sel=sample(1:length(prob_pool_positivo),size=100)
    for(quale in tmp_sel)
      tmp=c(tmp,rbinom(n=1,size=pool_analizzati,prob=prob_pool_positivo[quale]))
    pool_positivi_simulati=cbind(pool_positivi_simulati,tmp)
    pool_positivi_real = c(pool_positivi_real, pool_positivi)
    yday = c(yday, giorno_pool)
  }
}
pool_pos_gazze = data.frame(cbind(t(pool_positivi_simulati)),
                            yday = yday,
                            RealPoolPos = as.numeric(pool_positivi_real))

simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Merli/Simulazioni/dynamics_2spec_Merli_M_', anno,
                                    '_1.txt')))
MS=simu[seq(1,nrow(simu),11),]
ME=simu[seq(2,nrow(simu),11),]
MI=simu[seq(3,nrow(simu),11),]

mosquito_prevalence=MI/(MS+ME+MI)

numero_pool=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_cluster_',anno))
numero_pool_positivi=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_WNVpos_cluster_',anno))
pool_size=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/mean_pool_size_cluster_',anno))

quando_ho_pool=which(pool_size>0)

pool_positivi_simulati=c()
pool_positivi_real = c()
yday = c()
for(index_giorno_pool in quando_ho_pool){
  #ora devi sottrarre al giorno di cattura un numero in modo tale che le date coincidano.
  # 120=il modello parte da 1 maggio
  giorno_pool=settimane[index_giorno_pool]
  if(giorno_pool < 180){
    prob_pool_positivo=1-(1-mosquito_prevalence[,giorno_pool])^as.numeric(pool_size[1,index_giorno_pool])
    pool_analizzati=numero_pool[1, index_giorno_pool]
    pool_positivi=numero_pool_positivi[1, index_giorno_pool]
    tmp=c()
    tmp_sel=sample(1:length(prob_pool_positivo),size=100)
    for(quale in tmp_sel)
      tmp=c(tmp,rbinom(n=1,size=pool_analizzati,prob=prob_pool_positivo[quale]))
    pool_positivi_simulati=cbind(pool_positivi_simulati,tmp)
    pool_positivi_real = c(pool_positivi_real, pool_positivi)
    yday = c(yday, giorno_pool)
  }
}
pool_pos_merli = data.frame(cbind(t(pool_positivi_simulati)),
                            yday = yday,
                            RealPoolPos = as.numeric(pool_positivi_real))

simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Germani/Simulazioni/dynamics_2spec_Germani_M_', anno,
                                    '_1.txt')))
MS=simu[seq(1,nrow(simu),11),]
ME=simu[seq(2,nrow(simu),11),]
MI=simu[seq(3,nrow(simu),11),]

mosquito_prevalence=MI/(MS+ME+MI)

numero_pool=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_cluster_',anno))
numero_pool_positivi=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/totale_pool_WNVpos_cluster_',anno))
pool_size=read.table(paste0('V1_Sept/WNModFEMRL/per_mcmc/mean_pool_size_cluster_',anno))

quando_ho_pool=which(pool_size>0)

pool_positivi_simulati=c()
pool_positivi_real = c()
yday = c()
for(index_giorno_pool in quando_ho_pool){
  #ora devi sottrarre al giorno di cattura un numero in modo tale che le date coincidano.
  # 120=il modello parte da 1 maggio
  giorno_pool=settimane[index_giorno_pool]
  if(giorno_pool < 180){
    prob_pool_positivo=1-(1-mosquito_prevalence[,giorno_pool])^as.numeric(pool_size[1,index_giorno_pool])
    pool_analizzati=numero_pool[1, index_giorno_pool]
    pool_positivi=numero_pool_positivi[1, index_giorno_pool]
    tmp=c()
    tmp_sel=sample(1:length(prob_pool_positivo),size=100)
    for(quale in tmp_sel)
      tmp=c(tmp,rbinom(n=1,size=pool_analizzati,prob=prob_pool_positivo[quale]))
    pool_positivi_simulati=cbind(pool_positivi_simulati,tmp)
    pool_positivi_real = c(pool_positivi_real, pool_positivi)
    yday = c(yday, giorno_pool)
  }
}
pool_pos_germani = data.frame(cbind(t(pool_positivi_simulati)),
                              yday = yday,
                              RealPoolPos = as.numeric(pool_positivi_real))



ggplot() +
  stat_boxplot(data = pool_pos_merli %>%
                 select(-RealPoolPos) %>%
                 pivot_longer(1:100), aes(x = yday, y = value, group = yday),
               geom = "boxplot",
               width = 1,
               fill = '#FC8D62',
               outlier.shape = '*',
               position = position_nudge(x = -1.3)) +
  stat_boxplot(data = pool_pos_gazze %>%
                 select(-RealPoolPos) %>%
                 pivot_longer(1:100), aes(x = yday, y = value, group = yday),
               geom = "boxplot",
               width = 1,
               fill = '#66C2A5',
               outlier.shape = '*',
               position = position_nudge(x = 0))  +
  stat_boxplot(data = pool_pos_germani %>%
                 select(-RealPoolPos) %>%
                 pivot_longer(1:100), aes(x = yday, y = value, group = yday),
               geom = "boxplot",
               width = 1,
               fill = '#8DA0CB',
               outlier.shape = '*',
               position = position_nudge(x = 1.3)) +
  geom_point(data = pool_pos_gazze, aes(yday, RealPoolPos), color = 'orange', size = rel(3)) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = 'Positive Pools'        # Custom y-axis label
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  ggtitle(anno) +
  scale_x_continuous(limits = c(min(pool_pos_germani$yday)-2, max(pool_pos_germani$yday)+2),
                     breaks = pool_pos_germani$yday,
                     labels = format(as.Date(pool_pos_germani$yday+120, origin = "2016-01-01"),
                                     "%b %d"))
# 1200x400

# contribution ----
set.seed(0)

b11_merlo = 0.35 # usa biting rate specie nota
b11_germano = 0.01 #usa biting rate specie nota (questo è dei germani perchè è l'ultima specie che hai simu)
b11_gazza = 0.1 #usa biting rate specie nota (questo è dei germani perchè è l'ultima specie che hai simu)
# Merlo ----
anno = 2016
output_mcmc = read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Merli/MCMC/parametri_2spec_Merli_M_', anno,
                                '_1.txt'))
simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Merli/Simulazioni/dynamics_2spec_Merli_M_', anno,
                                    '_1.txt')))
colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca",
                          "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca",
                          "niB_ca", "recB_ca")

sel_MS = seq(1, nrow(simu), 11)

sel_ME = seq(2, nrow(simu), 11)
sel_MI = seq(3, nrow(simu), 11)
sel_BS = seq(4, nrow(simu), 11)
sel_BE = seq(5, nrow(simu), 11)
sel_BI = seq(6, nrow(simu), 11)
sel_BR = seq(7, nrow(simu), 11)
sel_BS2 = seq(8, nrow(simu), 11)
sel_BE2 = seq(9, nrow(simu), 11)
sel_BI2 = seq(10, nrow(simu), 11)
sel_BR2 = seq(11, nrow(simu), 11)
MS = simu[sel_MS, ]
ME = simu[sel_ME, ]
MI = simu[sel_MI, ]
BS = simu[sel_BS, ]
BE = simu[sel_BE, ]
BI = simu[sel_BI, ]
BR = simu[sel_BR, ]
BS2 = simu[sel_BS2, ]
BE2 = simu[sel_BE2, ]
BI2 = simu[sel_BI2, ]
BR2 = simu[sel_BR2, ]

b1 = mean(output_mcmc[, 'b1_ca']) #usa bititng rate calcolato da MCMC (per avian community)

pop_tot = (BS + BE + BI + BR) # apply((BS + BE + BI + BR), MARGIN = 2, mean)
pop_tot2 = (BS2 + BE2 + BI2 + BR2) # apply((BS2 + BE2 + BI2 + BR2), MARGIN = 2, mean)

pr = data.frame(t((b11_merlo*BI/pop_tot)/((b11_merlo*BI/pop_tot)+(b1*BI2/pop_tot2)))) %>%
  mutate(yday = seq(1, 180)) %>%
  rowwise() %>%
  mutate(Mean = mean(c_across(1:100), na.rm = TRUE),
         Qmin = quantile(c_across(1:100), probs = 0.025),
         Qmax = quantile(c_across(1:100), probs = 0.975)) %>%
  select(yday, Mean, Qmin, Qmax)
min(pr$Mean)*100
max(pr$Mean)*100

p2016B = pr %>%
  ggplot() +
  geom_line(aes(yday, Mean*100), color = '#FC8D62', linewidth = rel(1.2)) +
  geom_ribbon(aes(x = yday, ymin = Qmin*100, ymax = Qmax*100), fill = "#FC8D62", alpha = 0.3) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = 'Blackbird contribution (%)'        # Custom y-axis label
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  ggtitle(anno) +
  scale_x_continuous(limits = c(0,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
  ylim(0,100)

anno = 2017
output_mcmc = read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Merli/MCMC/parametri_2spec_Merli_M_', anno,
                                '_1.txt'))
simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Merli/Simulazioni/dynamics_2spec_Merli_M_', anno,
                                    '_1.txt')))
colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca",
                          "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca",
                          "niB_ca", "recB_ca")

sel_MS = seq(1, nrow(simu), 11)
sel_ME = seq(2, nrow(simu), 11)
sel_MI = seq(3, nrow(simu), 11)
sel_BS = seq(4, nrow(simu), 11)
sel_BE = seq(5, nrow(simu), 11)
sel_BI = seq(6, nrow(simu), 11)
sel_BR = seq(7, nrow(simu), 11)
sel_BS2 = seq(8, nrow(simu), 11)
sel_BE2 = seq(9, nrow(simu), 11)
sel_BI2 = seq(10, nrow(simu), 11)
sel_BR2 = seq(11, nrow(simu), 11)
MS = simu[sel_MS, ]
ME = simu[sel_ME, ]
MI = simu[sel_MI, ]
BS = simu[sel_BS, ]
BE = simu[sel_BE, ]
BI = simu[sel_BI, ]
BR = simu[sel_BR, ]
BS2 = simu[sel_BS2, ]
BE2 = simu[sel_BE2, ]
BI2 = simu[sel_BI2, ]
BR2 = simu[sel_BR2, ]

b1 = mean(output_mcmc[, 'b1_ca']) #usa bititng rate calcolato da MCMC (per avian community)

pop_tot = (BS + BE + BI + BR) # apply((BS + BE + BI + BR), MARGIN = 2, mean)
pop_tot2 = (BS2 + BE2 + BI2 + BR2) # apply((BS2 + BE2 + BI2 + BR2), MARGIN = 2, mean)

pr = data.frame(t((b11_merlo*BI/pop_tot)/((b11_merlo*BI/pop_tot)+(b1*BI2/pop_tot2)))) %>%
  mutate(yday = seq(1, 180)) %>%
  rowwise() %>%
  mutate(Mean = mean(c_across(1:100), na.rm = TRUE),
         Qmin = quantile(c_across(1:100), probs = 0.025),
         Qmax = quantile(c_across(1:100), probs = 0.975)) %>%
  select(yday, Mean, Qmin, Qmax)
min(pr$Mean)*100
max(pr$Mean)*100

p2017B = pr %>%
  ggplot() +
  geom_line(aes(yday, Mean*100), color = '#FC8D62', linewidth = rel(1.2)) +
  geom_ribbon(aes(x = yday, ymin = Qmin*100, ymax = Qmax*100), fill = "#FC8D62", alpha = 0.3) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = ''        # Custom y-axis label
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  ggtitle(anno) +
  scale_x_continuous(limits = c(0,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
  ylim(0,100)

anno = 2018
output_mcmc = read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Merli/MCMC/parametri_2spec_Merli_M_', anno,
                                '_1.txt'))
simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Merli/Simulazioni/dynamics_2spec_Merli_M_', anno,
                                    '_1.txt')))
colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca",
                          "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca",
                          "niB_ca", "recB_ca")

sel_MS = seq(1, nrow(simu), 11)
sel_ME = seq(2, nrow(simu), 11)
sel_MI = seq(3, nrow(simu), 11)
sel_BS = seq(4, nrow(simu), 11)
sel_BE = seq(5, nrow(simu), 11)
sel_BI = seq(6, nrow(simu), 11)
sel_BR = seq(7, nrow(simu), 11)
sel_BS2 = seq(8, nrow(simu), 11)
sel_BE2 = seq(9, nrow(simu), 11)
sel_BI2 = seq(10, nrow(simu), 11)
sel_BR2 = seq(11, nrow(simu), 11)
MS = simu[sel_MS, ]
ME = simu[sel_ME, ]
MI = simu[sel_MI, ]
BS = simu[sel_BS, ]
BE = simu[sel_BE, ]
BI = simu[sel_BI, ]
BR = simu[sel_BR, ]
BS2 = simu[sel_BS2, ]
BE2 = simu[sel_BE2, ]
BI2 = simu[sel_BI2, ]
BR2 = simu[sel_BR2, ]

b1 = mean(output_mcmc[, 'b1_ca']) #usa bititng rate calcolato da MCMC (per avian community)

pop_tot = (BS + BE + BI + BR) # apply((BS + BE + BI + BR), MARGIN = 2, mean)
pop_tot2 = (BS2 + BE2 + BI2 + BR2) # apply((BS2 + BE2 + BI2 + BR2), MARGIN = 2, mean)

pr = data.frame(t((b11_merlo*BI/pop_tot)/((b11_merlo*BI/pop_tot)+(b1*BI2/pop_tot2)))) %>%
  mutate(yday = seq(1, 180)) %>%
  rowwise() %>%
  mutate(Mean = mean(c_across(1:100), na.rm = TRUE),
         Qmin = quantile(c_across(1:100), probs = 0.025),
         Qmax = quantile(c_across(1:100), probs = 0.975)) %>%
  select(yday, Mean, Qmin, Qmax)
min(pr$Mean)*100
max(pr$Mean)*100

p2018B = pr %>%
  ggplot() +
  geom_line(aes(yday, Mean*100), color = '#FC8D62', linewidth = rel(1.2)) +
  geom_ribbon(aes(x = yday, ymin = Qmin*100, ymax = Qmax*100), fill = "#FC8D62", alpha = 0.3) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = ''        # Custom y-axis label
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  ggtitle(anno) +
  scale_x_continuous(limits = c(0,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
  ylim(0,100)

ggarrange(p2016B, p2017B, p2018B,  common.legend = F, ncol = 3, labels = NULL)
# 1300x400
# Gazza ----
anno = 2016
output_mcmc = read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Gazze/MCMC/parametri_2spec_Gazze_M_', anno,
                                '_1.txt'))
simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Gazze/Simulazioni/dynamics_2spec_Gazze_M_', anno,
                                    '_1.txt')))
colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca",
                          "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca",
                          "niB_ca", "recB_ca")

sel_MS = seq(1, nrow(simu), 11)
sel_ME = seq(2, nrow(simu), 11)
sel_MI = seq(3, nrow(simu), 11)
sel_BS = seq(4, nrow(simu), 11)
sel_BE = seq(5, nrow(simu), 11)
sel_BI = seq(6, nrow(simu), 11)
sel_BR = seq(7, nrow(simu), 11)
sel_BS2 = seq(8, nrow(simu), 11)
sel_BE2 = seq(9, nrow(simu), 11)
sel_BI2 = seq(10, nrow(simu), 11)
sel_BR2 = seq(11, nrow(simu), 11)
MS = simu[sel_MS, ]
ME = simu[sel_ME, ]
MI = simu[sel_MI, ]
BS = simu[sel_BS, ]
BE = simu[sel_BE, ]
BI = simu[sel_BI, ]
BR = simu[sel_BR, ]
BS2 = simu[sel_BS2, ]
BE2 = simu[sel_BE2, ]
BI2 = simu[sel_BI2, ]
BR2 = simu[sel_BR2, ]

b1 = mean(output_mcmc[, 'b1_ca']) #usa bititng rate calcolato da MCMC (per avian community)

pop_tot = (BS + BE + BI + BR) # apply((BS + BE + BI + BR), MARGIN = 2, mean)
pop_tot2 = (BS2 + BE2 + BI2 + BR2) # apply((BS2 + BE2 + BI2 + BR2), MARGIN = 2, mean)

pr = data.frame(t((b11_gazza*BI/pop_tot)/((b11_gazza*BI/pop_tot)+(b1*BI2/pop_tot2)))) %>%
  mutate(yday = seq(1, 180)) %>%
  rowwise() %>%
  mutate(Mean = mean(c_across(1:100), na.rm = TRUE),
         Qmin = quantile(c_across(1:100), probs = 0.025),
         Qmax = quantile(c_across(1:100), probs = 0.975)) %>%
  select(yday, Mean, Qmin, Qmax)
min(pr$Mean)*100
max(pr$Mean)*100

p2016M = pr %>%
  ggplot() +
  geom_line(aes(yday, Mean*100), color = '#66C2A5', linewidth = rel(1.2)) +
  geom_ribbon(aes(x = yday, ymin = Qmin*100, ymax = Qmax*100), fill = "#66C2A5", alpha = 0.3) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = 'Magpie contribution (%)'        # Custom y-axis label
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  # ggtitle(anno) +
  scale_x_continuous(limits = c(0,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
  ylim(0,100)

anno = 2017
output_mcmc = read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Gazze/MCMC/parametri_2spec_Gazze_M_', anno,
                                '_1.txt'))
simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Gazze/Simulazioni/dynamics_2spec_Gazze_M_', anno,
                                    '_1.txt')))
colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca",
                          "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca",
                          "niB_ca", "recB_ca")

sel_MS = seq(1, nrow(simu), 11)
sel_ME = seq(2, nrow(simu), 11)
sel_MI = seq(3, nrow(simu), 11)
sel_BS = seq(4, nrow(simu), 11)
sel_BE = seq(5, nrow(simu), 11)
sel_BI = seq(6, nrow(simu), 11)
sel_BR = seq(7, nrow(simu), 11)
sel_BS2 = seq(8, nrow(simu), 11)
sel_BE2 = seq(9, nrow(simu), 11)
sel_BI2 = seq(10, nrow(simu), 11)
sel_BR2 = seq(11, nrow(simu), 11)
MS = simu[sel_MS, ]
ME = simu[sel_ME, ]
MI = simu[sel_MI, ]
BS = simu[sel_BS, ]
BE = simu[sel_BE, ]
BI = simu[sel_BI, ]
BR = simu[sel_BR, ]
BS2 = simu[sel_BS2, ]
BE2 = simu[sel_BE2, ]
BI2 = simu[sel_BI2, ]
BR2 = simu[sel_BR2, ]

b1 = mean(output_mcmc[, 'b1_ca']) #usa bititng rate calcolato da MCMC (per avian community)

pop_tot = (BS + BE + BI + BR) # apply((BS + BE + BI + BR), MARGIN = 2, mean)
pop_tot2 = (BS2 + BE2 + BI2 + BR2) # apply((BS2 + BE2 + BI2 + BR2), MARGIN = 2, mean)

pr = data.frame(t((b11_gazza*BI/pop_tot)/((b11_gazza*BI/pop_tot)+(b1*BI2/pop_tot2)))) %>%
  mutate(yday = seq(1, 180)) %>%
  rowwise() %>%
  mutate(Mean = mean(c_across(1:100), na.rm = TRUE),
         Qmin = quantile(c_across(1:100), probs = 0.025),
         Qmax = quantile(c_across(1:100), probs = 0.975)) %>%
  select(yday, Mean, Qmin, Qmax)
min(pr$Mean)*100
max(pr$Mean)*100

p2017M = pr %>%
  ggplot() +
  geom_line(aes(yday, Mean*100), color = '#66C2A5', linewidth = rel(1.2)) +
  geom_ribbon(aes(x = yday, ymin = Qmin*100, ymax = Qmax*100), fill = "#66C2A5", alpha = 0.3) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = ''        # Custom y-axis label
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  # ggtitle(anno) +
  scale_x_continuous(limits = c(0,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
  ylim(0,100)


anno = 2018
output_mcmc = read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Gazze/MCMC/parametri_2spec_Gazze_M_', anno,
                                '_1.txt'))
simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Gazze/Simulazioni/dynamics_2spec_Gazze_M_', anno,
                                    '_1.txt')))
colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca",
                          "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca",
                          "niB_ca", "recB_ca")

sel_MS = seq(1, nrow(simu), 11)
sel_ME = seq(2, nrow(simu), 11)
sel_MI = seq(3, nrow(simu), 11)
sel_BS = seq(4, nrow(simu), 11)
sel_BE = seq(5, nrow(simu), 11)
sel_BI = seq(6, nrow(simu), 11)
sel_BR = seq(7, nrow(simu), 11)
sel_BS2 = seq(8, nrow(simu), 11)
sel_BE2 = seq(9, nrow(simu), 11)
sel_BI2 = seq(10, nrow(simu), 11)
sel_BR2 = seq(11, nrow(simu), 11)
MS = simu[sel_MS, ]
ME = simu[sel_ME, ]
MI = simu[sel_MI, ]
BS = simu[sel_BS, ]
BE = simu[sel_BE, ]
BI = simu[sel_BI, ]
BR = simu[sel_BR, ]
BS2 = simu[sel_BS2, ]
BE2 = simu[sel_BE2, ]
BI2 = simu[sel_BI2, ]
BR2 = simu[sel_BR2, ]

b1 = mean(output_mcmc[, 'b1_ca']) #usa bititng rate calcolato da MCMC (per avian community)

pop_tot = (BS + BE + BI + BR) # apply((BS + BE + BI + BR), MARGIN = 2, mean)
pop_tot2 = (BS2 + BE2 + BI2 + BR2) # apply((BS2 + BE2 + BI2 + BR2), MARGIN = 2, mean)

pr = data.frame(t((b11_gazza*BI/pop_tot)/((b11_gazza*BI/pop_tot)+(b1*BI2/pop_tot2)))) %>%
  mutate(yday = seq(1, 180)) %>%
  rowwise() %>%
  mutate(Mean = mean(c_across(1:100), na.rm = TRUE),
         Qmin = quantile(c_across(1:100), probs = 0.025),
         Qmax = quantile(c_across(1:100), probs = 0.975)) %>%
  select(yday, Mean, Qmin, Qmax)
min(pr$Mean)*100
max(pr$Mean)*100

p2018M = pr %>%
  ggplot() +
  geom_line(aes(yday, Mean*100), color = '#66C2A5', linewidth = rel(1.2)) +
  geom_ribbon(aes(x = yday, ymin = Qmin*100, ymax = Qmax*100), fill = "#66C2A5", alpha = 0.3) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = ''        # Custom y-axis label
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  # ggtitle(anno) +
  scale_x_continuous(limits = c(0,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +
  ylim(0,100)

ggarrange(p2016M, p2017M, p2018M,  common.legend = F, ncol = 3, labels = NULL)
# 1300x400

# Germani ----
anno = 2016
output_mcmc = read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Germani/MCMC/parametri_2spec_Germani_M_', anno,
                                '_1.txt'))
simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Germani/Simulazioni/dynamics_2spec_Germani_M_', anno,
                                    '_1.txt')))
colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca",
                          "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca",
                          "niB_ca", "recB_ca")

sel_MS = seq(1, nrow(simu), 11)
sel_ME = seq(2, nrow(simu), 11)
sel_MI = seq(3, nrow(simu), 11)
sel_BS = seq(4, nrow(simu), 11)
sel_BE = seq(5, nrow(simu), 11)
sel_BI = seq(6, nrow(simu), 11)
sel_BR = seq(7, nrow(simu), 11)
sel_BS2 = seq(8, nrow(simu), 11)
sel_BE2 = seq(9, nrow(simu), 11)
sel_BI2 = seq(10, nrow(simu), 11)
sel_BR2 = seq(11, nrow(simu), 11)
MS = simu[sel_MS, ]
ME = simu[sel_ME, ]
MI = simu[sel_MI, ]
BS = simu[sel_BS, ]
BE = simu[sel_BE, ]
BI = simu[sel_BI, ]
BR = simu[sel_BR, ]
BS2 = simu[sel_BS2, ]
BE2 = simu[sel_BE2, ]
BI2 = simu[sel_BI2, ]
BR2 = simu[sel_BR2, ]

b1 = mean(output_mcmc[, 'b1_ca']) #usa bititng rate calcolato da MCMC (per avian community)

pop_tot = (BS + BE + BI + BR) # apply((BS + BE + BI + BR), MARGIN = 2, mean)
pop_tot2 = (BS2 + BE2 + BI2 + BR2) # apply((BS2 + BE2 + BI2 + BR2), MARGIN = 2, mean)

pr = data.frame(t((b11_germano*BI/pop_tot)/((b11_germano*BI/pop_tot)+(b1*BI2/pop_tot2)))) %>%
  mutate(yday = seq(1, 180)) %>%
  rowwise() %>%
  mutate(Mean = mean(c_across(1:100), na.rm = TRUE),
         Qmin = quantile(c_across(1:100), probs = 0.025),
         Qmax = quantile(c_across(1:100), probs = 0.975)) %>%
  select(yday, Mean, Qmin, Qmax)
min(pr$Mean)*100
max(pr$Mean)*100

p2016G = pr %>%
  ggplot() +
  geom_line(aes(yday, Mean*100), color = '#8DA0CB', linewidth = rel(1.2)) +
  geom_ribbon(aes(x = yday, ymin = Qmin*100, ymax = Qmax*100), fill = "#8DA0CB", alpha = 0.3) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = 'Mallard contribution (%)'        # Custom y-axis label
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  #geom_segment(aes(x = -1, xend = 1, y = 0.975, yend = 1.025), size = 0.5, color = "black") +  # Custom tick 1
  #geom_segment(aes(x = -1, xend = 1, y = 99.45, yend = 99.5), size = 0.5, color = "black") +  # Custom tick 1

  # ggtitle(anno) +
  scale_x_continuous(limits = c(-1,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +

  ylim(0,100) +
  scale_y_break(breaks = c(1.025, 99.45),
                scale = 0.5, # 'free' 'fixed',
                ticklabels = c(0, 1, 99.5, 100),
                space = 0.1)

p2016G

anno = 2017
output_mcmc = read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Germani/MCMC/parametri_2spec_Germani_M_', anno,
                                '_1.txt'))
simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Germani/Simulazioni/dynamics_2spec_Germani_M_', anno,
                                    '_1.txt')))
colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca",
                          "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca",
                          "niB_ca", "recB_ca")

sel_MS = seq(1, nrow(simu), 11)
sel_ME = seq(2, nrow(simu), 11)
sel_MI = seq(3, nrow(simu), 11)
sel_BS = seq(4, nrow(simu), 11)
sel_BE = seq(5, nrow(simu), 11)
sel_BI = seq(6, nrow(simu), 11)
sel_BR = seq(7, nrow(simu), 11)
sel_BS2 = seq(8, nrow(simu), 11)
sel_BE2 = seq(9, nrow(simu), 11)
sel_BI2 = seq(10, nrow(simu), 11)
sel_BR2 = seq(11, nrow(simu), 11)
MS = simu[sel_MS, ]
ME = simu[sel_ME, ]
MI = simu[sel_MI, ]
BS = simu[sel_BS, ]
BE = simu[sel_BE, ]
BI = simu[sel_BI, ]
BR = simu[sel_BR, ]
BS2 = simu[sel_BS2, ]
BE2 = simu[sel_BE2, ]
BI2 = simu[sel_BI2, ]
BR2 = simu[sel_BR2, ]

b1 = mean(output_mcmc[, 'b1_ca']) #usa bititng rate calcolato da MCMC (per avian community)

pop_tot = (BS + BE + BI + BR) # apply((BS + BE + BI + BR), MARGIN = 2, mean)
pop_tot2 = (BS2 + BE2 + BI2 + BR2) # apply((BS2 + BE2 + BI2 + BR2), MARGIN = 2, mean)

pr = data.frame(t((b11_germano*BI/pop_tot)/((b11_germano*BI/pop_tot)+(b1*BI2/pop_tot2)))) %>%
  mutate(yday = seq(1, 180)) %>%
  rowwise() %>%
  mutate(Mean = mean(c_across(1:100), na.rm = TRUE),
         Qmin = quantile(c_across(1:100), probs = 0.025),
         Qmax = quantile(c_across(1:100), probs = 0.975)) %>%
  select(yday, Mean, Qmin, Qmax)
min(pr$Mean)*100
max(pr$Mean)*100

p2017G = pr %>%
  ggplot() +
  geom_line(aes(yday, Mean*100), color = '#8DA0CB', linewidth = rel(1.2)) +
  geom_ribbon(aes(x = yday, ymin = Qmin*100, ymax = Qmax*100), fill = "#8DA0CB", alpha = 0.3) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = ''        # Custom y-axis label
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  #geom_segment(aes(x = -1, xend = 1, y = 0.975, yend = 1.025), size = 0.5, color = "black") +  # Custom tick 1
  #geom_segment(aes(x = -1, xend = 1, y = 99.45, yend = 99.5), size = 0.5, color = "black") +  # Custom tick 1

  # ggtitle(anno) +
  scale_x_continuous(limits = c(-1,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +

  ylim(0,100) +
  scale_y_break(breaks = c(1.025, 99.45),
                scale = 0.5, # 'free' 'fixed',
                ticklabels = c(0, 1, 99.5, 100),
                space = 0.1)

p2017G

anno = 2018
output_mcmc = read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Germani/MCMC/parametri_2spec_Germani_M_', anno,
                                '_1.txt'))
simu = data.frame(read.table(paste0('V1_Sept/WNModFEMRL/Output_WNV/Germani/Simulazioni/dynamics_2spec_Germani_M_', anno,
                                    '_1.txt')))
colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca",
                          "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca",
                          "niB_ca", "recB_ca")

sel_MS = seq(1, nrow(simu), 11)
sel_ME = seq(2, nrow(simu), 11)
sel_MI = seq(3, nrow(simu), 11)
sel_BS = seq(4, nrow(simu), 11)
sel_BE = seq(5, nrow(simu), 11)
sel_BI = seq(6, nrow(simu), 11)
sel_BR = seq(7, nrow(simu), 11)
sel_BS2 = seq(8, nrow(simu), 11)
sel_BE2 = seq(9, nrow(simu), 11)
sel_BI2 = seq(10, nrow(simu), 11)
sel_BR2 = seq(11, nrow(simu), 11)
MS = simu[sel_MS, ]
ME = simu[sel_ME, ]
MI = simu[sel_MI, ]
BS = simu[sel_BS, ]
BE = simu[sel_BE, ]
BI = simu[sel_BI, ]
BR = simu[sel_BR, ]
BS2 = simu[sel_BS2, ]
BE2 = simu[sel_BE2, ]
BI2 = simu[sel_BI2, ]
BR2 = simu[sel_BR2, ]

b1 = mean(output_mcmc[, 'b1_ca']) #usa bititng rate calcolato da MCMC (per avian community)

pop_tot = (BS + BE + BI + BR) # apply((BS + BE + BI + BR), MARGIN = 2, mean)
pop_tot2 = (BS2 + BE2 + BI2 + BR2) # apply((BS2 + BE2 + BI2 + BR2), MARGIN = 2, mean)

pr = data.frame(t((b11_germano*BI/pop_tot)/((b11_germano*BI/pop_tot)+(b1*BI2/pop_tot2)))) %>%
  mutate(yday = seq(1, 180)) %>%
  rowwise() %>%
  mutate(Mean = mean(c_across(1:100), na.rm = TRUE),
         Qmin = quantile(c_across(1:100), probs = 0.025),
         Qmax = quantile(c_across(1:100), probs = 0.975)) %>%
  select(yday, Mean, Qmin, Qmax)
min(pr$Mean)*100
max(pr$Mean)*100

p2018G = pr %>%
  ggplot() +
  geom_line(aes(yday, Mean*100), color = '#8DA0CB', linewidth = rel(1.2)) +
  geom_ribbon(aes(x = yday, ymin = Qmin*100, ymax = Qmax*100), fill = "#8DA0CB", alpha = 0.3) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = ''        # Custom y-axis label
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  #geom_segment(aes(x = -1, xend = 1, y = 1.95, yend = 2.2), size = 0.5, color = "black") +  # Custom tick 1
  #geom_segment(aes(x = -1, xend = 1, y = 99.45, yend = 99.5), size = 0.5, color = "black") +  # Custom tick 1

  # ggtitle(anno) +
  scale_x_continuous(limits = c(-1,180),
                     breaks = seq(0,180, length.out = 10),
                     labels = format(as.Date(seq(0,180, length.out = 10)+120, origin = "2016-01-01"),
                                     "%b %d")) +

  ylim(0,100) +
  scale_y_break(breaks = c(2.05, 99.45),
                scale = 0.5, # 'free' 'fixed',
                ticklabels = c(0, 2, 99.5, 100),
                space = 0.1)
p2018G

ggarrange(p2016G, p2017G, p2018G,  common.legend = F, ncol = 3, labels = NULL)

ggarrange(p2016B, p2017B, p2018B,
          p2016M, p2017M, p2018M,
          p2016G, p2017G, p2018G,common.legend = F, ncol = 3, nrow = 3, labels = NULL)

# 1300x400
#########


ggarrange(p1, p2, p3,  common.legend = F, ncol = 3, labels = NULL)
# 1300x400



ggarrange(p1, p2, p3,  common.legend = F, ncol = 3, labels = NULL)
# 1300x400











#############
#########
prev_df2 %>%
  filter(Anno == '2017', Par == 'muB') %>%
  ggplot() +
  geom_line(aes(x = yday, y = value, col = name
  ), linewidth = 1) +
  #scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = expression("n"[t])         # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm"))

# scelgo solo alcuni parametri...tipo nove come i colori di set1?
color_blind_palette <- c(
  "#117733", "#44AA99", "#88CCEE", "#DDCC77", "#CC6677",
  "#AA4499", "#882255", "#332288", "#999933", "#E69F00"
)

MI_df = c()
prev_df = c()
for (i in 1:length(listSimu)) {
  if(i %in% c(1,4,7))
    anno = 2016
  if(i %in% c(2,5,8))
    anno = 2017
  if(i %in% c(3,6,9))
    anno = 2018
  if(i %in% c(1,2,3))
    par = 'muB'
  if(i %in% c(4,5,6))
    par = 's'
  if(i %in% c(7,8,9))
    par = 'phi'
  print(anno)

  tmp = listSimu[[i]]

  MI = t(tmp[seq(3,nrow(tmp),7),])[,seq(1,100,length.out = 10)]
  MI_df = rbind(MI_df, cbind('yday' = seq(1, 180),
                             MI,
                             'Anno' = anno,
                             'Par' = par))

  prev = t((tmp[seq(3,nrow(tmp),7),])/(tmp[seq(1,nrow(tmp),7),]+tmp[seq(2,nrow(tmp),7),]+tmp[seq(3,nrow(tmp),7),]))[,seq(1,100,length.out = 10)]
  prev_df = rbind(prev_df, cbind('yday' = seq(1, 180),
                                 prev,
                                 'Anno' = anno,
                                 'Par' = par))
}

MI_df = data.frame(MI_df)
prev_df = data.frame(prev_df)

colnames(MI_df) = c('yday', paste0('P', seq(1,10)),
                    # muB,
                    'Anno',
                    'Par')
colnames(prev_df) = c('yday', paste0('P', seq(1,10)),
                      # muB,
                      'Anno',
                      'Par')

MI_df2 = MI_df %>%
  pivot_longer(cols = 2:11) %>%
  mutate(yday = as.numeric(yday),
         Anno = as.factor(Anno),
         Par = as.factor(Par),
         name = as.factor(name),
         value = as.numeric(value))

str(MI_df2)
prev_df2 = prev_df %>%
  pivot_longer(cols = 2:11) %>%
  mutate(yday = as.numeric(yday),
         Anno = as.factor(Anno),
         Par = as.factor(Par),
         name = as.factor(name),
         value = as.numeric(value))

str(prev_df2)

MI_df2 %>%
  filter(Anno == '2017', Par == 'muB') %>%
  ggplot() +
  geom_line(aes(x = yday, y = value, col = Par
  ), linewidth = 1) +
  # scale_color_manual(palette = color_blind_palette) +
  scale_color_gradient() +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = expression("n"[t])         # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm"))

prev_df2 %>%
  filter(Anno == '2017', Par == 'muB') %>%
  ggplot() +
  geom_line(aes(x = yday, y = value, col = name
  ), linewidth = 1) +
  scale_color_brewer(palette = "color_blind_palette") +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = expression("n"[t])         # Custom y-axis label
  ) +
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm"))



 +
  ggtitle(expression(paste("A)           ", mu[B])))
p1

Sens = read.table('V1_Sept/WNModFEMRL/Output_WNV/MCMC/parametri_sens_MeadiaAllYears_s.txt')
s = Sens$V6
min(s)
max(s)

par = s
tmp = data.frame(sapply(par, FUN = function(a) {
  muBBase * exp(-a * sin(pi * (seq(1,365)/365 - phiBase))^2)/besselI(a/2, 0, T)
}))
colnames(tmp) = paste0('p', seq(1,100))
tmp = tmp %>%
  mutate(yday = seq(1, 365)) %>%
  pivot_longer(1:100)

p2 = ggplot() +
  geom_line(data = tmp, aes(x = yday, y = value, col = name), linewidth = 1, alpha = 0.5) +
  geom_line(data = ntdf, aes(x = yday, y = nt), linewidth = 1.2) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = "", #expression("n"[t])         # Custom y-axis label
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(0, 365),
                     breaks = c(0,
                                28,
                                28+31,
                                31+28+30,
                                31+28+30+31,
                                31+28+30+31+30,
                                31+28+30+31+30+31,
                                31+28+30+31+30+31+31,
                                28+31+30+31+30+31+31+30,
                                28+31+30+31+30+31+31+30+31,
                                28+31+30+31+30+31+31+30+31+30,
                                28+31+30+31+30+31+31+30+31+30+31),
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
  ggtitle(expression(paste("B)           ", s)))


Sens = read.table('V1_Sept/WNModFEMRL/Output_WNV/MCMC/parametri_sens_MeadiaAllYears_phi.txt')
phi = Sens$V7
phi =  phi  %% 1
min(phi)
max(phi)

par = phi
tmp = data.frame(sapply(par, FUN = function(a) {
  muBBase * exp(-sBase * sin(pi * (seq(1,365)/365 - a))^2)/besselI(sBase/2, 0, T)
}))
colnames(tmp) = paste0('p', seq(1,100))
tmp = tmp %>%
  mutate(yday = seq(1, 365)) %>%
  pivot_longer(1:100)

p3 = ggplot() +
  geom_line(data = tmp, aes(x = yday, y = value, col = name), linewidth = 1, alpha = 0.5) +
  geom_line(data = ntdf, aes(x = yday, y = nt), linewidth = 1.2) +
  theme_minimal() +
  labs(
    x = "",          # Custom x-axis label
    y = "", #expression("n"[t])         # Custom y-axis label
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme(plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.1, 0.1), "cm")) +
  scale_x_continuous(limits = c(0, 365),
                     breaks = c(0,
                                28,
                                28+31,
                                31+28+30,
                                31+28+30+31,
                                31+28+30+31+30,
                                31+28+30+31+30+31,
                                31+28+30+31+30+31+31,
                                28+31+30+31+30+31+31+30,
                                28+31+30+31+30+31+31+30+31,
                                28+31+30+31+30+31+31+30+31+30,
                                28+31+30+31+30+31+31+30+31+30+31),
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
  ggtitle(expression(paste("C)           ", phi)))



