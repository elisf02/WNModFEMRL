####PARAMETRI GRAFICI PER PLOT PAPER

library(dplyr)
library(tidyr)
library(ggplot2)
library(WNModFEMRL)
library(RColorBrewer)
#display.brewer.all()
#display.brewer.pal(11,"Spectral") # Spectral sono 11 colori

cex_points = 1.2
cex_axes = 1.6
cex_lab_axes = 1.6
margins_plot = c(5, 10, 2, 2) #in alternativa c(5,5,2,2) o c(7, 7, 4, 2)
cex_main = 1.8


par(mfrow = c(1, numero_cluster), mar = c(5, 10, 2, 2), 
    cex.lab = 1.6, cex.main = 1.8, cex.axis = 1.6, bty ="l")

##colori
brewer.pal(11,"Spectral")
"#9E0142" "#D53E4F" "#F46D43" "#FDAE61" "#FEE08B" "#FFFFBF" "#E6F598"
"#ABDDA4" "#66C2A5" "#3288BD" "#5E4FA2"


#divisione per anni
2016 = "#9E0142"
2017 = "#F46D43"
2018 = "#66C2A5"

#divisione per specie

merli = "#9E0142"
AC_merli = "#D53E4F" 

gazze = "#F46D43"
AC_gazze = "#FDAE61" 

germani = "#66C2A5" 
AC_germani = "#ABDDA4"

c("#D53E4F" ,"#FDAE61" ,"#ABDDA4")
c("#9E0142","#F46D43","#66C2A5" )

#per fit modelli
box = "#5E4FA2"
osservazioni = "#FDAE61"

#dinamiche
bird_populatio = "#9E0142"
bird_prevalence = "#D53E4F"
BS = "#F46D43" 
BE ="#FDAE61" 
BI ="#FEE08B"
BR = "#E6F598"

mosquito_prevalence ="#ABDDA4" 

MS ="#66C2A5" 
ME ="#3288BD" 
MI ="#5E4FA2"

