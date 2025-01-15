##set working directory del lavoro (cartella dove ho messo i file da richiamare e dove ho inserito cartelle output)
setwd('/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/WNModFEMRL/')

##richiamo il pacchetto
library(WNModFEMRL)
vignette("WNModFEMRL")

#da cambiare il nome del file a seconda dell'anno che si vuole plottare
data <- read.table('/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/WNModFEMRL/Output_WNV/MCMC/parametri_M_2018_1.txt')
data = data[,-10]

colnames(data) = c("p", "B0", "pR", "b1", "muB", 
                   "s", "phi", "niB", "recB")
plot(data[,'p'], data[,'B0'])
plot(data[,'p'], data[,'pR'])
plot(data[,'p'], data[,'b1'])
plot(data[,'p'], data[,'muB'], col = 'red')
plot(data[,'p'], data[,'s'], col = 'red')
plot(data[,'p'], data[,'phi'], col = 'red')
plot(data[,'p'], data[,'niB'])
plot(data[,'p'], data[,'recB'])

plot(data[,'B0'], data[,'pR'])
plot(data[,'B0'], data[,'b1'])
plot(data[,'B0'], data[,'muB'], col = 'red')
plot(data[,'B0'], data[,'s'], col = 'red')
plot(data[,'B0'], data[,'phi'], col = 'red')
plot(data[,'B0'], data[,'niB'])
plot(data[,'B0'], data[,'recB'])

plot(data[,'pR'], data[,'b1'])
plot(data[,'pR'], data[,'muB'], col = 'red')
plot(data[,'pR'], data[,'s'], col = 'red')
plot(data[,'pR'], data[,'phi'], col = 'red')
plot(data[,'pR'], data[,'niB'])
plot(data[,'pR'], data[,'recB'])

plot(data[,'b1'], data[,'muB'], col = 'red')
plot(data[,'b1'], data[,'s'], col = 'red')
plot(data[,'b1'], data[,'phi'], col = 'red')
plot(data[,'b1'], data[,'niB'])
plot(data[,'b1'], data[,'recB'])

plot(data[,'muB'], data[,'s'], col = 'red')
plot(data[,'muB'], data[,'phi'], col = 'red')
plot(data[,'muB'], data[,'niB'], col = 'red')
plot(data[,'muB'], data[,'recB'], col = 'red')

plot(data[,'s'], data[,'phi'], col = 'red')
plot(data[,'s'], data[,'niB'], col = 'red')
plot(data[,'s'], data[,'recB'], col = 'red')

plot(data[,'niB'], data[,'phi'], col = 'red')
plot(data[,'phi'], data[,'recB'], col = 'red')

plot(data[,'niB'], data[,'recB'])

###########################à
cor(data)
cor.test(data[,1], data[,2])

plot(cor(data))
cor(data$V1, data$V5)
cor(data)

cor.test(data$V1, data$V2)

plot(cor(data$V1, data$V5))

     