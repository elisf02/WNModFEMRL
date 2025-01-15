#"p", "B0", "pR", "b1", "muB", "s", "phi", "niB", "recB"

data<- read.table('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/MCMC/parametri_M_2018_1.txt')
dev.off()
p <- data[,1]
hist(p)
B0 <- data[,2]
hist(B0)
pR <- data[,3]
hist(pR)
b1 <- data[,4]
hist(b1)
muB <- data[,5]
hist(muB)
s <- data[,6]
hist(s)
phi <- data[,7]
hist(phi)
niB <- data[,8]
hist(niB)
recB <- data[,9]
hist(recB)
