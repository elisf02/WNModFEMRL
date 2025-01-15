#creo vettore con i nomi dei parametri (TUTTI QUELLI STIMATI)
# 1 anno alla volta ----
if(UnAnno) {
  nomi_parametri = c("p", "B0", "pR", "b1", "muB", 
                     "s", "phi", "niB", "recB")
  
  nome_file_parametri = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/MCMC/',
                               'parametri_M_', anno, '_1.txt')
  output_mcmc = read.table(nome_file_parametri)
  burnin = nrow(output_mcmc) * 0.1
  output_mcmc = output_mcmc[-c(1:burnin),-(length(nomi_parametri)+1)]
  
  colnames(output_mcmc) = c("p", "B0", "pR", "b1", "muB", 
                            "s", "phi", "niB", "recB")
  mean_parms = apply(output_mcmc, MARGIN = 2, mean)
  
  nome_file_parametri_save = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/MCMC/',
                                    'parametri_medi_', anno, '.txt')
  write.table(t(mean_parms), nome_file_parametri_save,
              row.names = F, col.names = F)
} else
{
  nomi_parametri = c("p", "B0", "pR", "b1", "muB", 
                     "s", "phi", "niB", "recB")
  
  nome_file_parametri1 = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/MCMC/',
                               'parametri_M_2016_1.txt')
  nome_file_parametri2 = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/MCMC/',
                               'parametri_M_2017_1.txt')
  nome_file_parametri3 = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/MCMC/',
                               'parametri_M_2018_1.txt')
  
  output_mcmc1 = read.table(nome_file_parametri1)
  output_mcmc2 = read.table(nome_file_parametri2)
  output_mcmc3 = read.table(nome_file_parametri3)
  
  burnin1 = nrow(output_mcmc1) * 0.1
  burnin2 = nrow(output_mcmc2) * 0.1
  burnin3 = nrow(output_mcmc3) * 0.1
  
  output_mcmc = rbind(output_mcmc1[-c(1:burnin1),-(length(nomi_parametri)+1)],
                      output_mcmc2[-c(1:burnin2),-(length(nomi_parametri)+1)],
                      output_mcmc3[-c(1:burnin3),-(length(nomi_parametri)+1)])
  
  colnames(output_mcmc) = c("p", "B0", "pR", "b1", "muB", 
                            "s", "phi", "niB", "recB")
  mean_parms = apply(output_mcmc, MARGIN = 2, mean)
  
  nome_file_parametri_save = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/MCMC/',
                                    'parametri_MediaAllYears.txt')
  write.table(t(mean_parms), nome_file_parametri_save,
              row.names = F, col.names = F)
} 
